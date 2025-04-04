# entity_functions.R

#Clean text (removing carriages and new line)
clean_text <- function(text) {
  # Remove hyphen followed by optional spaces before newline
  cleaned_text <- gsub("-\\s*\n", "", text)

  # Replace remaining newlines (\n) with spaces
  cleaned_text <- gsub("\n+", " ", cleaned_text)

  # Replace remaining carriage returns (\r) with spaces
  cleaned_text <- gsub("\r", "", cleaned_text)

  # Remove extra spaces between words (multiple spaces replaced with one)
  cleaned_text <- gsub("\\s+", " ", cleaned_text)

  # Trim leading and trailing whitespace
  cleaned_text <- trimws(cleaned_text)
  return(cleaned_text)
}

format_keyword_vector <- function(keywords, end_boundary = FALSE) {
  # typically word boundaries are added in the beginning only to allow for different possible endings
  if (end_boundary) {
    keywords_formatted <- paste0("\\b", keywords, "\\b")
  } else {
    keywords_formatted <- paste0("\\b", keywords)
  }
  # collapse keywords into one string with OR symbol between them and convert to lowercase
  keywords_formatted <- paste(keywords_formatted, collapse = "|")

  return(keywords_formatted)
}

create_keyword_list <- function()
{
  # read regular expression dictionaries from yaml file
  # yaml_path <- system.file("extdata", "keywords_patterns.yaml", package = "ContriBOT")
  yaml_path <- here::here("config", "Keywords.yaml")
  keyword_list <- yaml::read_yaml(file.path(yaml_path))

  # add conditional formatting for some dictionaries
  keyword_list <- keyword_list |>
    purrr::map2(names(keyword_list), \(x, y) switch(y,
                                                    "Verbs_keywords" = format_keyword_vector(x, end_boundary = TRUE),
                                                    "CRediT_Roles_Variations" = format_keyword_vector(x),
                                                    "CRediT_Roles" = format_keyword_vector(x),
                                                    "all_authors" = format_keyword_vector(x),
                                                    "criteriaship_keywords" = format_keyword_vector(x),

                                                    format_keyword_vector(x, end_boundary = TRUE)
    ))

  return(keyword_list)
}

# Function to get the pattern for author-related statements
get_author_pattern <- function() {
  return("(All authors|The authors|All the authors|The author\\(s\\))([^\\.]*?)(\\.|$)")
}

# Function to get the pattern for "contributed equally"
get_contributed_equal_pattern <- function() {
  return(".*contributed equally.*?\\.")  # This will match any "contributed equally" phrase and the sentence ending
}

# Extract roles from Statements
Extract_roles <- function(df, column_name, keywords) {
  # Create a new data frame with columns: Original Text, Matched Keywords, Modified Text
  df_result <- df

  # Add the Matched_Keywords and Modified_Text columns
  df_result$Matched_Keywords <- NA
  df_result$Modified_Text <- NA

  # Loop through each row in the dataframe
  for (i in 1:nrow(df)) {
    text <- df[[column_name]][i]

    # Initialize the modified_text with the original text
    modified_text <- text

    # Initialize a vector to store matched keywords
    matched_keywords <- c()

    for (keyword in keywords) {
      # Construct the regex pattern to match the keyword and optional annotations (e.g., (lead), (supporting), etc.)
      # Allowing '&' as part of the match, and capturing roles like "Writing – review & editing"
      pattern <- paste0("\\b", keyword, "\\b(?:\\s*\\((lead|supporting|equal)\\))?")

      # Use gregexpr to find all matches of the pattern in the text
      matches <- gregexpr(pattern, text, ignore.case = TRUE)
      matched_words <- regmatches(text, matches)

      if (length(matched_words[[1]]) > 0) {
        # Collect the matched keywords for the row
        matched_keywords <- c(matched_keywords, unique(unlist(matched_words)))

        # Remove the matched keywords and their annotations from the modified text
        modified_text <- gsub(pattern, "", modified_text, perl = TRUE, ignore.case = TRUE)
      }
    }

    # Additional step: Remove any instances of "Writing – review & editing" with '&' (in case it's missed)
    # Remove extra spaces after removal and fix punctuation like semicolons
    modified_text <- gsub("\\s*;\\s*", "; ", modified_text)  # Ensure proper spacing after semicolons
    modified_text <- gsub("\\s+", " ", trimws(modified_text)) # Remove excessive whitespace

    # Assign results to the new columns
    df_result$Matched_Keywords[i] <- ifelse(length(matched_keywords) > 0, paste(matched_keywords, collapse = ", "), NA)
    df_result$Modified_Text[i] <- modified_text
  }

  return(df_result)
}

# Function to remove text inside parentheses
remove_parentheses <- function(text) {
  cleaned_text <- gsub("\\(.*?\\)", "", text)
  return(cleaned_text)
}

# Function to escape special characters
escape_special_chars <- function(string) {
  # Escape characters that have special meaning in regex (e.g. ., (, ), [, ], +, ?, etc.)
  return(gsub("([\\[\\]\\.\\|\\(\\)\\+\\*\\?\\^\\$\\\\])", "\\\\\\1", string))
}

remove_underscores <- function(text) {
  # Replace underscores with spaces unless they are next to hyphens
  text <- gsub("([^_-])_([^_-])", "\\1 \\2", text)  # Replace _ with space between non-hyphen, non-underscore characters

  # Remove underscores that are adjacent to hyphens but keep the hyphen
  text <- gsub("_-", "-", text)  # Remove underscore before hyphen
  text <- gsub("-_", "-", text)  # Remove underscore after hyphen

  return(text)
}

is_initials <- function(entity) {
  # Match initials (e.g., "J.D." or "BM-S") followed optionally by a full name (e.g., "Smith")
  return(grepl("^[A-Za-z]+(-[A-Za-z]+)*\\.([A-Za-z]\\.)*[A-Za-z]*$", entity) || nchar(entity) < 4)
}


# Function to classify based on the count of comma-separated matched keywords
classify <- function(keywords_string) {
  # Split the comma-separated string into a vector of keywords
  keywords <- unlist(strsplit(keywords_string, ","))

  # Remove any leading/trailing whitespace from the keywords
  keywords <- trimws(keywords)

  # Count how many keywords are present
  keyword_count <- length(keywords)

  # Return 1 for Concise (3 or more keywords), 0 for Narrative (less than 3 keywords)
  if (keyword_count >= 3) {
    return(1)  # Concise
  } else {
    return(0)  # Narrative
  }
}

extract_entities <- function(text) {
  # Parse the text with spaCy
  parsed_text <- spacy_parse(text)

  # Extract entities
  entities <- entity_extract(parsed_text)

  # Filter for "PERSON" entities
  person_entities <- entities[entities$entity_type == "PERSON", "entity"]

  # Check if there are non-"PERSON" entities
  other_entities <- entities[entities$entity_type != "PERSON", "entity"]

  # If no non-"PERSON" entities exist, skip processing them
  other_entity_list <- if (length(other_entities) > 0) {
    as.character(unlist(other_entities))  # Convert to character vector if not empty
  } else {
    character(0)  # Return empty character vector if no "other" entities
  }

  # Convert "PERSON" entities to character vectors
  person_entity_list <- as.character(person_entities)

  # Escape special regex characters in entity names (if needed)
  person_entity_list <- sapply(person_entity_list, remove_underscores)
  other_entity_list <- sapply(other_entity_list, remove_underscores)

  # Use `is_initials` function to filter out entities that contain initials from `other_entity_list`
  other_entity_list_with_initials <- if (length(other_entity_list) > 0) {
    other_entity_list[sapply(other_entity_list, is_initials)]
  } else {
    character(0)  # Return empty vector if no "other" entities exist
  }

  # Create a single regex pattern to match any of the "PERSON" entities
  person_pattern <- paste(person_entity_list, collapse = "|")

  # Create a regex pattern for entities with initials (from `other_entity_list_with_initials`)
  other_pattern <- if (length(other_entity_list_with_initials) > 0) {
    paste(other_entity_list_with_initials, collapse = "|")
  } else {
    ""  # No "other" entities with initials
  }

  # Combine both patterns into a single pattern
  combined_pattern <- paste(person_pattern, other_pattern, sep = "|")

  # Use gsub with the combined pattern to remove both "PERSON" entities and "other entities with initials"
  modified_text <- gsub(combined_pattern, "", text)

  # Remove any extra spaces left after removing the entities
  modified_text <- gsub("\\s+", " ", modified_text)

  # Return a list with "PERSON" entities, non-"PERSON" entities, and the modified text
  return(list(person_entities = person_entity_list,
              other_entities = other_entity_list,
              other_entities_with_initials = other_entity_list_with_initials,
              original_text = text,
              modified_text = modified_text))
}

remove_initials_from_text <- function(text) {

  # Split the text into words
  words <- unlist(strsplit(text, "\\s+"))  # Split text by spaces

  # Remove punctuation from each word and filter based on length
  cleaned_words <- sapply(words, function(word) {
    # Remove punctuation by keeping only alphanumeric characters
    cleaned_word <- gsub("[^A-Za-z0-9]", "", word)  # Keep only letters and numbers

    # Skip the word if it is NA
    if (is.na(cleaned_word)) {
      return(NULL)
    }

    # Check if the word contains initials (like "A.B.C."), i.e., letters with dots between them
    if (grepl("([A-Za-z])\\.([A-Za-z])", word)) {
      return(NULL)  # Remove the word if it has initials with dots (like "F.D.G.S.")
    }

    # If the word is "all", keep it regardless of length
    if (tolower(cleaned_word) == "all") {
      return(word)  # Keep "all" even if it's short
    }

    # Check for NULL or NA in cleaned_word before using nchar
    if (nchar(cleaned_word) >= 4) {
      return(word)  # Keep the original word (with punctuation)
    } else {
      return(NULL)  # Remove the word if its length is too short after cleaning
    }
  })

  # Remove NULL values (words with less than 5 characters after cleaning or initials)
  cleaned_text <- paste(cleaned_words[!sapply(cleaned_words, is.null)], collapse = " ")

  return(cleaned_text)
}


remove_rolestype <- function(input_text) {
  # Regular expression to match the words "equal", "supporting", or "lead" inside parentheses
  cleaned_text <- gsub("\\((equal|supporting|lead)\\)", "", input_text)
  return(cleaned_text)
}

# Function to extract unique words and keep the same notation (comma, colon, semicolon)
extract_unique_words <- function(text) {
  # Split the text by commas, colons, or semicolons
  words <- unlist(strsplit(text, "[,;:]"))

  # Clean the words by trimming any leading or trailing spaces
  cleaned_words <- trimws(words)

  # Remove any empty strings
  cleaned_words <- cleaned_words[cleaned_words != ""]

  # Get the unique words
  unique_words <- unique(cleaned_words)

  # Recombine the unique words into a single string with commas separating them
  unique_words_string <- paste(unique_words, collapse = ", ")

  return(unique_words_string)
}


# Function to filter words and create an extra column showing matched words
filter_roles <- function(df, column_name, keywords) {
  # Convert the column to character (if it's not already)
  df[[column_name]] <- as.character(df[[column_name]])

  # Function to find matched words using regex patterns
  find_matched_words <- function(entry) {
    # Ensure entry is a string (sometimes NA values might cause problems)
    if (is.na(entry)) return(NA)

    # Split the entry by commas and trim leading/trailing whitespaces
    words <- unlist(strsplit(entry, ",\\s*"))

    # Initialize a vector to store matched words
    contribution_words <- character(0)

    # Loop through each word and check for a match
    for (word in words) {
      if (any(sapply(keywords, function(k) grepl(k, word, ignore.case = TRUE)))) {
        contribution_words <- c(contribution_words, word)
      }
    }

    # Return the matched words as a comma-separated string
    return(paste(contribution_words, collapse = ", "))
  }

  # Apply the function to each row of the specified column
  df$contribution_words <- sapply(df[[column_name]], find_matched_words)

  # Return the modified dataframe
  return(df)
}
# Function to process each narrative row
process_narrative <- function(narrative) {
  # Preprocess the text: lowercase, remove punctuation, and numbers
  narrative_clean <- tolower(narrative)
  narrative_clean <- removePunctuation(narrative_clean)
  narrative_clean <- removeNumbers(narrative_clean)

  # Split the cleaned text into words
  words <- str_split(narrative_clean, " ", simplify = TRUE)

  # Stem the words
  narrative_stemmed <- wordStem(words)

  # Combine the stemmed words back into a sentence
  narrative_stemmed_text <- paste(narrative_stemmed, collapse = " ")

  # Check for the presence of CRediT roles
  roles_found <- sapply(CRediT_Roles_stemmed, function(role) {
    grepl(role, narrative_stemmed_text, ignore.case = TRUE)
  })

  # Extract the detected roles
  detected_roles <- Roles[roles_found]

  return(paste(detected_roles, collapse = ", "))  # Join detected roles as a comma-separated string
}


performance_matrix <- function(df1, col1, df2, col2) {
  # Ensure both data frames have the same number of rows to avoid index out of range errors
  n_rows <- min(nrow(df1), nrow(df2))  # Limit the comparison to the smaller number of rows

  # Calculate exact matches excluding NAs
  exact_matches <- sum(df1[[col1]][1:n_rows] == df2[[col2]][1:n_rows], na.rm = TRUE)

  # Count where both columns have NA
  na_matches <- sum(is.na(df1[[col1]][1:n_rows]) & is.na(df2[[col2]][1:n_rows]))

  # Count where one column is NA and the other is not
  na_mismatches <- sum((is.na(df1[[col1]][1:n_rows]) & !is.na(df2[[col2]][1:n_rows])) |
                         (!is.na(df1[[col1]][1:n_rows]) & is.na(df2[[col2]][1:n_rows])))

  # Calculate total mismatches excluding NAs
  mismatches <- sum(df1[[col1]][1:n_rows] != df2[[col2]][1:n_rows], na.rm = TRUE)

  # Create a logical vector for mismatches, excluding NAs
  mismatch_logical <- !is.na(df1[[col1]][1:n_rows]) & !is.na(df2[[col2]][1:n_rows]) & (df1[[col1]][1:n_rows] != df2[[col2]][1:n_rows])

  # Create a data frame of mismatched rows, with indices and values
  mismatched_rows <- data.frame(
    Index = which(mismatch_logical) ,  # Adjust index to account for Excel's row 2 starting (R starts at 1)
    Expected = df2[[col2]][mismatch_logical],  # values from df2 (expected)
    Actual = df1[[col1]][mismatch_logical]  # values from df1 (actual)
  )

  # Return a list of results
  result <- list(
    exact_matches = exact_matches,
    na_matches = na_matches,
    na_mismatches = na_mismatches,
    mismatches = mismatches,
    mismatched_rows = mismatched_rows  # Include the mismatched rows data frame
  )

  return(result)
}

# Function to capture text until the first period or comma
extract_roles_narrative <- function(text) {
  # Parse the text and get part-of-speech tagging
  parsed_text <- spacy_parse(text)

  # Initialize a vector to store the segments
  segments <- character()

  # Loop through the parsed text to identify nouns and extract text after them
  for (i in 1:(nrow(parsed_text) - 1)) {
    # Check if the current word is a noun (NOUN)
    if (parsed_text$pos[i] == "VERB") {
      # Capture everything after the noun up to the period or comma
      sentence_end <- which(parsed_text$token == "." | parsed_text$token == ".")[which(parsed_text$token == "." | parsed_text$token == ".") > i][1]

      # Extract the portion of text after the noun and before the period or comma
      if (!is.na(sentence_end)) {
        segment <- paste(parsed_text$token[i:sentence_end], collapse = " ")

        # Replace period or comma at the end with a comma
        segment <- gsub("[.,]$", ",", segment)

        segments <- c(segments, segment)
      }
    }
  }

  # Return the joined result with commas separating the segments
  return(paste(segments, collapse = ""))
}


# Function to check if any required word is in the text (case-insensitive using str_detect)
check_matching_words <- function(text, required_words) {
  # Initialize vectors to store matched and non-matched words
  matched_words <- character(0)
  non_matched_words <- character(0)

  # Convert the text to lowercase for case-insensitive comparison
  text_lower <- tolower(text)

  # Check which required words are present in the text using str_detect
  for (word in required_words) {
    if (str_detect(text_lower, fixed(tolower(word)))) {
      matched_words <- c(matched_words, word)
    } else {
      non_matched_words <- c(non_matched_words, word)
    }
  }

  # If any words are matched, return 1 for match status and the matched words
  match_status <- if (length(matched_words) > 0) 1 else 0

  # Return the match status, matched words, and non-matched words
  return(c(match_status, paste(matched_words, collapse = ", "), paste(non_matched_words, collapse = ", ")))
}

find_roles_and_remaining <- function(text) {
  # Remove leading/trailing spaces and replace multiple spaces with a single space
  cleaned_text <- gsub("\\s+", " ", trimws(text))

  # Split the cleaned text into individual phrases based on commas
  phrases <- unlist(strsplit(cleaned_text, ",\\s*"))

  # Check each phrase against the pattern and get the matched phrases
  matched_phrases <- phrases[grepl(pattern, phrases, ignore.case = TRUE)]

  # Get the unmatched phrases by excluding the matched ones
  unmatched_phrases <- phrases[!grepl(pattern, phrases, ignore.case = TRUE)]

  # Return matched and unmatched as strings, joined by commas, or NA if none found
  matched_text <- if (length(matched_phrases) > 0) paste(matched_phrases, collapse = ", ") else NA
  unmatched_text <- if (length(unmatched_phrases) > 0) paste(unmatched_phrases, collapse = ", ") else NA

  return(c(matched_text, unmatched_text))
}
