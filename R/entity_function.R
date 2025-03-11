# entity_functions.R


check_narrative <- function(statement, filter_keywords) {
  # Convert statement to lowercase for case-insensitive matching
  statement <- tolower(statement)

  # Check if any of the filter_keywords are in the statement
  for (keyword in filter_keywords) {
    if (grepl(keyword, statement)) {
      return(1)  # Return 1 if any keyword is found
    }
  }

  return(0)  # Return 0 if no keyword is found
}

# Extract roles from Statements
Extract_roles <- function(df, column_name, keywords) {
  # Create a new data frame with columns: Original Text, Matched Keywords, Modified Text
  df_result <- df[, column_name, drop = FALSE]

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

    # Check if the word contains initials (like "A.B.C."), i.e., letters with dots between them
    if (grepl("([A-Za-z])\\.([A-Za-z])", word)) {
      return(NULL)  # Remove the word if it has initials with dots (like "F.D.G.S.")
    }
    # Skip removing "and" (case-insensitive)
    # if (tolower(cleaned_word) == "and") {
    #   return(word)  # Keep "and"
    # }
    # If the word is "all", keep it regardless of length
    if (tolower(cleaned_word) == "all") {
      return(word)  # Keep "all" even if it's short
    }

    # If the cleaned word length is 4 or more, keep it, otherwise NULL
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
