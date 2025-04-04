# Main R script

# List of libraries to load
packages <- c("spacyr",
              "reticulate",
              "readxl",
              "yaml",
              "dplyr",
              "writexl",
              "stringr",
              "SnowballC",
              "here",
              "tm",
              "knitr",
              "caret",
              "testthat")
# Load all libraries
lapply(packages, library, character.only = TRUE)


# # Creating virtual env
# spacy_install()
#
# #Download and initilize the model
# spacy_download_langmodel("en_core_web_sm")  # This downloads the small English model (you can use "en_core_web_md" for a larger model)
# spacy_initialize(model = "en_core_web_sm")  # Initialize the model
#
# spacy_download_langmodel("xx_ent_wiki_sm")  # Multilanguage
# spacy_initialize(model = "xx_ent_wiki_sm")  # Initialize the model

# Source the functions from the external file
source(here::here("R", "entity_function.R"))

# Read the training data file
training_data <- read_excel(here::here("Data", "Author_Contributions.xlsx"), sheet = "Sheet1")

#Load yaml file
keywords <- yaml::read_yaml(here::here("config", "Keywords.yaml"))

#Access for the function
keyword_list <- create_keyword_list()

# Apply the cleaning function
training_data <- training_data |>
  mutate(Statement_Text = clean_text(Statement_Text))

# Step 1: Check if Statement text is present
AC_Statement <- training_data |>
  mutate(
    AC_Statement = if_else(
      !is.na(Statement_Text),
      1,  # Assign 1 if the condition is true
      0   # Assign 0 if the condition is false
    )
  ) |>
  select(Statement_Text, AC_Statement)


# Step 2: Remove author statements or "contributed equally" as they are not part of roles
Author_contributions_check <- AC_Statement %>%
  mutate(
    # Use the functions to extract the patterns
    pattern_all_authors = get_author_pattern(),
    pattern_contributed_equally = get_contributed_equal_pattern(),

    # Extract sentences starting with author statements
    extracted_text = str_extract_all(Statement_Text, pattern_all_authors),

    # Flatten the list of extracted text into a single string, separating with a space
    extracted_text = sapply(extracted_text, function(x) paste(x, collapse = " ")),

    # Check if "contributed equally" exists and append it to extracted_text
    extracted_text = if_else(str_detect(Statement_Text, "contributed equally"),
                             paste(extracted_text, str_extract(Statement_Text, pattern_contributed_equally)),
                             extracted_text),

    # Remove all occurrences of "All authors", "The authors", "author(s)", and "contributed equally"
    A_Contributions = str_replace_all(Statement_Text, pattern_all_authors, ""),
    A_Contributions = str_replace_all(A_Contributions, pattern_contributed_equally, "")
  ) %>%
  select(Statement_Text, extracted_text, A_Contributions)  # Select relevant columns


# Statements text that contain Author contributions
Contributions <- Author_contributions_check |>
  mutate(
    # Check if Contributions is empty or not and assign 1 or 0
    Contributions = if_else(str_trim(A_Contributions) != "", 1, 0)
  )


# Classifier to check author criteriship
criteriaship_pattern <- keyword_list$criteriaship_keywords
Contributions$Criteriaship <- ifelse(
  !is.na(Contributions$extracted_text) & grepl(keyword_list$criteriaship_keywords, Contributions$extracted_text, ignore.case = TRUE),
  1,
  ifelse(is.na(Contributions$extracted_text), NA, 0)
)

# Give row_id
Contributions <- Contributions |>
  mutate(row_id = row_number()) |>
  select(row_id, everything())

# Check if statement text is Narrative Statements
Verbs_keywords <- keyword_list$Verbs_keywords
# Set 'Narrative' to NA if 'Contributions$Contributions' is 0

# Check if statement text is Narrative Statements
Contributions$Narrative <- ifelse(Contributions$Contributions == 0, NA, NA)
Contributions$Narrative <- ifelse(
  is.na(Contributions$Narrative),  # If it's NA (not set in step 1)
  ifelse(
    !is.na(Contributions$A_Contributions) & grepl(keyword_list$Verbs_keywords, Contributions$A_Contributions, ignore.case = TRUE),
    1,  # Set to 1 if a keyword is found
    ifelse(is.na(Contributions$A_Contributions), NA, 0)  # Set to 0 if no keyword match
  ),
  Contributions$Narrative  # Otherwise, keep the already assigned value (empty string or NA)
)


# Apply extract roles function
CRediT_Roles_Variations <- keywords$CRediT_Roles_Variations
Roles_df <- Extract_roles(Contributions %>% filter(Narrative == 0), 'A_Contributions', CRediT_Roles_Variations)


# Apply the function to extract entities
statment_text <- Roles_df$Modified_Text
results <- lapply(statment_text, extract_entities)

# Create a new dataframe with the original text, extracted "PERSON" entities, other entities, and modified text
entities_df <- data.frame(
  row_id=Roles_df$row_id,
  Statement_Text=Roles_df$Statement_Text,
  extracted_text=Roles_df$extracted_text,
  A_Contributions=Roles_df$A_Contributions,
  Contributions=Roles_df$Contributions,
  Criteriaship=Roles_df$Criteriaship,
  Narrative=Roles_df$Narrative,
  Roles_Matched = Roles_df$Matched_Keywords,
  text = statment_text,
  person_entity = sapply(results, function(x) paste(x$person_entities, collapse = ", ")),
  other_entity = sapply(results, function(x) paste(x$other_entities, collapse = ", ")),
  removed_initials = sapply(results, function(x) paste(x$other_entities_with_initials, collapse = ", ")),
  modified_text = sapply(results, function(x) x$modified_text),
  stringsAsFactors = FALSE
)

# Remove initials
entities_df$Roles <- sapply(entities_df$modified_text, remove_initials_from_text)

#Get unnique roles
entities_df$unique_roles <- sapply(entities_df$Roles, extract_unique_words)

# Filter the names to get unique roles
filter_keywords <- keyword_list$filter_keywords
entities_df <- filter_roles(entities_df, "unique_roles", filter_keywords)


# Filter Credit_Taxonomy
Credit_Taxonomy_check <- entities_df |>
  mutate(Credit_Taxonomy = ifelse(contribution_words == "", 1, 0))

Credit_Taxonomy <- Credit_Taxonomy_check |>
  filter(!is.na(Roles_Matched)) |> # Remove rows where Roles_Matched is NA
  filter(Credit_Taxonomy == 1)       # Keep only rows where Credit_Taxonomy == 1

# Filter out Narrative Statements
Narrative_statements  <-
  Contributions  |>
  filter(Narrative==1)


# Apply the function to create a new 'processed_text' column
Narrative_keywords <- keywords$Narrative

Narrative_statements <- Narrative_statements %>%
  mutate(processed_text = sapply(A_Contributions, extract_roles_narrative))



# Create a pattern from these variations, joining them with an OR (|) operator
Narrative_keywords <- keywords$Narrative
pattern <- paste(Narrative_keywords, collapse = "|")

# Apply the function to your data to get matched and unmatched phrases
result <- t(sapply(Narrative_statements$processed_text, find_roles_and_remaining))

# Combine the results into the original data frame
Narrative_statements$matched_phrases <- result[, 1]
Narrative_statements$unmatched_phrases <- result[, 2]
Narrative_statements$Credit_Taxonomy <- ifelse(!is.na(Narrative_statements$matched_phrases) & is.na(Narrative_statements$unmatched_phrases), 1,0)


Na_statements <- Contributions |>
  filter(is.na(Narrative) | Narrative == "") |>
  mutate(Credit_Taxonomy = NA)



final_result <- bind_rows(Credit_Taxonomy_check, Narrative_statements,Na_statements) %>%
  arrange(row_id) %>%
  select(-processed_text, -matched_phrases, -unmatched_phrases, -Roles_Matched,
         -text, -person_entity, -other_entity, -removed_initials,
         -modified_text, -Roles, -unique_roles, -contribution_words)
write_xlsx(final_result, path = here("outputs", "result.xlsx"))



