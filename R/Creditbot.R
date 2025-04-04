# Main R script

# Load the necessary libraries
library(spacyr)
library(readxl)
library(reticulate)
library(dplyr)
library(writexl)
library(stringr)
library(testthat)
library(here)


# Creating virtual env
spacy_install()

#Download and initilize the model
spacy_download_langmodel("en_core_web_sm")  # This downloads the small English model (you can use "en_core_web_md" for a larger model)
spacy_initialize(model = "en_core_web_sm")  # Initialize the model

spacy_download_langmodel("xx_ent_wiki_sm")  # Multilanguage
spacy_initialize(model = "xx_ent_wiki_sm")  # Initialize the model

# Source the functions from the external file
source("R/entity_function.R")

# CReDit roles
CRediT_Roles <- c(
  "Conceptualization", "Data curation", "Formal analysis", "Funding acquisition",
  "Investigation", "Methodology", "Project administration", "Software",
  "Resources", "Supervision", "Validation", "Visualization",
  "Writing\\s*[-–—]\\s*original\\s*draft\\s*(?:preparation)?",
  "Writing\\s*[-–—]\\s*review\\s*(?:and|&)\\s*editing"
)

CRediT_Roles_Variations <- c(
  "Conceptualization", "Conceptualism", "Conceptualisation",
  "Data curation",
  "Formal analysis","Formal data analysis",
  "Funding acquisition","funding",
  "Investigations?", "Methodology", "Project[^\\w]+administration", "Software",
  "Resources", "Supervision", "Validation",
  "Visualizations?", "Visualisations?",
  "Review\\s*[^.,;:]*",
  "Edit\\s*[^.,;:]*",
  "Writing\\s*[^.,;:]*",
  "Original\\s*[^.,;:]*"
)


# Read the Excel file
checking <- read_excel(here("data", "Author_Contributions.xlsx"), sheet = "Sheet1")


# Step 1: Create a new column based on the filter conditions
AC_Statement <- checking %>%
  mutate(
    AC_Statement = if_else(
      !is.na(Statement_Text),
      1,  # Assign 1 if the condition is true
      0   # Assign 0 if the condition is false
    )
  )%>%
  select(Statement_Text, AC_Statement)


#Step 2:  Remove all/the authors statements or "contributed equally"
Contributions <- AC_Statement %>%
  mutate(
    # Extract sentences starting with "All authors", "The authors", "author(s)", or similar statements
    # Handle cases where the sentence either ends with a period or appears at the end of a sentence
    extracted_text = str_extract_all(Statement_Text, "(All authors|The authors|All the authors|The author(s)?)([^\\.]*?)(\\.|$)"),

    # Flatten the list of extracted text into a single string, separating with a space
    extracted_text = sapply(extracted_text, function(x) paste(x, collapse = " ")),

    # Check if "contributed equally" exists and append it to extracted_text
    extracted_text = if_else(str_detect(Statement_Text, "contributed equally"),
                             paste(extracted_text, str_extract(Statement_Text, ".*contributed equally.*?\\.")),
                             extracted_text),

    # Remove all occurrences of "All authors", "The authors", "author(s)", and "contributed equally"
    Contributions = str_replace_all(Statement_Text, "(All authors|The authors|All the authors|The author(s)?)([^\\.]*?)(\\.|$)", ""),
    Contributions = str_replace_all(Contributions, ".*contributed equally.*?\\.", "")
  ) %>%
  select(Statement_Text, extracted_text, Contributions)  # Select relevant columns

# Filter the Author contribution Statments
Author_contributions_check <- Contributions %>%
  mutate(
    # Check if Contributions is empty or not and assign 1 or 0
    Author_contribution_check = if_else(str_trim(Contributions) != "", 1, 0)
  )


# Using regex tfor Authorship
Author_contributions_check$Criteriaship <- ifelse(grepl("\\b(respnsible|accountable|responsibility|authorship|substantial, direct)\\b", Author_contributions_check$extracted_text, ignore.case = TRUE), 1, 0)

# Statements where Author Contributions are available
Author_contributions  <-
  Author_contributions_check  %>%
  filter(Author_contribution_check==1)

# Define the filter keywords
Verbs_keywords <- c("acted", "acquired", "conceived", "conducted",
                    "developed", "designed", "drafted", "established",
                    "had taken", "helped", "implemented", "initiated",
                    "looked", "participated", "planned", "prepared",
                    "proposed", "proposed", "responsible", "suggested",
                    "supervised", "was the", "wrote", "written","conceptualized","involved","contributed","led","collected","completed")



# Apply the function to the column 'text' and create a new column 'keyword_found'
Author_contributions$Narrative <- sapply(Author_contributions$Contributions, check_narrative, filter_keywords = Verbs_keywords)

# Check for Author contribution Statement where its Narrative
#Author_contributions$Narrative <- sapply(Author_contributions$Statement_Text, check_narrative, keyword_list = CRediT_Roles)

# Filter out Narrative Statements
Narrative_statements  <-
  Author_contributions  %>%
  filter(Narrative==1)

# Test data: Keywords (including complex regex for specific phrases)
keywords <- c(
  "Conceptualization", "Data curation", "Formal analysis", "Funding acquisition",
  "Investigation", "Methodology", "Project administration", "Software",
  "Resources", "Supervision", "Validation", "Visualization",
  "Writing\\s*[-–—]\\s*original\\s*draft\\s*(?:preparation)?",  # Regex for "Writing – original draft"
  "Writing\\s*[-–—]\\s*review\\s*(?:and|&)\\s*editing"  # Regex for "Writing – review and editing"
)

# Pattern for case 1: Match any of the keywords followed by punctuation, then the same keyword again.
pattern_case1 <- paste0(
  "(?i)(?:\\b(", paste(keywords, collapse = "|"), ")\\b)\\s*[:;,.]\\s*.*?\\b(", paste(keywords, collapse = "|"), ")\\b"
)

# Pattern for case 2: Match any of the keywords followed by punctuation, then parentheses with specific words like "lead", "supporting", or "equal".
pattern_case2 <- paste0(
  "(?i)(?:\\b(", paste(keywords, collapse = "|"), ")\\b)\\s*[:;,.]?\\s*\\(.*?\\b(lead|supporting|equal)\\b.*?\\)\\s*[:;,]?"
)

# Combine both patterns with an OR (|)
combined_pattern <- paste0("(", pattern_case1, ")|(", pattern_case2, ")")


# Apply the regex pattern to the 'text' column and create a new column 'match' with 1 where matched
Narrative_statements$match <- ifelse(grepl(combined_pattern, Narrative_statements$Contributions, perl = TRUE), 1, 0)

# Apply extract roles function
Roles_df <- Extract_roles(Author_contributions %>% filter(Narrative == 0), 'Contributions', CRediT_Roles_Variations)

# Extract Text column
statment_text <- Roles_df$Modified_Text

# Apply the function to extract entities
results <- lapply(statment_text, extract_entities)

# Create a new dataframe with the original text, extracted "PERSON" entities, other entities, and modified text
entities_df <- data.frame(
  # Statement_text=Roles_df$cleaned_text,
  #Statement_text=Author_contributions$Statement_Text,
  Contributions=Roles_df$Contributions,

  Roles_Matched = Roles_df$Matched_Keywords,
  #classifier = classify_df$classifier,
  text = statment_text,
  person_entity = sapply(results, function(x) paste(x$person_entities, collapse = ", ")),
  other_entity = sapply(results, function(x) paste(x$other_entities, collapse = ", ")),
  removed_initials = sapply(results, function(x) paste(x$other_entities_with_initials, collapse = ", ")),
  modified_text = sapply(results, function(x) x$modified_text),
  stringsAsFactors = FALSE
)

entities_df$Roles <- sapply(entities_df$modified_text, remove_initials_from_text)

entities_df$unique_roles <- sapply(entities_df$Roles, extract_unique_words)

# Apply the function to remove author criteria statemetns
#entities_df <- Auth_Criteria_text(entities_df, text_column = "unique_roles", length_threshold = 40)



# List of valid keywords (including regex patterns)
filter_keywords <- c("preparation", "Figures", "design", "coding", "data\\w*", "study\\w*", "tests",
              "funding", "experiment\\w*", "writing", "editing", "analyses", "Concept\\w*","Modelling",
              "harmonisation","Suggestions","analysis","project")

# Apply the function
filter_entities <- filter_roles(entities_df, "unique_roles", filter_keywords)


# Filter Credit_Taxonomy
Credit_Taxonomy_check <- filter_entities %>%
  mutate(Credit_Taxonomy = ifelse(contribution_words == "", 1, 0))

Credit_Taxonomy <- Credit_Taxonomy_check %>%
  filter(!is.na(Roles_Matched)) %>%  # Remove rows where Roles_Matched is NA
  filter(Credit_Taxonomy == 1)       # Keep only rows where Credit_Taxonomy == 1




