library(tidyverse)
library(tidytext)
library(readxl)
library(here)
library(yardstick)

#' Search for several categories of similar keywords in a sentence.
#' Multiple categories must match to trigger a detection.
#' @noRd
.create_keyword_list <- function()
{
  # read regular expression dictionaries from yaml file
  # yaml_path <- system.file("extdata", "keywords_patterns.yaml", package = "ContriBOT")
  yaml_path <- here::here("inst", "extdata", "keywords_patterns.yaml")
  keyword_list <- yaml::read_yaml(file.path(yaml_path))
  
  # add conditional formatting for some dictionaries
  keyword_list <- keyword_list |>
    purrr::map2(names(keyword_list), \(x, y) switch(y,
                                                    "credit_roles" = .format_keyword_vector(x, end_boundary = TRUE),
                                                    "non_credit_roles" = .format_keyword_vector(x),
                                                    "equally" = .format_keyword_vector(x),
                                                    "n3_credit_roles"=.format_keyword_vector(x),
                                                    
                                                    
                                                    .format_keyword_vector(x, end_boundary = TRUE)
    ))
  
  return(keyword_list)
}

#' standardize the format of different keyword vectors
#' @noRd
.format_keyword_vector <- function(keywords, end_boundary = FALSE) {
  # typically word boundaries are added in the beginning only to allow for different possible endings
  if (end_boundary) {
    keywords_formatted <- paste0("\\b", keywords, "\\b")
  } else {
    keywords_formatted <- paste0("\\b", keywords)
  }
  # collapse keywords into one string with OR symbol between them and convert to lowercase
  keywords_formatted <- paste(keywords_formatted, collapse = "|") |> tolower()
  
  return(keywords_formatted)
}

#' clean text from hyphenation, (non-)breaking spaces, line returns, multiple spaces
#' @noRd
.clean_sentences <- function(input_str) {
  cleaned_sentences <- stringr::str_replace_all(input_str, "\r\n", " ") |>
    stringr::str_remove_all(" (-|‐)|\\u25a1|\\u200b|(?<=\\w)(-|‐)( |\n)") |>
    stringr::str_replace_all("fnal", "final") |>
    stringr::str_replace_all("frst", "first") |>
    stringr::str_replace_all("fgure", "figure") |>
    stringr::str_replace_all("drat", "draft") |>
    stringr::str_replace_all("sotware", "software") |>
    stringr::str_squish()
  return(cleaned_sentences)
}

#' unnest sentences in tibble
#' @noRd
unnest_statements <- function(input_tib, input_col) {
  unnested_tib <- input_tib |>
    tidytext::unnest_tokens(output = sentence, input = {{ input_col }},
                            token = "regex", pattern = "(?<!\\b[A-Za-z]?\\w)\\. ") |>
    tidytext::unnest_tokens(output = sentence, input = sentence, token = "regex",
                            pattern = "(\\.|;|:|,) (?=all)")
  
  return(unnested_tib)
}

gold_set <- read_excel(here("data", "Author_Contributions.xlsx"), sheet = "Sheet1")

keyword_list <- .create_keyword_list()

gold_set_unnested <- gold_set |>
  mutate(cleaned_statement = .clean_sentences(Statement_Text)) |>
  unnest_statements(cleaned_statement) |>
  mutate(is_all_authors = str_detect(sentence, keyword_list$all_authors),
         has_credit_role = str_detect(sentence, keyword_list$credit_roles),
         n_credit = str_count(sentence, keyword_list$credit_roles),
         n3_credit=str_count(sentence, keyword_list$n3_credit_roles),
         has_noncredit_role = str_detect(sentence, keyword_list$non_credit_roles) &!is_all_authors,
         is_equally = str_detect(sentence, keyword_list$equally),
         has_narrative=str_detect(sentence,keyword_list$narrative),
         is_narrative = str_detect(sentence, keyword_list$narrative) & !is_all_authors & !is_equally,
         is_responsibility = str_detect(sentence, keyword_list$responsibility),
         is_contrib = (n_credit > 0 | has_noncredit_role | is_narrative) & !is_all_authors & !is_equally)




qa_performance <- gold_set_unnested |>
  group_by(doi) |>
  summarise(across(AC_Statement:Comment, dplyr::first),
            has_credit = any(has_credit_role, na.rm = TRUE),
            has_non_credit = any(has_noncredit_role, na.rm = TRUE),
            is_narrative = any(is_narrative, na.rm = TRUE),
            n_credit = sum(n_credit, na.rm = TRUE),
            n3_credit=sum(n3_credit,na.rm = TRUE),
            is_responsibility=any(is_responsibility,na.rm = TRUE),
            cleaned_sentences = paste(sentence, collapse = ". "),
            credit_estimate = n_credit > 3 & !has_non_credit,
            contrib_estimate = any(is_contrib, na.rm = TRUE)
  ) |>
  #mutate(credit_estimate = factor(credit_estimate, levels = c(TRUE, FALSE)),
         
         mutate(
           credit_estimate = factor(
             case_when(
               n_credit > 3 & !has_non_credit ~ TRUE,  
               n_credit <= 3 & !has_non_credit & n3_credit > 2 ~ TRUE,  
               TRUE ~ FALSE  
             ), levels = c(TRUE, FALSE)
           ),
         credit_truth = factor(as.logical(CRT_Taxonomy, na.rm = TRUE), levels = c(TRUE, FALSE)),
         contrib_estimate = factor(contrib_estimate, levels = c(TRUE, FALSE)),
         contrib_truth = factor(as.logical(Contributions, na.rm = TRUE), levels = c(TRUE, FALSE)),
         narrative_estimate = factor(is_narrative, levels = c(TRUE, FALSE)),
         narrative_truth = factor(as.logical(Narrative, na.rm = TRUE), levels = c(TRUE, FALSE)),
         Authorship_estimate = factor(is_responsibility, levels = c(TRUE, FALSE)),
         Authorship_truth = factor(as.logical(Auth_criteria, na.rm = TRUE), levels = c(TRUE, FALSE)))



multi_metric <- metric_set(accuracy, ppv, sensitivity, specificity, npv, f_meas)
multi_metric(qa_performance,
             truth = credit_truth, estimate = credit_estimate)

qa_performance |>
  conf_mat(truth = credit_truth, estimate = credit_estimate) |>
  autoplot(type = "heatmap")

multi_metric(qa_performance,
             truth = narrative_truth, estimate = narrative_estimate)

qa_performance |>
  conf_mat(truth = narrative_truth, estimate = narrative_estimate) |>
  autoplot(type = "heatmap")

multi_metric(qa_performance,
             truth = contrib_truth, estimate = contrib_estimate)

qa_performance |>
  conf_mat(truth = contrib_truth, estimate = contrib_estimate) |>
  autoplot(type = "heatmap")

multi_metric(qa_performance,
             truth = Authorship_truth, estimate = Authorship_estimate)

qa_performance |>
  conf_mat(truth = Authorship_truth, estimate = Authorship_estimate) |>
  autoplot(type = "heatmap")



qa_performance |>
  filter(credit_truth == FALSE, credit_estimate == TRUE) |>
  pull(doi)

qa_narrative_credit <- gold_set_unnested |>
  group_by(doi) |>
  mutate(has_noncredit = any(has_noncredit_role == TRUE, na.rm = TRUE)) |>
  filter(has_credit_role, !has_noncredit, !is_all_authors,
         is_narrative, CRT_Taxonomy == 0, n_credit > 3)

qa_credit <- gold_set_unnested |>
  group_by(doi) |>
  mutate(has_noncredit = any(has_noncredit_role == TRUE, na.rm = TRUE)) |>
  filter(has_credit_role, !has_noncredit, !is_all_authors,
         !is_narrative, CRT_Taxonomy == 0)

gold_set_words <- gold_set_unnested |>
  group_by(doi, sentence) |>
  unnest_tokens(output = word, input = sentence, drop = FALSE) |>
  anti_join(stop_words, by = "word") |>
  filter(str_length(word) > 4 | str_detect(word, keyword_list$four_letters), !str_detect(word, "\\.")) |>
  mutate(length = str_length(word)) |>
  arrange(length, word)

word_counts <- gold_set_words |>
  ungroup() |>
  count(word, sort = TRUE)
