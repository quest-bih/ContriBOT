#' Search for several categories of similar keywords in a sentence.
#' Multiple categories must match to trigger a detection.
#' @noRd
.create_keyword_list <- function()
{
  # read regular expression dictionaries from yaml file
  yaml_path <- system.file("extdata", "keywords_patterns.yaml", package = "ContriBOT")
  # yaml_path <- here::here("inst", "extdata", "keywords_patterns.yaml")
  keyword_list <- yaml::read_yaml(file.path(yaml_path))

  # add conditional formatting for some dictionaries
  keyword_list <- keyword_list |>
    purrr::map2(
      names(keyword_list),
      \(x, y) switch(y,
                     "credit_roles" = .format_keyword_vector(x, end_boundary = TRUE),
                     "stop_sections" = x,
                     .format_keyword_vector(x)
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
    stringr::str_remove_all(" (-|\\u2010)|\\u25a1|\\u200b|(?<=\\w)(-|\\u2010)( |\n)") |>
    stringr::str_replace_all("fnal", "final") |>
    stringr::str_replace_all("frst", "first") |>
    stringr::str_replace_all("fgure", "figure") |>
    stringr::str_replace_all("drat", "draft") |>
    stringr::str_replace_all("sotware", "software") |>
    stringr::str_squish()
  return(cleaned_sentences)
}

#' unnest sentences in tibble
#' @importFrom rlang .data
#' @noRd
unnest_statements <- function(input_tib, input_col) {
  unnested_tib <- input_tib |>
    tidytext::unnest_tokens(output = .data$sentence,
                            input = {{ input_col }},
                            token = "regex",
                            pattern = "(?<!\\b[A-Za-z]?\\w)\\. ") |>
    tidytext::unnest_tokens(output = .data$sentence,
                            input = .data$sentence,
                            token = "regex",
                            pattern = "(\\.|;|:) (?=(all|the))")

  return(unnested_tib)
}