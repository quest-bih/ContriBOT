#' Seach for CRediT keywords and extract section.
#'
#' The algorithm searchers for a  Authorship, Acknowledgement, and ORCID statements
#' and extracts them. If no known section title was found, the algorithm searches
#' for several categories of similar keywords in each sentence of the whole manuscript.
#'
#' @param pdf_text_sentences Document corpus loaded with the pdf_load function.
#' @importFrom rlang .data
#' 
#' @return Tibble with one row per screened document and the file name and logical values for authorship,
#' acknowledgement, and orcid statements detected as columns,
#' plus additional columns that contain the statements that were extracted.
#'
#' @examples
#' \dontrun{
#' extract_contributions(pdf_load("examples/"))
#' }
#'
#' @export
extract_contributions <- function(pdf_text_sentences)
{
  keyword_list <- .create_keyword_list()

  # pdf_text_sentences <- pdf_text_corpus

  # pdf_text_sentences <- list(pdf_text_sentences)
  # tib <- tibble(text = pdf_text_sentences[[1]])
  message("Extracting Contributions...")
  contrib_text_sentences <- pdf_text_sentences |> 
    .extract_section_progress(
      keyword_list$autorship_section,
      look_in_tables = TRUE)
  
  message("Extracting Acknowledgements...")
  ackn_text_sentences <- pdf_text_sentences |>
    .extract_section_progress(
      keyword_list$ackn_section,
      look_in_tables = TRUE)

  message("Extracting ORCIDs...")
  orcid_text_sentences <- pdf_text_sentences |>
    .extract_section_progress(
      keyword_list$orcid_section,
      look_in_tables = TRUE)

  ackn_results <- ackn_text_sentences |>
    .enframe_results(name = "article", value = "ackn_statement")

  orcid_results <- orcid_text_sentences |>
    .enframe_results(name = "article", value = "orcid_statement")

  contrib_text_sentences |>
    .enframe_results(name = "article", value = "contrib_statement") |>
    dplyr::left_join(ackn_results, by = "article") |>
    dplyr::left_join(orcid_results, by = "article") |>
    dplyr::mutate(has_contrib = .data$contrib_statement != "",
                  has_ackn = .data$ackn_statement != "",
                  has_orcid = .data$orcid_statement != "")

}


#' convert results from list to tibble
#' @noRd
.enframe_results <- function(ls, name, value) {
  ls |>
    purrr::map_chr(\(x) paste(x, collapse = " ")) |>
    tibble::enframe(name = name, value = value)
}
# tib <- tibble(text = pdf_text_sentences)
# section_regexes <- credit_section_list
# section_regexes <- orcid_section_list

#' extract section
#' @noRd
.extract_section <- function(pdf_text_sentences, section_regexes, look_in_tables = FALSE) {

  # TODO: validate that text extraction stops at the right spots for ackn and credit
  keyword_list <- .create_keyword_list()

  section_string <- paste0("(<section>|#+)[^\\w+][\\d,^\\w]*(", section_regexes, ")\\b")
  if (look_in_tables == TRUE) {
    section_string <- paste0(section_string, "|", keyword_list$table_credit) 
  }
  # stringr::str_detect(PDF_text_sentence, data_availability)
  section_detections <- 
    furrr::future_map_lgl(pdf_text_sentences,
                          \(sentence) stringr::str_detect(
                            sentence,
                            stringr::regex(section_string, ignore_case = TRUE)))
# str_detect("<section> contributors all authors were involved in the discussion and formulation of the points to consider.", section_string)
  # if (sum(section_detections) > 0) {
  #   DAS_detections <- furrr::future_map_lgl(pdf_text_sentences,
  #                                           \(sentence) .has_DAS(sentence, keyword_list))
  # }

  section_start <- which(section_detections)

  # if more than one detections of a section were made, then return empty string
  if (length(section_start) >= 2) {
    if (diff(section_start)[1] < 10) {
      section_start <- min(section_start)
    } else {
      section_start <- max(section_start)
    }
  } else if (length(section_start) != 1) {
    return("")
  }

  str_section <- pdf_text_sentences[section_start] |>
    stringr::str_trim()
  str_section_sameline <- str_section |>
    stringr::str_remove(stringr::regex(section_string, ignore_case = TRUE))

  # section_regexes

  stop_regex <- keyword_list$stop_sections
  
  stop_regex <- stop_regex[!stringr::str_detect(stop_regex,
                                                section_regexes)] |>
    paste(collapse = "|")

  # candidates are sentences after the first section but before the next
  # which begin with <section> or digit. (reference number at start of line)
  is_plos <- any(stringr::str_detect(pdf_text_sentences[1:10], "^plos ")) #explosive?

  if (is_plos == TRUE) {

    section_end <- 
      furrr::future_map_lgl(
        pdf_text_sentences[(section_start + 1):length(pdf_text_sentences)],
        \(sentence) stringr::str_detect(sentence,
                                        stringr::regex("section> references",
                                                       ignore_case = TRUE))) |>
      which() - 1

  } else {

      section_end_candidates <-
        furrr::future_map_lgl(
          pdf_text_sentences[(section_start + 1):length(pdf_text_sentences)],
          \(sentence) stringr::str_detect(sentence,
                                          stringr::regex(stop_regex,
                                                         ignore_case = TRUE))) |>
        which() - 1

      section_end <- section_end_candidates[1]

  }

  if (is.na(section_end)) section_end <- 0
  section_end <- section_start + section_end

  section <- pdf_text_sentences[section_start:section_end]

  if (section_start < 50 & any(stringr::str_detect(pdf_text_sentences[1:10],
                                                   stringr::regex("plos",
                                                                  ignore_case = TRUE)))) {
    section <- oddpub::splice_plos_twopager(section)
  }
  section |>
    stringr::str_remove_all("\\u200b") |> # remove zerowidth spaces
    stringr::str_trim()
}

#' extract contribution section with progress bar
#' @noRd
.extract_section_progress <- function(pdf_text_sentences,
                                      keywords,
                                      look_in_tables) {
  p <- progressr::progressor(along = pdf_text_sentences)
  pdf_text_sentences |>
    furrr::future_map(\(x) {
      p()
      .extract_section(x,
                       keywords,
                       look_in_tables = look_in_tables)
      
      })
}
