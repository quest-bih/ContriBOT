#' Search for Authorship, Acknowledgement, and ORCID keywords and extract corresponding sections or statements.
#'
#' The algorithm searchers for a  Authorship, Acknowledgement, and ORCID statements
#' and extracts them.
#'
#' @param text_sentences Document corpus loaded with the `oddpub::pdf_load` function.
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
extract_contributions <- function(text_sentences)
{
  keyword_list <- .create_keyword_list()

  message("Extracting Contributions...")
  contrib_text_sentences <- text_sentences |>
    .extract_section_progress(
      keyword_list$authorship_section,
      look_in_tables = TRUE)

  message("Extracting Acknowledgements...")
  ackn_text_sentences <- text_sentences |>
    .extract_section_progress(
      keyword_list$ackn_section,
      look_in_tables = FALSE)

  # message("Extracting ORCID sections...")
  # orcid_text_sentences <- text_sentences |>
  #   .extract_section_progress(
  #     keyword_list$orcid_section,
  #     look_in_tables = FALSE)
  message("Extracting any ORCIDs written out as text...")
  orcids_anywhere <- .extract_all_orcids_progress(text_sentences, keyword_list$orcid_link)
# tt <- tib |> mutate(orcids = str_extract_all(text, keyword_list$orcid_link))
  ackn_results <- ackn_text_sentences |>
    .enframe_results(name = "article", value = "ackn_statement")

  # orcid_results <- orcid_text_sentences |>
  #   .enframe_results(name = "article", value = "orcid_statement")

  orcids_anywhere_results <- orcids_anywhere |>
    .enframe_results(name = "article", value = "orcids_as_text")

  contrib_text_sentences |>
    .enframe_results(name = "article", value = "contrib_statement") |>
    dplyr::left_join(ackn_results, by = "article") |>
    # dplyr::left_join(orcid_results, by = "article") |>
    dplyr::left_join(orcids_anywhere_results, by = "article") |>
    dplyr::mutate(has_contrib = .data$contrib_statement != "",
                  has_ackn = .data$ackn_statement != "",
                  has_orcid = .data$orcids_as_text != "")
}

#' convert results from list to tibble
#' @noRd
.enframe_results <- function(ls, name, value) {
  ls |>
    purrr::map_chr(\(x) paste(x, collapse = " ")) |>
    tibble::enframe(name = name, value = value)
}

#' remove inserts
#' @noRd
.remove_inserts <- function(text_sentences, look_in_tables = FALSE) {

  keyword_list <- .create_keyword_list()

  regex_insert <- stringr::regex("<insert>", ignore_case = TRUE)

  flagged_inserts <- tibble::tibble(text = text_sentences, mask = 0) |>
    dplyr::mutate(mask =
                    ifelse(stringr::str_detect(.data$text, regex_insert),
                           1, .data$mask)) |>
    dplyr::mutate(mask = cumsum(.data$mask)) |>
    dplyr::group_by(.data$mask) |>
    dplyr::mutate(is_gap = stringr::str_detect(dplyr::lag(.data$text, default = ""), "<iend>") &
                    !stringr::str_detect(.data$text, regex_insert),
                  has_insert =
                    any(stringr::str_detect(.data$text, regex_insert)),
                  gapsum = cumsum(.data$is_gap)) |>
    dplyr::mutate(gapsum = ifelse(.data$has_insert == FALSE, 1, .data$gapsum))

  clean_section <- flagged_inserts |>
    dplyr::filter(.data$gapsum != 0) |>
    dplyr::pull(.data$text)

  if (look_in_tables == TRUE) {

    regex_table <- stringr::regex(keyword_list$table_credit,
                                  ignore_case = TRUE)
    contrib_tables <- flagged_inserts |>
      dplyr::filter(stringr::str_detect(.data$text, regex_table))

    if (nrow(contrib_tables) > 0) {
      return(
        flagged_inserts |>
          dplyr::filter(.data$gapsum == 0) |>
          dplyr::group_by(.data$mask) |>
          dplyr::mutate(is_appendix = any(stringr::str_detect(.data$text, regex_table))) |>
          dplyr::filter(.data$is_appendix == TRUE, .data$text != "<iend>") |>
          # dplyr::mutate(text = stringr::str_remove(.data$text, regex_insert)) |>
          dplyr::pull(.data$text) |>
          stringr::str_remove("^.*(continued\\) |Authors )(?=Name)") |>
          stringr::str_squish()
      )
    }
  }
  return(clean_section)

}

#' extract section
#' @noRd
.extract_section <- function(text_sentences, section_regexes, look_in_tables = FALSE) {

  keyword_list <- .create_keyword_list()

  section_string <- paste0("(<section>|#+)[^\\w+][\\d,^\\w]*(", section_regexes, ")\\b")

  text_sentences <- .remove_inserts(text_sentences, look_in_tables = look_in_tables)

  if (look_in_tables == TRUE & length(text_sentences) < 5) {
    return(text_sentences)
  }

  section_detections <-
    furrr::future_map_lgl(text_sentences,
                          \(sentence) stringr::str_detect(
                            sentence,
                            stringr::regex(section_string, ignore_case = TRUE)))

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

  str_section <- text_sentences[section_start] |>
    stringr::str_trim()
  str_section_sameline <- str_section |>
    stringr::str_remove(stringr::regex(section_string, ignore_case = TRUE))
  str_section_location <- stringr::str_locate(str_section, stringr::regex(section_string, ignore_case = TRUE))

  if (str_section_location[1] > 1) {
    text_sentences[section_start] <- stringr::str_sub(text_sentences[section_start],
                                                      str_section_location[1],
                                                      stringr::str_length(text_sentences[section_start]))
  }

  stop_regex <- keyword_list$stop_sections

  if (stringr::str_detect(section_regexes, "thank")) {
    stop_regex <- stop_regex[!stringr::str_detect(stop_regex,
                             "fund|financ")]
  }

  stop_regex <- stop_regex[!stringr::str_detect(stop_regex,
                                                section_regexes)] |>
    paste(collapse = "|")

  # candidates are sentences after the first section but before the next
  # which begin with <section> or digit. (reference number at start of line)
  is_plos <- any(stringr::str_detect(text_sentences[1:10], "^plos "), na.rm = TRUE) #explosive?

  if (is_plos == TRUE) {

    section_end <-
      furrr::future_map_lgl(
        text_sentences[(section_start + 1):length(text_sentences)],
        \(sentence) stringr::str_detect(sentence,
                                        stringr::regex("section> references",
                                                       ignore_case = TRUE))) |>
      which() - 1

  } else {

      section_end_candidates <-
        furrr::future_map_lgl(
          text_sentences[(section_start + 1):length(text_sentences)],
          \(sentence) stringr::str_detect(sentence,
                                          stringr::regex(stop_regex,
                                                         ignore_case = TRUE))) |>
        which() - 1

      section_end <- section_end_candidates[1]

  }

  if (is.na(section_end)) section_end <- 0
  section_end <- section_start + section_end

  section <- text_sentences[section_start:section_end]

  if (section_start < 50 & any(stringr::str_detect(text_sentences[1:10],
                                                   stringr::regex("plos",
                                                                  ignore_case = TRUE)),
                               na.rm = TRUE)) {
    section <- oddpub::splice_margin_text(section)
  }
  section |>
    stringr::str_remove_all("\\u200b") |> # remove zerowidth spaces
    stringr::str_trim()
}

#' extract contribution section with progress bar
#' @noRd
.extract_section_progress <- function(text_sentences,
                                      keywords,
                                      look_in_tables) {
  p <- progressr::progressor(along = text_sentences)
  text_sentences |>
    furrr::future_map(\(x) {
      p()
      .extract_section(x,
                       keywords,
                       look_in_tables = look_in_tables)

      })
}

#' extract any orcids from a list of papers with progress bar
#' @noRd
.extract_all_orcids_progress <- function(text_sentences, orcid_regex) {
  p <- progressr::progressor(along = text_sentences)
  text_sentences |>
    furrr::future_map(\(x) {
      p()
      .extract_all_orcids(x, orcid_regex)
    })
}

#' extract any orcids from a vector of strings
#' @noRd
.extract_all_orcids <- function(str_vec, orcid_regex) {
  # tt <-
  str_vec |>
    stringr::str_extract_all(orcid_regex, simplify = TRUE) |>
    unique() |>
    dplyr::na_if("") |>
    stats::na.omit() |>
    paste(collapse = ";")

}
