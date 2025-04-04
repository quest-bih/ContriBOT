#' Seach for CRediT keywords and extract section.
#'
#' The algorithm searchers for a  Authorship, Acknowledgement, and ORCID statements
#' and extracts them. If no known section title was found, the algorithm searches
#' for several categories of similar keywords in each sentence of the whole manuscript.
#'
#' @param PDF_text_sentences Document corpus loaded with the pdf_load function.
#'
#' @return Tibble with one row per screened document and the file name and logical values for authorship,
#' acknowledgement, and orcid statements detected as columns,
#' plus additional columns that contain the statements that were extracted.
#'
#' @examples
#' \dontrun{
#' contribution_detection(pdf_load("examples/"))
#' }
#'
#' @export
contribution_detection <- function(PDF_text_sentences)
{

  # credit_regex <- c("manuscript\\.?$",
  #                   "editing",
  #                   "version",
  #                   "paper",
  #                   "work",
  #                   "project\\.$",
  #                   "entirety\\.$",
  #                   "validation",
  #                   "review",
  #                   "analysis",
  #                   "investigation",
  #                   "methodo",
  #                   "supervision",
  #                   "\\(equal\\)",
  #                   "writing",
  #                   "contributors?",
  #                   "resources",
  #                   "author",
  #                   "conceptuali(z|s)ation",
  #                   "draft"
  # ) |>
  #   paste(collapse = "|")


  credit_section_list <- c(
    "Author\\W?s?\\W? ?Contributions? Statement",
    "A ?u ?t ?h ?o ?r ?(\\W?s?|s? ?\\W?)? ?C ?o ?n ?t ?r ?i ?b ?u ?t ?i ?o ?n ?s?",
    "CRediT Authors?hip? Contributions?( Statement)?",
    "CRediT Author Statement",
    "Author\\W?s?\\W?s responsibilities",
    "Contribut.* Statement",
    # "Contributor’s Statement"
    "Contributions$|Contributions:",
    "C ?O ?N ?T ?R ?I ?B ?U ?T ?I ?O ?N ?S ?O ?F ?A ?U ?T ?H ?O ?R ?S",
    "AUTHOR CONTRIBUTORS",
    "Author statements?",
    "C ?o ?n ?t ?r ?i ?b ?u ?t ?o ?r ?s", # TODO: check this for 10.1136 $ or no $
    # paste0("(", "Contributors .* (", credit_regex, "))"),
    "Authorship",
    "Author roles",
    "Name Location Contribution",
    "Description of authors\\W? roles"
  ) |>
    .format_keyword_vector() |>
    stringr::str_replace_all("w", "W")


# str_detect("credit authors contributions", "credit authors?(hip)? contributions?(statement)?")
  ackn_section_list <- c(
    "A ?c ?k ?n ?o ?w ?l ?e ?d ?g ?e? ?m ?e ?n ?t ?s?",
    "We (would like to )?thank",
    "Additional contributions"
  ) |>
    .format_keyword_vector()



  orcid_section_list <- c(
    "ORCID"
  ) |>
    .format_keyword_vector()

  # PDF_text_sentences <- pdf_text_corpus

  # PDF_text_sentences <- list(PDF_text_sentences)
  # tib <- tibble(text = PDF_text_sentences[[1]])
  print("Extracting Contributions...")
  contrib_text_sentences <- PDF_text_sentences |>
    furrr::future_map(\(x) .extract_section(x, credit_section_list, look_in_tables = TRUE), .progress = TRUE)

  print("Extracting Acknowledgements...")
  ackn_text_sentences <- PDF_text_sentences |>
    furrr::future_map(\(x) .extract_section(x, ackn_section_list), .progress = TRUE)

  print("Extracting ORCIDs...")
  orcid_text_sentences <- PDF_text_sentences |>
    furrr::future_map(\(x) .extract_section(x, orcid_section_list), .progress = TRUE)

  ackn_results <- ackn_text_sentences |>
    .enframe_results(name = "article", value = "ackn_statement")

  orcid_results <- orcid_text_sentences |>
    .enframe_results(name = "article", value = "orcid_statement")

  contrib_text_sentences |>
    .enframe_results(name = "article", value = "contrib_statement") |>
    dplyr::left_join(ackn_results, by = "article") |>
    dplyr::left_join(orcid_results, by = "article") |>
    dplyr::mutate(has_contrib = contrib_statement != "",
                  has_ackn = ackn_statement != "",
                  has_orcid = orcid_statement != "")

}


#' convert results from list to tibble
#' @noRd
.enframe_results <- function(ls, name, value) {
  ls |>
    purrr::map_chr(\(x) paste(x, collapse = " ")) |>
    tibble::enframe(name = name, value = value)
}
# tib <- tibble(text = PDF_text_sentences)
# section_regexes <- credit_section_list
# section_regexes <- orcid_section_list
#' extract section
#' @noRd
.extract_section <- function(PDF_text_sentences, section_regexes, look_in_tables = FALSE) {

  # TODO: validate that text extraction stops at the right spots for ackn and credit

  table_titles <- c("appendix( \\d)? authors")

  section_string <- paste0("(<section>)[^\\w+][\\d,^\\w]*(", section_regexes, ")\\b")
  if (look_in_tables == TRUE) section_string <- paste0(section_string, "|", table_titles)
  # stringr::str_detect(PDF_text_sentence, data_availability)
  section_detections <- furrr::future_map_lgl(PDF_text_sentences,
                                          \(sentence) stringr::str_detect(sentence, stringr::regex(section_string, ignore_case = TRUE)))
# str_detect("<section> contributors all authors were involved in the discussion and formulation of the points to consider.", section_string)
  # if (sum(section_detections) > 0) {
  #   DAS_detections <- furrr::future_map_lgl(PDF_text_sentences,
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

  str_section <- PDF_text_sentences[section_start] |>
    stringr::str_trim()
  str_section_sameline <- str_section |>
    stringr::str_remove(stringr::regex(section_string, ignore_case = TRUE))

  # section_regexes

  stop_regex <- c(
    "c ?o ?n ?f ?l ?i ?c ?t ?s?",
    "o ?f ?i ?n ?t ?e ?r ?e ?s ?t",
    "r ?e ?f ?e ?r ?e ?n ?c ?e ?s",
    "disclaimer",
    "availability",
    "competing",
    "prior presentation",
    "specialty section",
    "d ?e ?c ?l ?a ?r ?a ?t ?i ?o ?n",
    "additional information:",
    "a ?c ?k ?n ?o ?w ?l ?e ?d ?g ?e? ?m ?e ?n ?t ?s?",
    "orcid",
    "<section>(\\) sources of)? funding",
    "disclosures?",
    "peer",
    "ethic",
    "keywords",
    "data .* statement",
    "credit author statement",
    "author contributions",
    "accepted \\d{1,2}",
    "\\u00a9",
    "citation:",
    "<section> open data",
    "supplement",
    "et al\\.",
    "abbreviations",
    "twitter",
    "receive"
    )

  stop_regex <- stop_regex[!stringr::str_detect(stop_regex, section_regexes)] |>
    paste(collapse = "|")

  # candidates are sentences after the first section but before the next
  # which begin with <section> or digit. (reference number at start of line)
  is_plos <- any(stringr::str_detect(PDF_text_sentences[1:10], "^plos ")) #explosive?

  if (is_plos == TRUE) {

    section_end <- furrr::future_map_lgl(PDF_text_sentences[(section_start + 1):length(PDF_text_sentences)],
                                         \(sentence) stringr::str_detect(sentence, stringr::regex("section> references", ignore_case = TRUE))) |>
      which() - 1

  } else {

    # if (stringr::str_detect(section_regexes, "contribution")) {

      section_end_candidates <- furrr::future_map_lgl(PDF_text_sentences[(section_start + 1):length(PDF_text_sentences)],
                                           \(sentence) stringr::str_detect(sentence, stringr::regex(stop_regex, ignore_case = TRUE))) |>
        which() - 1

      section_end <- section_end_candidates[1]

    # } else {
    #
    #   section_end_candidates <- furrr::future_map_lgl(PDF_text_sentences[(section_start + 1):length(PDF_text_sentences)],
    #                                                   # \(sentence) stringr::str_detect(sentence, "section> (?!d )|^\\d\\.|section> references")) |>
    #                                                   \(sentence) stringr::str_detect(sentence, stop_regex)) |>
    #     which() - 1
    #   # check if candidates are full sentences ending in full stop. This achieves splicing if section contines on next page
    #   completed_sentences <- furrr::future_map_lgl(PDF_text_sentences[section_start + section_end_candidates],
    #                                                \(sentence) stringr::str_detect(sentence, "\\.$"))
    #
    #   if (stringr::str_length(str_section_sameline) < 5 & str_section_sameline != ".") {
    #     # first_sentence <- DAS_start + 1
    #
    #     section_end <- section_end_candidates[-1][completed_sentences[-1]][1]#
    #
    #   } else {
    #     section_end <- section_end_candidates[completed_sentences][1] # the first complete sentence before the beginning of a section
    #
    #   }
    #
    # }

    # if (section_start / length(section_detections) < 0.1 & section_end != 0) { # for plos journals with DAS on first page
    #
    #   section_second_part <- furrr::future_map_lgl(PDF_text_sentences[(section_start + section_end + 1):length(PDF_text_sentences)],
    #                                            \(sentence) stringr::str_detect(sentence, "<section> funding:"))
    #   if (sum(section_second_part) == 0) {
    #     section_end <- section_end
    #   } else {
    #     section_end <- section_end + which(section_second_part) - 1
    #   }
    # }
  }

  if (is.na(section_end)) section_end <- 0
  section_end <- section_start + section_end

  section <- PDF_text_sentences[section_start:section_end]

  if (section_start < 50 & any(stringr::str_detect(PDF_text_sentences[1:10], stringr::regex("plos", ignore_case = TRUE)))) {
    section <- .splice_plos_twopager(section)
  }
  section |>
    # paste(collapse = " ") |>
    stringr::str_remove_all("\\u200b") |> # remove zerowidth spaces
    stringr::str_trim()

}


#' standardize the format of different keyword vectors
#' @noRd
.format_keyword_vector <- function(keywords, end_boundary = FALSE) {
  #typically word boundaries are added in the beginning only to allow for different possible endings
  if(end_boundary) {
    keywords_formatted <- paste0("\\b", keywords, "\\b")
  } else {
    keywords_formatted <- paste0("\\b", keywords)
  }
  #collapse keywords into one string with OR symbol between them and convert to lowercase
  keywords_formatted <- paste(keywords_formatted, collapse = "|") |> tolower()

  return(keywords_formatted)
}

# pdf_folder <- "C:/Users/Vladi/OneDrive - Charité - Universitätsmedizin Berlin/PDFs_22/"
#
# PDF_file <- paste0(pdf_folder, "dir2/10.1002+alz.12365.pdf")

#' Seach for ORCID hyperlinks and extract ORCIDs.
#'
#' The algorithm searchers for an ORCID and extracts what it finds.
#'
#' @param PDF_file String with the path to the PDF to be screened.
#'
#' @return String with the ORCIDs
#'
#' @examples
#' \dontrun{
#' extract_orcid_hyperlink(some_pdf)
#' }
#'
#' @export

extract_orcid_hyperlink <- function(PDF_file) {
  orcids <- readr::read_file_raw(PDF_file) |>
    furrr::future_map_chr(rawToChar) |>
    paste(collapse = "") |>
    stringr::str_extract_all(stringr::regex("(https?\\://orcid\\.org/\\d{4}-\\d{4}-\\d{4}-\\w{4})|(https?.*orcid.*(\\d|X))",
                             ignore_case = TRUE)) |>
    unlist() |>
    unique() |>
    stringi::stri_unescape_unicode()

   # stringr::str_view("https://orcid.org/000-0002-1162-1318)>>/Type/Annot/Subtype/Link/Rect[301.436 642.104 309.316 652.422]/Border[0 0 0; https://orcid.org/0000-0001-5332-6811; https://orcid.org/0000-0002-4232-3305", "(https?.*orcid.*(\\d|X)(?=(\\)|,)))")

# stringr::str_view("http\072\057\057orcid\056org\0570000\0550001\0556389\0550029", "(https?\\://orcid\\.org/\\d{4}-\\d{4}-\\d{4}-\\w{4})|
#   (https?\072\057\057orcid\056org\057\\d{4}\055\\d{4}\055\\d{4}\055\\w{4})")
  if (length(orcids) > 1) {
    return(paste(orcids, collapse = "; "))
  } else if (length(orcids) == 0) {
    return("")

  } else {
      return(orcids)
    }
}

# tib <- tibble(orcids)
#' Seach for ORCID hyperlinks and extract ORCIDs from a folder of PDF files.
#'
#' The algorithm searchers for an ORCID and extracts what it finds from each file.
#'
#' @param pdf_folder String with the path to folder with PDF files to be screened.
#'
#' @return Tibble with one row per screened file and the file name and orcids extracted,
#' as well as a logical value for orcids detected as columns.
#'
#' @examples
#' \dontrun{
#' extract_orcids_from_folder(pdf_folder)
#' }
#'
#' @export
extract_orcids_from_folder <- function(pdf_folder) {

  PDF_files <- list.files(pdf_folder, full.names = TRUE)

  furrr::future_map_chr(PDF_files, extract_orcid_hyperlink,
                        .progress =  TRUE)
}

