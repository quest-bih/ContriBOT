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