#' Search for ORCID hyperlinks and extract ORCIDs.
#'
#' The algorithm searchers for an ORCID and extracts any links that it finds.
#'
#' @param pdf_file String with the path to the PDF to be screened.
#'
#' @return String with the ORCIDs
#'
#' @examples
#' \dontrun{
#' extract_orcid_hyperlink(some_pdf)
#' }
#'
#' @export
extract_orcid_hyperlink <- function(pdf_file) {

  pdf_temp <- file.path(tempdir(), "output.pdf")

  orcids <- pdftools::pdf_combine(pdf_file, output = pdf_temp) |>
    readr::read_file_raw() |>
    purrr::map_chr(rawToChar) |>
    paste(collapse = "") |>
    stringr::str_extract_all(stringr::regex("(https?\\://orcid\\.org/\\d{4}-\\d{4}-\\d{4}-\\w{4})|(https?.*orcid.*(\\d|X))",
                                            ignore_case = TRUE)) |>
    unlist() |>
    unique() |>
    stringi::stri_unescape_unicode()

  if (length(orcids) > 1) {
    return(paste(orcids, collapse = "; "))
  } else if (length(orcids) == 0) {
    return("")

  } else {
    return(orcids)
  }
}

#' Search for ORCID hyperlinks and extract ORCIDs from a folder of PDF files.
#'
#' The algorithm searchers for an ORCID and extracts what it finds from each file.
#'
#' @param pdf_folder String with the path to folder with PDF files to be screened.
#'
#' @return Tibble with one row per screened file and the file name and ORCIDs extracted,
#' as well as a logical value for ORCIDs detected as columns.
#'
#' @examples
#' \dontrun{
#' extract_orcids_from_folder(pdf_folder)
#' }
#'
#' @export
extract_orcids_from_folder <- function(pdf_folder) {

  pdf_files <- list.files(pdf_folder, full.names = TRUE)
  p <- progressr::progressor(along = pdf_files)
  furrr::future_map_chr(pdf_files, \(x) {
    p()
    extract_orcid_hyperlink(x)
  })
}