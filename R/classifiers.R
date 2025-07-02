#' Classify statements for CRediT, narrative, and authorship criteria.
#'
#' @param tib Tibble with extracted contribution statements with the
#' `extract_contributions` function.
#' @param key_col Unquoted expression with the name of the key column of the input
#' tibble, usually `doi` or `file_name`.
#' @param statement_col Unquoted expression with the name of the column that
#' contains the authorship statements to be classified.
#' @importFrom rlang .data
#'
#' @return Tibble with one row per screened document and logical values for
#'  credit_estimate (whether or not the statement follows CREdiT),
#'  contrib_estimate (whether or not author roles are listed at all),
#'  narrative_estimate (whether or not author roles are given with sentences
#'  containing verbs), and authorship_estimate (whether or not a statement about
#'  the authors fulfilling authorship criteria is included).
#'
#' @examples
#' \dontrun{
#' extracted_sections <- extract_contributions(oddpub::pdf_load("examples/"))
#' contribution_results <- classify_contributions(extracted_sections)
#' }
#'
#' @export
classify_contributions <- function(tib, key_col, statement_col) {

  keyword_list <- .create_keyword_list()

  cols_no_key <- setdiff(names(tib), rlang::as_name(rlang::enquo(key_col)))

  tib_unnested <- tib |>
    dplyr::mutate(cleaned_statement = .clean_sentences({{ statement_col }})) |>
    unnest_statements(.data$cleaned_statement) |>
    dplyr::mutate(is_all_authors =
                    stringr::str_detect(.data$sentence, keyword_list$all_authors),
           has_credit_role =
             stringr::str_detect(.data$sentence, keyword_list$credit_roles),
           n_credit =
             stringr::str_count(.data$sentence, keyword_list$credit_roles),
           n3_credit =
             stringr::str_count(.data$sentence, keyword_list$n3_credit_roles),
           has_noncredit_role =
             stringr::str_detect(.data$sentence, keyword_list$non_credit_roles) &
             !.data$is_all_authors,
           is_advancement = stringr::str_detect(.data$sentence, keyword_list$advancement),
           is_equally =
             stringr::str_detect(.data$sentence, keyword_list$equally),
           is_responsibility =
             stringr::str_detect(.data$sentence, keyword_list$responsibility),
           is_narrative =
             stringr::str_detect(.data$sentence, keyword_list$narrative) &
             !.data$is_all_authors & !.data$is_equally & !.data$is_responsibility,
           is_contrib = (.data$n_credit > 0 |
                           .data$has_noncredit_role |
                           .data$is_narrative) &
             !.data$is_all_authors & !.data$is_equally & !.data$is_advancement)

  tib_classified <- tib_unnested |>
    dplyr::group_by({{ key_col }}) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(cols_no_key), dplyr::first),
              has_credit = any(.data$has_credit_role, na.rm = TRUE),
              has_non_credit = any(.data$has_noncredit_role, na.rm = TRUE),
              is_narrative = any(.data$is_narrative, na.rm = TRUE),
              n_credit = sum(.data$n_credit, na.rm = TRUE),
              n3_credit = sum(.data$n3_credit,na.rm = TRUE),
              is_responsibility = any(.data$is_responsibility,na.rm = TRUE),
              cleaned_sentences = paste(.data$sentence, collapse = ". "),
              credit_estimate = .data$n_credit > 3 & !.data$has_non_credit,
              contrib_estimate = any(.data$is_contrib, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      credit_estimate = factor(
        dplyr::case_when(
          .data$n_credit > 3 & !.data$has_non_credit ~ TRUE,
          .data$n_credit <= 3 & !.data$has_non_credit & .data$n3_credit > 2 ~ TRUE,
          .default = FALSE
        ), levels = c(TRUE, FALSE)
      ),
      contrib_estimate = factor(.data$contrib_estimate, levels = c(TRUE, FALSE)),
      narrative_estimate = factor(.data$is_narrative, levels = c(TRUE, FALSE)),
      authorship_estimate = factor(.data$is_responsibility,
                                   levels = c(TRUE, FALSE))
      ) |>
    dplyr::select(dplyr::all_of(c(rlang::as_name(rlang::enquo(key_col)), cols_no_key)),
                  dplyr::contains("estimate"))

  return(tib_classified)
}
