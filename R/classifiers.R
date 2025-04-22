#' Classify statements for CRediT, narrative, and authorship criteria.
#'
#'
#' @param tib Tibble with extracted contribution statements with the
#' `extract_contributions` function.
#' @param key_col Unquoted expression with the name of the key column of the input
#' tibble, usually `doi` or `file_name`.
#' @param statement_col Unquoted expression with the name of the column that
#' contains the authorship statements to be classified.
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
#' classify_contributions <- extracted_sections()
#' }
#'
#' @export
classify_contributions <- function(tib, key_col, statement_col) {
  
  keyword_list <- .create_keyword_list()
  
  cols_no_key <- setdiff(names(tib), rlang::as_name(rlang::enquo(key_col)))
  
  tib_unnested <- tib |>
    dplyr::mutate(cleaned_statement = .clean_sentences({{ statement_col }})) |>
    unnest_statements(cleaned_statement) |>
    dplyr::mutate(is_all_authors =
                    stringr::str_detect(sentence, keyword_list$all_authors),
           has_credit_role =
             stringr::str_detect(sentence, keyword_list$credit_roles),
           n_credit =
             stringr::str_count(sentence, keyword_list$credit_roles),
           n3_credit =
             stringr::str_count(sentence, keyword_list$n3_credit_roles),
           has_noncredit_role =
             stringr::str_detect(sentence, keyword_list$non_credit_roles) &
             !is_all_authors,
           is_equally =
             stringr::str_detect(sentence, keyword_list$equally),
           is_narrative =
             stringr::str_detect(sentence, keyword_list$narrative) &
             !is_all_authors & !is_equally,
           is_responsibility =
             stringr::str_detect(sentence, keyword_list$responsibility),
           is_contrib = (n_credit > 0 | has_noncredit_role | is_narrative) &
             !is_all_authors & !is_equally)

  tib_classified <- tib_unnested |>
    dplyr::group_by({{ key_col }}) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(cols_no_key), dplyr::first),
              has_credit = any(has_credit_role, na.rm = TRUE),
              has_non_credit = any(has_noncredit_role, na.rm = TRUE),
              is_narrative = any(is_narrative, na.rm = TRUE),
              n_credit = sum(n_credit, na.rm = TRUE),
              n3_credit = sum(n3_credit,na.rm = TRUE),
              is_responsibility = any(is_responsibility,na.rm = TRUE),
              cleaned_sentences = paste(sentence, collapse = ". "),
              credit_estimate = n_credit > 3 & !has_non_credit,
              contrib_estimate = any(is_contrib, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      credit_estimate = factor(
        dplyr::case_when(
          n_credit > 3 & !has_non_credit ~ TRUE,
          n_credit <= 3 & !has_non_credit & n3_credit > 2 ~ TRUE,
          .default = FALSE
        ), levels = c(TRUE, FALSE)
      ),
      # credit_truth = factor(as.logical(CRT_Taxonomy, na.rm = TRUE),
      #                       levels = c(TRUE, FALSE)),
      contrib_estimate = factor(contrib_estimate, levels = c(TRUE, FALSE)),
      # contrib_truth = factor(as.logical(Contributions, na.rm = TRUE),
      #                        levels = c(TRUE, FALSE)),
      narrative_estimate = factor(is_narrative, levels = c(TRUE, FALSE)),
      # narrative_truth = factor(as.logical(Narrative, na.rm = TRUE),
      #                          levels = c(TRUE, FALSE)),
      authorship_estimate = factor(is_responsibility,
                                   levels = c(TRUE, FALSE))
      # Authorship_truth = factor(as.logical(Auth_criteria, na.rm = TRUE),
      #                           levels = c(TRUE, FALSE))
      )
  
  return(tib_classified)
}

# res <- classify_contributions(tib, doi, Statement_Text)

