# #
#  credit_str <- "<section> Author Contributions <section> The followings are the authors' contributions: study concepts: M.K. and U.K; study design: M.K. and U.K; data acquisition: K.K., M.K., and U.K; quality control of data and algorithms: K.K., U.G., M.K., and V.G; data analysis and interpretation: P.M., U.G., K.K., and U.K; statistical analysis: P.M., U.G., and K.K; manuscript preparation: U.G. and K.K; manuscript editing: all; manuscript review: all."
# #
# credit_str <- "<section> AUTHOR CONTRIBUTIONS <section> Conception and design: All authors <section> Collection and assembly of data: Reinhard Büttner, John R. Gosney, Birgit Guldhammer Skov, Julien Adam, Noriko Motoi, Keith M. Kerr, Ming- <section> Sound Tsao <section> Data analysis and interpretation: All authors <section> Manuscript writing: All authors <section> Final approval of manuscript: All authors <section> Accountable for all aspects of the work: All authors"
# credit_str <- credit_results |> filter(article == "10.1212+nxi.0000000000200359.txt") |> pull(contrib_statement)
# rr <- parse_credit_roles(credit_str)

#' `r lifecycle::badge("experimental")`
#' Parse an authorship statement that is list-formatted, especially as CRediT, and extract
#' the authors and roles into a tidy tibble, with a single author and role per row.
#'
#' @param credit_str String with extracted contribution statement with the
#' `extract_contributions` function.
#' @importFrom rlang .data
#'
#' @return Tibble with one row per author and role, or NA if the statement could
#' not be parsed, for example if narrative or formatted in a non-standardized way.
#'
#' @examples
#' \dontrun{
#' parse_credit_roles("<section> Author Contributions <section>:
#'  study concepts: A.B. and C.D; study design: A.B. and E.F;
#'   data acquisition: G.H, A.B., and C.D.;
#'    quality control of data and algorithms: A.B., G.H., I.J., and C.D.;
#'     data analysis and interpretation: K.L., I.J., G.H., and C.D.;
#'      statistical analysis: G.H., I.J., and K.L;
#'       manuscript preparation: A.B. and E.F.; manuscript editing: all;
#'        manuscript review: all.")
#' # or if we have a tibble of extracted and classified authorship statements:
#' library(dplyr)
#' library(purrr)
#' parsed_results <- credit_results |>
#'  pull(contrib_statement) |>
#'  map(parse_credit_roles)
#' }
#'
#' @export
parse_credit_roles <- function(credit_str) {

  V1 <- Vs <- NULL

  keyword_list <- .create_keyword_list()

  if (stringr::str_locate(credit_str, stringr::regex(keyword_list$authorship_section, ignore_case = TRUE)) |>
    (\(x) x[2])() < 100) {
    credit_str <- credit_str |>
      stringr::str_remove(stringr::regex(paste0(".*(", keyword_list$authorship_section,
                                                ")"), ignore_case = TRUE))
  }

  credit_str <- stringr::str_remove_all(credit_str, "<section>|##+") |>
    stringr::str_remove_all(stringr::regex(paste0("(", keyword_list$authorship_section, ")\\.?"), ignore_case = TRUE)) |>
    stringr::str_remove("^ ?\\: ?") |>
    stringr::str_remove(" <iend>.*") |>
    stringr::str_squish()

  separators <- tibble::tibble(seps = stringr::str_extract_all(credit_str, "[:punct:]") |> unlist()) |>
    dplyr::filter(!.data$seps %in% c(":", "\\u2019", ".", "(", ")")) |>
    dplyr::count(.data$seps, sort = TRUE)

  abbrev <- FALSE
  if (stringr::str_count(credit_str, "[A-Z]\\.[A-Z]\\.") > 2) {
    abbrev <- TRUE
  }

  # if roles are separeted by list separator:
  list_sep <- stringr::str_extract(credit_str,
  stringr::regex(paste0("(", keyword_list$credit_roles, ")\\W+(", keyword_list$credit_roles, ")"), ignore_case = TRUE)) |>
    stringr::str_extract("[:punct:]")

  # if no sep or if a unicode character
  if (is.na(list_sep) | utf8ToInt(list_sep) > 127) {
    list_sep <- stringr::str_extract(credit_str, ",(?= and)|(?<=\\.),")

    if (is.na(list_sep)) {
      list_sep <- separators |>
        dplyr::slice(1) |>
        dplyr::pull(.data$seps)
    }
  }

  if (length(list_sep) == 0) list_sep <- "_"

  if (list_sep != ";" | stringr::str_count(credit_str, ",") == 0) {
    role_sentences <- tokenizers::tokenize_regex(credit_str, "(((?<!\\b\\w{1,2})\\.)|;|\\.(?= (All|The)))(?=\\s)(?! and)", simplify = TRUE) |>
        stringr::str_squish()
    if (stringr::str_count(credit_str, "%") >= 2) {
      # for e.g. tabular format 10.31822+jomat.2023-8-2-141
      role_sentences <- tokenizers::tokenize_regex(credit_str, "(?<=\\d{2}%) ", simplify = TRUE) |>
        stringr::str_squish()
    }
  } else {
    role_sentences <- tokenizers::tokenize_regex(credit_str, "(((?<!\\b\\w{1,2})\\.|\\.(?= (All|The))))(?=\\s)(?! and)", simplify = TRUE) |>
      stringr::str_squish()
  }

  # if (stringr::str_detect(credit_str, keyword_list$narrative) |
  #     stringr::str_detect(credit_str, keyword_list$responsibility) |
  #     stringr::str_detect(credit_str, keyword_list$advancement)) {

    role_sentences <- role_sentences |>
      remove_sentences_regex(keyword_list$narrative) |>
      remove_sentences_regex(keyword_list$responsibility) |>
      remove_sentences_regex(keyword_list$advancement) |>
      remove_sentences_regex(keyword_list$all_authors) |>
      stringr::str_squish()
  # }

  if (length(role_sentences) == 0) return(NA)


  first_sentence <- role_sentences[1]

  role_author_pairs <- role_sentences |>
    strsplit("\\: ")


  if (!stringr::str_detect(first_sentence,
                           stringr::regex(keyword_list$credit_roles, ignore_case = TRUE)) |
      stringr::str_detect(first_sentence,
                          stringr::regex(keyword_list$non_credit_roles, ignore_case = TRUE))) {
    message("No CRediT role found in first sentence of statement!
            Statement will be further processed only if formatted with semicolons!")
    if (sum(stringr::str_count(role_sentences, "\\:")) != length(role_sentences)) {

      if (sum(stringr::str_count(role_sentences, "(?<![A-Z])\\.|;"), na.rm = TRUE) == 0 &
          sum(stringr::str_count(role_sentences, "\\:")) > 0) {
        left_content <- role_sentences |>
          stringr::str_extract_all("[A-Z]\\w+(\\s[a-z\\-]+)+(?=\\:)") |>
          unlist()
        right_content <- role_sentences |>
          strsplit("[A-Z]\\w+(\\s[a-z\\-]+)+\\: ") |>
          unlist() |>
          stringr::str_squish()

        right_content <- right_content[right_content != ""]
        if (length(left_content) != length(right_content)) {
          message("Non-standard formatting encountered! Statement will not be parsed.")
          return(NA)
        }

      } else {
        message("Non-standard formatting encountered! Statement will not be parsed.")
        return(NA)
      }
    } else {
      left_content <- role_author_pairs |>
        purrr::map_chr(1) |>
        stringr::str_remove_all("\\.$")
      right_content <- role_author_pairs |>
        purrr::map_chr(2) |>
        stringr::str_remove_all("\\.$")
    }


    if (any(stringr::str_detect(left_content, stringr::regex(keyword_list$credit_roles, ignore_case = TRUE)) |
            stringr::str_detect(left_content, stringr::regex(keyword_list$non_credit_roles, ignore_case = TRUE)))) {


      contrib_tib <- tibble::tibble(credit_roles = left_content,
                                   credit_names = right_content
                                   )
    } else {
      contrib_tib <- tibble::tibble(credit_roles = right_content,
                                   credit_names = left_content
      )
    }

    contrib_tib <- unnest_contrib(contrib_tib, credit_names, list_sep) |>
      dplyr::rename(contrib_names = credit_names,
                    contrib_roles = credit_roles)

    return(contrib_tib)
  }

  #
  # contrib_sep <- stringr::str_extract(credit_str, stringr::regex(paste0("(", keyword_list$credit_roles, ")\\W+(", keyword_list$credit_roles, ")"), ignore_case = TRUE)) |>
  #   stringr::str_extract("[:punct:]")
  #
  # role_sentences

  # str_view(credit_str, stringr::regex(paste0("(", keyword_list$credit_roles, ")\\W+(?!(", keyword_list$credit_roles, "))"), ignore_case = TRUE))

  # stringr::str_extract_all(credit_str, stringr::regex(paste0("(", keyword_list$credit_roles, ")\\W+(?!(", keyword_list$credit_roles, "))"), ignore_case = TRUE))
  # contrib_sep <- separators |>
  #   dplyr::slice(2) |>
  #   dplyr::pull(seps)

  # if (contrib_sep == ".") contrib_sep <- "\\."
#
#   list_sep <- separators |>
#     dplyr::slice(1) |>
#     dplyr::pull(seps)
#
#   if (abbrev == FALSE) {
#     role_sentences <- tokenizers::tokenize_regex(role_sentences, paste0("(\\.|", contrib_sep, ")(?=\\s)"), simplify = TRUE) |>
#       stringr::str_squish()
#   } else {
#     role_sentences <- tokenizers::tokenize_regex(credit_str, paste0("(", contrib_sep, ")(?=\\s)"), simplify = TRUE) |>
#       stringr::str_squish()
#   }

  #### TODO: debug this for multiword roles ?
  role_first <- stringr::str_extract(role_sentences[1], "^\\w+\\b") |>
    stringr::str_detect(stringr::regex(keyword_list$credit_roles, ignore_case = TRUE))

  credit_roles <- role_sentences |>
    stringr::str_extract_all(stringr::regex(keyword_list$credit_roles,
                                            ignore_case = TRUE), simplify = TRUE) |>
    as.data.frame() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) stringr::str_remove_all(x, "[\\.,\\:] ?")))

  ######TODO replace empties with actual roles from list

  # if (!stringr::str_detect(credit_str, "\\:") & abbrev != TRUE) {

  if (stringr::str_count(credit_str, "\\:") > 1) {

    if (role_first == FALSE) {
      credit_names <- role_sentences |>
        stringr::str_remove("\\:.*")
    } else {
      credit_names <- role_sentences |>
        stringr::str_remove("^.*\\: ?")
    }


  } else {
    credit_names <- role_sentences |>
      stringr::str_remove_all(stringr::regex(keyword_list$credit_roles, ignore_case = TRUE)) |>
      stringr::str_remove_all(paste0(";|\\:|(", list_sep, " )+$")) |>
      stringr::str_remove_all(paste0("^", list_sep, " ?")) |>
      stringr::str_squish()
    }



  # } else {
#
#     if (stringr::str_count(credit_str, "\\:") > 1) {
#       split_sep <- "\\:"
#
#     } else if (stringr::str_detect(credit_str, stringr::regex(paste0(keyword_list$credit_roles, " ?", list_sep), ignore_case = TRUE))) {
#       split_sep <- list_sep
#     }
#
#     name_role_pairs <- role_sentences |>
#       stringr::str_replace(paste0(split_sep, " ?"), "_") |>
#       strsplit("_")
#
#     if (role_first) {
#       credit_roles <- name_role_pairs |>
#         purrr::map_chr(1)
#       credit_names <- name_role_pairs |>
#         purrr::map_chr(2)
#     } else {
#       credit_roles <- name_role_pairs |>
#         purrr::map_chr(2)
#       credit_names <- name_role_pairs |>
#         purrr::map_chr(1)
#     }
#   }

  if (is.data.frame(credit_roles)) {
    n_roles <- nrow(credit_roles)
  } else {
    n_roles <- length(credit_roles)
  }


  if (n_roles == length(credit_names)) {

    credit_tib <- tibble::tibble(credit_roles, credit_names)

    if (ncol(credit_tib) > 2) {
      credit_tib <- credit_tib |>
        tidyr::pivot_longer(-credit_names, names_to = "Vs", values_to = "credit_roles") |>
        dplyr::select(-Vs) |>
        dplyr::filter(.data$credit_roles != "", .data$credit_names != "")
    } else if ("V1" %in% names(credit_tib)) {
      credit_tib <- credit_tib |>
        dplyr::rename(credit_roles = V1)
    }

    credit_tib <- unnest_contrib(credit_tib, credit_names, list_sep) |>
      unnest_contrib(credit_roles, list_sep)  |>
      dplyr::mutate(contrib_roles_std = standardize_roles(credit_roles)) # standardized contribution roles

    return(credit_tib)
  } else {
    return(NA)
  }
}

# credit_tib$credit_roles |> standardize_roles()
#' unnest table by credit_roles or credit_names
#' unnest_by Unquoted name of the column to be unnested
#' sep String with the symbol for the list separator, most often ",".
#' @importFrom rlang `:=`
#' @noRd
unnest_contrib <- function(tib, unnest_by, sep) {
#
  # if (any(stringr::str_detect(tib$credit_roles, sep) |
  #         stringr::str_detect(tib$credit_roles, "(?<!review) (and|&) "))) {
  #   tib <- tib |>
  #     mutate(credit_roles = strsplit(credit_roles, paste0(sep, " (and )?|( (and|&) )"))) |>
  #     unnest(credit_roles)
  # }
  # if (any(stringr::str_detect(tib$credit_names, sep) |
  #         stringr::str_detect(tib$credit_names, "(?<!review) (and|&) "))) {
  #   tib <- tib |>
  #     mutate(credit_names = strsplit(credit_names, paste0(sep, " (and )?|( (and|&) )"))) |>
  #     unnest(credit_names)
  # }



  if (any(stringr::str_detect(tib[[rlang::as_name(rlang::enquo(unnest_by))]], sep) |
          stringr::str_detect(tib[[rlang::as_name(rlang::enquo(unnest_by))]], "(?<!review) (and|&) "))) {


    tib <- tib |>
      dplyr::mutate({{unnest_by}} := strsplit({{ unnest_by }}, paste0(sep, " (and )?|( (and|&) )"))) |>
      tidyr::unnest({{unnest_by}}) |>
      # dplyr::mutate(credit_names = stringr::str_remove(credit_names, "and ")) |>
      dplyr::arrange(.data$credit_roles, .data$credit_names)

  }

  return(tib)

}

#' Standardize the names of a single CRediT roles
#' role_str String value with an unstandardized CRediT role
#' @noRd
which_role <- function(role_str, regexes = NULL) {

  stopifnot("which_role takes only a single string as input and you supplied a vector" =
              length(role_str) == 1)

  if (is.null(regexes)) {
    keyword_list <- .create_keyword_list()
    regexes <- keyword_list[names(keyword_list) %in%
                            c("conceptualization",
                              "formal analysis",
                              "project administration",
                              "supervision",
                              "investigation",
                              "validation",
                              "visualization",
                              "writing - original draft",
                              "writing - review and editing")]
  }

  hits <- purrr::map_lgl(regexes, \(x)
                         stringr::str_detect(role_str, stringr::regex(x, ignore_case = TRUE)))
  std_name <- regexes[hits] |>
    names()

  if (rlang::is_empty(std_name)) {
    std_name <- role_str
  }

  return(std_name)
}

#' Standardize the names of a vector of CRediT roles
#' str_vec is a String vector with unstandardized CRediT roles
#' @noRd
standardize_roles <- function(str_vec) {
  keyword_list <- .create_keyword_list()

  roles <- keyword_list[names(keyword_list) %in%
                          c("conceptualization",
                            "formal analysis",
                            "project administration",
                            "supervision",
                            "investigation",
                            "validation",
                            "visualization",
                            "writing - original draft",
                            "writing - review and editing")]


  # p <- progressr::progressor(along = str_vec)
  purrr::map_chr(str_vec, \(x) {
    # p()
    which_role(x, regexes = roles)
  })

}

#' remove sentences corresponding to a regex
#' @noRd
remove_sentences_regex <- function(str_vec, regex_str) {
  str_vec[!stringr::str_detect(str_vec, stringr::regex(regex_str, ignore_case = TRUE))]
}
