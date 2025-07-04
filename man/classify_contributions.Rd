% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classifiers.R
\name{classify_contributions}
\alias{classify_contributions}
\title{Classify statements for CRediT, narrative, and authorship criteria.}
\usage{
classify_contributions(tib, key_col, statement_col)
}
\arguments{
\item{tib}{Tibble with extracted contribution statements with the
\code{extract_contributions} function.}

\item{key_col}{Unquoted expression with the name of the key column of the input
tibble, usually \code{doi} or \code{file_name}.}

\item{statement_col}{Unquoted expression with the name of the column that
contains the authorship statements to be classified.}
}
\value{
Tibble with one row per screened document and logical values for
credit_estimate (whether or not the statement follows CREdiT),
contrib_estimate (whether or not author roles are listed at all),
narrative_estimate (whether or not author roles are given with sentences
containing verbs), and authorship_estimate (whether or not a statement about
the authors fulfilling authorship criteria is included).
}
\description{
Classify statements for CRediT, narrative, and authorship criteria.
}
\examples{
\dontrun{
extracted_sections <- extract_contributions(oddpub::pdf_load("examples/"))
contribution_results <- classify_contributions(extracted_sections)
}

}
