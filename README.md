# ContriBOT
Detection and extraction of Author Contribution statements and Acknowledgements from scientific papers

[![License: AGPL v3](https://img.shields.io/badge/License-AGPL_v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)


ContriBOT is a text mining algorithm that parses a set of publications and detects
which publications included Authorship Statements, Acknowledgements and ORCIDs.


## Authors

Vladislav Nachev (vladislav.nachev@charite.de), Fyeqa Akram - BIH QUEST Center for Responsible Research

## Installation

The latest version of the algorithm is structured as an R package and can easily be installed with the following command:

``` r
# install.packages("devtools") # if devtools currently not installed
devtools::install_github("quest-bih/ContriBOT")
```

## Description

ContriBOT is a tool that searches for typical section headings preceded by
a section tag, in order to extract the author contribution, acknowledgement,
and ORCID sections. The extracted author contribution section can then be
further screened and categorized, for example for following the CRediT taxonomy,
or including actual author contributions.


## Usage

The package relies on PDF processing functions from ODDPub, a text-mining tool
that parses a set of publications and detects which publications include
statements about Open Data or Open Code. The first step in the usual workflow
is the conversion of a folder of PDF files to txt file format:

``` r
library(oddpub)
library(furrr) # for parallel processing
library(progressr) # for visualizing computational progress

plan(multisession) # general setting for parallel processing
handlers(global = TRUE) # general setting for progress visualization

pdf_folder <- "/path/to/input/pdf/files"
txt_folder <- "/path/to/output/txt/files/"
conversion_success <- oddpub::pdf_convert(pdf_folder, txt_folder)

list.files(pdf_folder)[!conversion_success] # examine if any files failed to convert
```

Converts PDFs contained in one folder to txt-files and saves them into the output txt folder.
This conversion does not aim to parse the whole pdf cleanly, but to correctly detect
the column layout and to (liberally) add section tags at the beginning of article sections,
including author contribution statements and acknowledgements to enable their detection
and extraction. This is a crucial step in the workflow and conversion via other methods
might not work at all or result in poorer performance.
A convention that will further enhance the PDF to txt conversion is to name the PDF files
as the DOI of the publication, with slashes "/" replaced by plus signs "+",
e.g. 10.1371+journal.pone.0302787.pdf. The txt file names are the same as the input PDF,
except for the file extension. The DOI information in the file name improves
the detection of the column layout of the input PDF file and therefore the quality of
the txt output and the remaining ContriBOT detection algorithms.

After ensuring that all files were correctly converted (or corrupted PDF files were
replaced or removed), the txt files can be loaded into memory for further processing:

```r
text_corpus <- oddpub::pdf_load(txt_folder, lowercase = FALSE, remove_regex = NULL)
```

Then the text corpus can be screened and the relevant sections can be extracted from
each txt file:

```r
contrib_extractions <- extract_contributions(text_corpus)
```

Finally, the extracted text of the author contribution section can be classified
and categorized with the following output variables:  

 - credit_estimate: whether or not the statement follows CREdiT, operationalized as
 listing at least any three CRediT roles and no non-CRediT roles, or listing 
 no non-CRediT roles and at least any two of the following CRediT roles:
 Conceptualization, Investigation, Methodology, Writing – original draft,
 and Writing – review & editing. Blanket statements about all authors are not considered.  
 - contrib_estimate: whether the statement contains at least one author role and is not
 a blanket statement about all authors.  
 - narrative_estimate: whether the author contributions (CRediT or non-CRediT) are
 written out as full sentences with verbs in the past tense such as 
 "Author X.Y. conceptualized and supervised the study."  
 - authorship_estimate: whether the statement includes a sentence about authors 
 qualifying for authorship, fulfilling authorship criteria, or taking responsibility for
 the text or data analysis.  

```r
credit_results <- classify_contributions(contrib_extractions, article, contrib_statement) |>
  # for convenience, the DOI can be obtained from the article column
  mutate(doi = stringr::str_replace_all(article, "\\+", "\\/") |> 
  stringr::str_remove(".txt"))
```

## License

ContriBOT is available under the APL-v3 license.
See the [LICENSE](https://github.com/quest-bih/ContriBOT/blob/master/LICENSE) file for more
information.
