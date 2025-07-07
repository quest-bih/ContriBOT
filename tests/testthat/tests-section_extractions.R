keywords <- .create_keyword_list()

contrib1 <- c("Irrelevant text.",
              "more of that.",
              "<section> Author contributions All authors contributed to the study conception
 and design.",
              "Author1 contributed to the ethical application and
 the statistical design.",
              "Author2 and Author3 wrote the
 manuscript and all authors commented on previous versions of the
 manuscript.",
              "All authors read and approved the final manuscript.",
              "<section> Funding Some funding Info here.",
              "The authors declare that they received no funds, grants
 or other support during the preparation of this manuscript.")

contrib2 <- c("not relevant",
              "<section> Acknowledgements We gratefully acknowledge the work of someone important.",
              "<section> Contributors All authors meet the National Health and Medical Research Council
 authorship requirements.",
              "AB and CD contributed equally to the first authorship.",
              "GH and IK contributed equally to the last authorship. AB, CD and GH initiated
              the collaborative project and designed it.",
              "AB and OP mainly defined inclusion and exclusion criteria.",
              "<section> MN, OP and QR provided essential methodological support.",
              "AB, EF, IK and OP contributed mainly to the first draft of the manuscript.",
              "GH and IK revised the manuscript after the initial submission with the support
              from AB, CD, OP, QR, MN and EF.",
              "AB, CD, EF, GH, IK, MN, OP and QR read and approved the every submitted version of the manuscript.",
              "GH act as guarantor.",
              "<section> Funding This research was partly supported by one foundation and party by another",
              "<section> Patient and public involvement Patients and/or the public were not involved in
 the design, or conduct, or reporting, or dissemination plans of this research.",
              "<section> ORCID",
              "<section> iDs",
              "AB http://orcid.org/0000-0000-0000-0000",
              "GH http://orcid.org/0000-0000-0000-000X",
              "<section> REFERENCES"
              )

contrib3 <- c("<section> Author Contributions:",
              "Conceptualization, A.A.A.; writing—original draft preparation, A.A.A. and D.D.D.;
              writing—review and editing, A.A.A.; funding acquisition, A.A.A.",
              "All authors have read and agreed to the published version of the manuscript.",
              "<section> Funding: This work was supported by Funder XYZ Grant Nr XXXXX.",
              "<section> Institutional Review Board Statement: Not applicable.",
              "<section> Informed Consent Statement: Not applicable."
              )


# contrib3 <- c("## consent for publication",
#           "## not applicable.",
#           "## availability of data and materials",
#           "## the datasets used in this study are available from the corresponding author on reasonable request.",
#           "## competing interests",
#           "## the authors declare no conflicts of interest."
# )
contrib_wiley <- c("<section> AU T H O R C O N T R I B U T I O N S",
                   "So and so did something very specific.",
                   "Another did something pretty banal.",
                   "Some other did something that was interrupted by",
                   "<insert> F I G U R E 1 0",
                   "figure stuff here",
                   "more figure stuff",
                   "<iend>",
                   "<insert> F I G U R E 1 1",
                   "figure stuff here",
                   "more figure stuff",
                   "<iend>",
                   "<insert> F I G U R E 1 2",
                   "figure stuff here",
                   "more figure stuff",
                   "<iend>",
                   "figures but continued here onwards.",
                   "All authors provided critical feedback, shaped the research,
                   and wrote the final manuscript.",
                   "<section> AC K N OW L E D G M E N T S",
                   "and here there is info on individuals being acknowledged, as well as grant numbers, etc.",
                   "<section> C O N F L I C T O F I N T E R E S T S TAT E M E N T",
                   "something or other about conflicts of interest.",
                   "<section> DATA AVA I L A B I L I T Y S TAT E M E N T",
                   "the data that support the findings of this study are available from the corresponding author upon reasonable request.",
                   "<section> ORCID",
                   "Name of Author",
                   "https://orcid.org/0000-0000-0000-0000",
                   "<section> REFERENCES")

contrib_plos <- c("<section> Supporting information",
                  "<section> Appendix 1 Some details on some appendix here.",
                  "<section> Acknowledgments",
                  "<section> We thank Dr. Who and Dr. Doolittle for their awesomeness.",
                  "<section> Author Contributions",
                  "<section> Conceptualization: Name1, Name2, Name3.",
                  "<section> Data curation: Name1, Name4.",
                  "<section> Formal analysis: Name2.",
                  "<section> Investigation: Name1, Name3.",
                  "<section> Methodology: Name1, Name3.",
                  "<section> Project administration: Name3.",
                  "<section> Resources: Name3, Name4, Name5.",
                  "<section> Software: Name1.",
                  "<section> Supervision: Name4, Name5.",
                  "<section> Validation: Name1, Name4, Name5.",
                  "<section> Visualization: Name1, Name2.",
                  "<section> Writing – original draft: Name1.",
                  "<section> Writing – review & editing: Name1, Name2, Name3, Name4, Name5.",
                  "<section> References"
)

contrib_table <- c("Irrelevant stuff",
                       "<insert> Table x of some sort",
                       "table stuff here",
                       "<iend>",
                       "<insert> Appendix Authors Name Location Contribution Author1,
                       MD Medical University of Atlantis, Atlantis
                       Acquisition of data, statistical analysis, execution, and interpretation of data
                       Author2, MSc Medical University of Pluto, Pluto
                       Acquisition of data, execution, interpretation of data, and critical review for important intellectual content Continued",
                       "<iend>",
                       "tons of interpolated text",
                       "<insert> Appendix (continued) Name Location Contribution Author3, MD University Children’s Hospital Utopia, Utopia
                       Acquisition of data and critical review for important intellectual content Continued",
                       "<iend>",
                       "<insert> Appendix (continued) Name Location Contribution Author4t, MD Olympos Mons, Elada
                       Conception and design, acquisition of data, execution, interpretation of data, and critical review for important intellectual content")
# pdf_text_sentences <- das_plosata


# das_cell <- c("### b data and code availability",
#               "## supplemental information",
#               "## acknowledgments",
#               "## materials availability",
#               "## this study did not generate new unique reagents.",
#               "## data and code availability all data reported in this paper will be shared by the lead contact upon request.",
#               "## this paper does not report original code.",
#               "## any additional information required to reanalyze the data reported in this paper is available from the lead contact upon request.",
#               "## experimental model and subject details"
# )
#
# das_last_line <- c("line third-to-last",
#                    "line before last",
#                    "<section> data availability statement anonymized data will be made publicly available at a future date."
# )
# das_hyphen <- c("<section> data availability: all data used in these analyses are freely available on a database which can be found at https://multi-hyphenated-",
#                 "<section> words-database.org/home or www.somerepodatabase.org.",
#                 "<section> author contributions: typical contribution statement about who analyzed data.")
# text_sentences <- contrib1

test_that("contribution_extraction", {
  expect_equal(.extract_section(contrib1,
                                keywords$authorship_section,
                                look_in_tables = FALSE) |> length(), 4)
  expect_equal(.extract_section(contrib1,
                                keywords$authorship_section,
                                look_in_tables = TRUE) |> length(), 4)
  expect_equal(.extract_section(contrib2,
                                keywords$authorship_section,
                                look_in_tables = FALSE) |> length(), 9)
  expect_equal(.extract_section(contrib_wiley,
                                keywords$authorship_section,
                                look_in_tables = FALSE) |> length(), 6)
  expect_equal(.extract_section(contrib_plos,
                                keywords$authorship_section,
                                look_in_tables = FALSE) |> length(), 14)
  expect_equal(.extract_section(contrib3,
                                keywords$authorship_section,
                                look_in_tables = FALSE) |> length(), 3)
  expect_equal(.extract_section(contrib_table,
                                keywords$authorship_section,
                                look_in_tables = TRUE) |> length(), 3)
  expect_equal(.extract_section(contrib_table,
                                keywords$authorship_section,
                                look_in_tables = FALSE) |> length(), 1)
})



test_that("orcid_extraction",
          {
            expect_equal(.extract_section(contrib2,
                                          keywords$orcid_section,
                                          look_in_tables = FALSE) |> length(), 4)
            expect_equal(.extract_section(contrib_wiley,
                                          keywords$orcid_section,
                                          look_in_tables = FALSE) |> length(), 3)
          })

