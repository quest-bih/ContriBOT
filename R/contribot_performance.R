library(tidyverse)
library(readxl)
library(here)
library(yardstick)

gold_set <- read_excel(here("inst", "extdata",  "Author_Contributions.xlsx"), sheet = "Sheet1")

qa_performance <- classify_contributions(gold_set, doi, Statement_Text) |>
  mutate(
    credit_truth = factor(as.logical(CRT_Taxonomy, na.rm = TRUE), levels = c(TRUE, FALSE)),
    contrib_truth = factor(as.logical(Contributions, na.rm = TRUE), levels = c(TRUE, FALSE)),
    narrative_truth = factor(as.logical(Narrative, na.rm = TRUE), levels = c(TRUE, FALSE)),
    Authorship_truth = factor(as.logical(Auth_criteria, na.rm = TRUE), levels = c(TRUE, FALSE)))


multi_metric <- metric_set(accuracy, ppv, sensitivity, specificity, npv, f_meas)
multi_metric(qa_performance,
             truth = credit_truth, estimate = credit_estimate)

qa_performance |>
  conf_mat(truth = credit_truth, estimate = credit_estimate) |>
  autoplot(type = "heatmap")

multi_metric(qa_performance,
             truth = narrative_truth, estimate = narrative_estimate)

qa_performance |>
  conf_mat(truth = narrative_truth, estimate = narrative_estimate) |>
  autoplot(type = "heatmap")

multi_metric(qa_performance,
             truth = contrib_truth, estimate = contrib_estimate)

qa_performance |>
  conf_mat(truth = contrib_truth, estimate = contrib_estimate) |>
  autoplot(type = "heatmap")

multi_metric(qa_performance,
             truth = Authorship_truth, estimate = authorship_estimate)

qa_performance |>
  conf_mat(truth = Authorship_truth, estimate = authorship_estimate) |>
  autoplot(type = "heatmap")

