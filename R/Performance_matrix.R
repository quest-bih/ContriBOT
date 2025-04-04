# Read the training data file
Actual_data <- read_excel(here::here("Data", "Author_Contributions.xlsx"), sheet = "Sheet1")


predicted_data <- read_excel(here::here("outputs", "result.xlsx"), sheet = "Sheet1")

# Create a list of label pairs (true labels and predicted labels)
label_pairs <- list(
  list(true = Actual_data$Contributions, predicted = predicted_data$Contributions, label_name = "Author Contributions "),
  list(true = Actual_data$Narrative, predicted = predicted_data$Narrative, label_name = "Narrative"),
  list(true = Actual_data$CRT_Taxonomy, predicted = predicted_data$Credit_Taxonomy, label_name = "CRT_Taxonomy")
  #list(true = test$Auth_criteria, predicted = data$Criteriaship, label_name = "Auth_criteria vs. Criteriaship")
)

# Initialize a data frame to store the results
report_data <- data.frame(
  Label = character(),
  Accuracy = numeric(),
  Precision = numeric(),
  Recall = numeric(),
  F1_Score = numeric(),
  TP = numeric(),
  TN = numeric(),
  FP = numeric(),
  FN = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each label pair to calculate the metrics
for (label_pair in label_pairs) {

  true_vals <- label_pair$true
  predicted_vals <- label_pair$predicted
  label_name <- label_pair$label_name

  # Compute confusion matrix using caret's confusionMatrix function
  cm <- confusionMatrix(as.factor(predicted_vals), as.factor(true_vals))

  # Extract the confusion matrix values
  tp <- cm$table[2, 2]  # True positives
  tn <- cm$table[1, 1]  # True negatives
  fp <- cm$table[1, 2]  # False positives
  fn <- cm$table[2, 1]  # False negatives

  # Calculate the evaluation metrics
  accuracy <- cm$overall['Accuracy']
  precision <- cm$byClass['Pos Pred Value']
  recall <- cm$byClass['Sensitivity']
  f1_score <- cm$byClass['F1']

  # Append the results to the report_data data frame
  report_data <- rbind(report_data, data.frame(
    Label = label_name,
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1_Score = f1_score,
    TP = tp,
    TN = tn,
    FP = fp,
    FN = fn
  ))
}

# Print the report in a nice table format
knitr::kable(report_data, format = "markdown", caption = "Confusion Matrix Summary Report")

# write the report to a text file
writeLines(capture.output(knitr::kable(report_data, format = "markdown")), here("outputs", "Confusion_Matrix_Report.txt"))

