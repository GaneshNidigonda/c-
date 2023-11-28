# Load necessary packages 
library(ISLR2)
library(pROC)
library(ggplot2) 
library(e1071)
library(plotmo) 
library(plotly)
# Display column names of the Khan dataset 
names(Khan)
# Check dimensions of training and testing sets 
dim(Khan$xtrain)
dim(Khan$xtest)
# Check the length of training and testing labels
length(Khan$ytrain)
length(Khan$ytest)
# Display class distribution in training and testing labels table(Khan$ytrain)
table(Khan$ytest)
# Create a dataframe 'dat' with training features and labels 
dat=data.frame(x = Khan$xtrain,y = as.factor(Khan$ytrain))
# Train an SVM model with linear kernel
out <- svm(y ~ ., data = dat, kernel = "linear", cost = 10) 
# Display summary of the trained SVM model
summary(out)
# Compare the fitted values with actual labels in the training data
table(out$fitted, dat$y)
# Create a dataframe 'dat.te' with testing features and labels  
dat.te <- data.frame(x = Khan$xtest, y = as.factor(Khan$ytest)) 
# Predict using the trained SVM model on the testing data 
pred.te <- predict(out, newdata = dat.te)
# Compare the predicted values with actual labels in the testing data 
table(pred.te, dat.te$y)
# Confusion matrix for training data 
train_pred <- predict(out, newdata = dat) 
train_conf_matrix <- table(train_pred, dat$y)
# Convert the confusion matrix to a dataframe 
train_conf_df <- as.data.frame(as.table(train_conf_matrix)) 
# Rename the columns for better readability
colnames(train_conf_df) <- c("Predicted", "Actual", "Frequency") 
# Create the heatmap
ggplot(train_conf_df, aes(x = Predicted, y = Actual, fill = Frequency)) + geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Predicted Class", y = "Actual Class", fill = "Frequency") + theme_minimal()
# Convert factor levels to numeric (1 or 2) 
numeric_pred <- as.numeric(train_pred)
for (i in 2:length(train_roc$rocs)) {
  lines(train_roc$rocs[[i]], col = i, print.auc = TRUE, auc.polygon = TRUE)} 
# Add a legend
legend("bottomright", legend = levels(dat$y), col = 1:length(train_roc$rocs), lwd = 2) 
# Calculate performance metrics for training data
train_pred <- predict(out, newdata = dat) 
train_conf_matrix <- table(train_pred, dat$y) 
# Calculate accuracy for training data
train_accuracy <- sum(diag(train_conf_matrix)) / sum(train_conf_matrix) 
# Calculate precision, recall, and F1-score for each class in training data 
class_names <- levels(dat$y)
train_metrics <- data.frame(Class = character(), Precision = numeric(), Recall = numeric(), F1_Score = numeric(), stringsAsFactors = FALSE)
for (class_name in class_names) {
  tp <- test_conf_matrix[class_name, class_name] fp <- sum(test_conf_matrix[class_name, ]) - tp fn <- sum(test_conf_matrix[, class_name]) - tp precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  test_metrics <- rbind(test_metrics, data.frame(Class = class_name, Precision = precision, Recall = recall, F1_Score = f1_score))}
# Calculate overall classification accuracy for testing data 
test_overall_accuracy <- mean(test_metrics$F1_Score)
# Print performance metrics for both training and testing data 
cat("Performance Metrics for Training Data:\n")
cat("Overall Accuracy:", train_accuracy, "\n") 
print(train_metrics)
cat("Overall Classification Accuracy (F1-Score):", train_overall_accuracy, "\n\n") 
cat("Performance Metrics for Testing Data:\n")
cat("Overall Accuracy:", test_accuracy, "\n") p
rint(test_metrics)
cat("Overall Classification Accuracy (F1-Score):", test_overall_accuracy, "\n")
for (class_name in class_names) { 
  tp <- train_conf_matrix[class_name, class_name] 
  fp <- sum(train_conf_matrix[class_name, ]) - tp 
  fn <- sum(train_conf_matrix[, class_name]) - tp 
  precision <- tp / (tp + fp) 
  recall <- tp / (tp + fn) 
  f1_score <- 2 * (precision * recall) / (precision + recall) 
  train_metrics <- rbind(train_metrics, data.frame(Class = class_name, Precision = precision, 
                                                   Recall = recall, F1_Score = f1_score))} 
# Calculate overall classification accuracy for training data 
train_overall_accuracy <- mean(train_metrics$F1_Score) 
# Calculate performance metrics for testing data 
test_conf_matrix <- table(pred.te, dat.te$y) 
# Calculate accuracy for testing data 
test_accuracy <-sum(diag(test_conf_matrix))/sum(test_conf_matrix) 
# Calculate precision, recall, and F1-score for each class in testing data 
test_metrics <- data.frame(Class = character(), Precision = numeric(), Recall = numeric(), 
                           F1_Score = numeric(), stringsAsFactors = FALSE
                           # Create a dataframe for testing metrics 
                           test_metrics_df <- data.frame(Class = test_metrics$Class, 
                                                         Precision = test_metrics$Precision, 
                                                         Recall = test_metrics$Recall, 
                                                         F1_Score = test_metrics$F1_Score) 
                           # Create a bar plot for testing metrics 
                           ggplot(test_metrics_df, aes(x = Class, y = F1_Score, fill = Class)) + 
                             geom_bar(stat = "identity", position = "dodge") + 
                             labs( 
                               title = "Performance Metricsfor Testing Data", 
                               y = "F1-Score", 
                               x = "Class" 
                             ) + 
                             theme_minimal() + 
                             theme(axis.text.x = element_text(angle = 45, hjust = 1))
                           
                           