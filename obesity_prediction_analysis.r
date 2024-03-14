# Install the necessary packages
# install.packages("readxl")
# install.packages("writexl")
# install.packages("caret")
# install.packages("GGally")
# install.packages('gridExtra')
# install.packages("dummies")
# install.packages("randomForest")
# install.packages("e1071")
# install.packages("psych")
# install.packages("arules") 
# install.packages("arulesViz") 
# install.packages("readr")

# Import the necessary packages
library(readxl)
library(writexl)
library(caret)
library(Hmisc)
library(dplyr)
library(MASS)
library(GGally)
library(gridExtra)
library(tidyverse)
library(ggplot2)
library(lattice)
library(dummies)
library(randomForest)
library(e1071)
library(psych)
library(arules)
library(arulesViz)
library(readr)

# |============================================================================|
# | Data Collection                                                            |
# |============================================================================|

# Read the data from the Excel file
file_path <- "/Users/ihsa332019/Desktop/Taylor_s Lesson Materials/Data Mining/GroupAssignment/Obesity_Predictive_data.xlsx"
data <- read_excel(file_path)

# Basic summary of the data
print(head(data))

# Explore the information of the data
print(str(data))

# Describe the statistical characteristics of data
describe(data)

# The size of columns and rows of the data
print(dim(data))

# split data into numerical and categorical columns (to facilitate different approaches for each variable type)
numerical_columns <- colnames(data)[c(4, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17)] # include only numerical variables
categorical_columns <- colnames(data)[c(2, 3, 5, 6, 16, 18, 19, 20, 21, 22, 23)] # remove non-numerical columns to test, and target variable
target_column <- colnames(data)[14] 

# |============================================================================|
# | Data Preprocessing                                                         |
# |============================================================================|

# 1. Handling Missing Values

# How many missing values in the data
colSums(is.na(data))

# 2. Standardization
data_standardized <- data
data_standardized[numerical_columns] <- scale(data[numerical_columns])

# Selecting numeric columns for creating boxplot
num_cols <- unlist(lapply(data, is.numeric)) 
df_num <- data[, num_cols]

# Set up the plot dimensions
par(mar = c(5, 5, 4, 2) + 0.1)  # Adjusting margins for better spacing
# Adjust the values inside mar() according to your preference for bottom, left, top, and right margins.

# Create the boxplot with larger size
boxplot(df_num, las = 2, cex.axis = 0.8, cex.lab = 0.8)  
# 'las' is to rotate x-axis labels, and 'cex.axis' and 'cex.lab' are for adjusting text size


# 3. Handling Outliers
for (col in numerical_columns) {
  Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  data[[col]] <- ifelse(data[[col]] < lower_bound | data[[col]] > upper_bound, NA, data[[col]])
}

# fill N/A in numerical features with mean value of each attribute
data$OW = ifelse(is.na(data$OW), ave(data$OW, FUN = function(x) mean(x, na.rm = TRUE)), data$OW)
data$Htension = ifelse(is.na(data$Htension), ave(data$Htension, FUN = function(x) mean(x, na.rm = TRUE)), data$Htension) 
data$HC = ifelse(is.na(data$HC), ave(data$HC, FUN = function(x) mean(x, na.rm = TRUE)), data$HC)
data$Weight = ifelse(is.na(data$Weight), ave(data$Weight, FUN = function(x) mean(x, na.rm = TRUE)), data$Weight)
data$Height = ifelse(is.na(data$Height), ave(data$Height, FUN = function(x) mean(x, na.rm = TRUE)), data$Height)

# Check whether all attributes' missing values are appropriately imputed
colSums(is.na(data))


# |============================================================================|
# | Exploratory Data Analysis (EDA)                                            |
# |============================================================================|

## Descriptive EDA

# Custom function to calculate mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# For Numerical Variables
numerical_stats <- list()

for (col in numerical_columns) {
  num_data <- data[[col]]
  num_stats <- data.frame(
    Variable = col,
    Max = max(num_data, na.rm = TRUE),
    Min = min(num_data, na.rm = TRUE),
    Mean = mean(num_data, na.rm = TRUE),
    Median = median(num_data, na.rm = TRUE),
    Mode = get_mode(num_data),
    Q1 = quantile(num_data, 0.25, na.rm = TRUE),
    Q3 = quantile(num_data, 0.75, na.rm = TRUE),
    SD = sd(num_data, na.rm = TRUE)
  )
  numerical_stats[[col]] <- num_stats
}

# Combine the list of data frames into one
numerical_stats_df <- do.call(rbind, numerical_stats)
rownames(numerical_stats_df) <- NULL  # Reset row names

numerical_stats_df

# Set up plotting area
par(mfrow=c(1,2))

for (col in categorical_columns) {
  cat_data <- data[[col]]
  freq_table <- prop.table(table(cat_data))
  
  # Plot bar chart with larger margins to accommodate labels
  barplot(freq_table, ylab = 'Proportion of participants', cex.names = 0.8, las = 2, mar = c(5, 5, 4, 2) + 0.1)
  title(main = col)
}

data$STRESS[data$STRESS == "yes" | data$STRESS == "YES"] <- "Yes"
## Correlation of Variables

for(col in numerical_columns) {
  # Replace both NA and "NA" with the mean of each column
  data[[col]] <- ifelse(is.na(data[[col]]) | data[[col]] == "NA", 
                        ave(data[[col]], FUN = function(x) mean(x, na.rm = TRUE)), 
                        data[[col]])
}
colSums(is.na(data))

# Calculate standard deviations for each numerical column
std_deviations <- apply(data[numerical_columns], 2, sd)

# Identify columns with zero or very low standard deviations
zero_variability_columns <- names(std_deviations[std_deviations == 0])

# Print the names of columns with zero or very low variability
print(zero_variability_columns)

# Create a new vector without the specified columns
new_numerical_columns <- setdiff(numerical_columns, c("OW", "Htension", "HC"))

# Calculate the correlation matrix with the updated numerical columns
correlation_matrix <- cor(data[c(new_numerical_columns, target_column)])
pairs(data[, new_numerical_columns], col = data$HD)

?barplot
duplicated_rows <- data[duplicated(data$Race), ]
unique_data <- data[!duplicated(data$Race), ]
barplot(HD ~ Race, data = unique_data)

#BY RACE
count_hd <- with(data, table(Race, HD))

# Create barplot
barplot(count_hd, beside = TRUE, main = "Count of Heart Disease by Race",
        xlab = "Race", ylab = "Count", col = c("skyblue", "salmon"),
        legend = rownames(count_hd))

#BY SMOKING
percentage_hd <- with(data, prop.table(table(smoking, HD), margin = 1)) * 100

# Create barplot
barplot(percentage_hd, beside = TRUE, main = "Count of Heart Disease by Smoking",
        xlab = "Smoking", ylab = "Count", col = c("skyblue", "salmon"),
        legend = rownames(percentage_hd))

# Calculate the percentage of individuals with heart disease (HD) values 0 and 1 for each race category
percentage_hd <- with(data, prop.table(table(Race, HD), margin = 1)) * 100

# Create barplot
barplot(percentage_hd, beside = TRUE, main = "Percentage of Heart Disease by Race",
        xlab = "Race", ylab = "Percentage", col = c("skyblue", "salmon"))
legend("bottomright", legend = rownames(percentage_hd), fill = c("skyblue", "salmon"))

#'*HD_Race*
# Generate 'value' variable
value <- abs(rnorm(nrow(data), 0, 15))

# Create ggplot
HD_Race <- ggplot(data, aes(x = factor(HD), y = value, fill = Race)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete(labels = c("0", "1"))

# Display the plot
HD_Race

#'*HD_Gender*
# Generate 'value' variable
value <- abs(rnorm(nrow(data), 0, 15))

# Create ggplot
HD_Gender <- ggplot(data, aes(x = factor(HD), y = value, fill = Gender)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete(labels = c("0", "1"))

# Display the plot
HD_Gender

#'*HD_Smoke*
# Generate 'value' variable
value <- abs(rnorm(nrow(data), 0, 15))

# Create ggplot
HD_Smoke <- ggplot(data, aes(x = factor(HD), y = value, fill = factor(smoking))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete(labels = c("0", "1"))

# Display the plot
HD_Smoke

#'*HD_Diabetes*
# Generate 'value' variable
value <- abs(rnorm(nrow(data), 0, 15))

# Create ggplot
HD_Diabetes <- ggplot(data, aes(x = factor(HD), y = value, fill = factor(Diabetes))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete(labels = c("0", "1"))

# Display the plot
HD_Diabetes

colnames(data)

#'*HD_proc*
# Generate 'value' variable
value <- abs(rnorm(nrow(data), 0, 15))

# Create ggplot
HD_proc <- ggplot(data, aes(x = factor(HD), y = value, fill = data$`PROC FOOD`)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete(labels = c("0", "1"))

# Display the plot
HD_proc

#'*HD_stress*
# Generate 'value' variable
value <- abs(rnorm(nrow(data), 0, 15))

# Create ggplot
HD_stress <- ggplot(data, aes(x = factor(HD), y = value, fill = data$`STRESS`)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete(labels = c("0", "1"))

# Display the plot
HD_stress

#'*HD_sleep*
# Generate 'value' variable
value <- abs(rnorm(nrow(data), 0, 15))

# Create ggplot
HD_sleep <- ggplot(data, aes(x = factor(HD), y = value, fill = data$`SLEEP > 8`)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete(labels = c("0", "1"))

# Display the plot
HD_sleep

#'*HD_History*
# Generate 'value' variable
value <- abs(rnorm(nrow(data), 0, 15))

# Create ggplot
HD_History <- ggplot(data, aes(x = factor(HD), y = value, fill = data$`FAMILY HISTORY( Have any of your parents or siblings encountered the following problems?)`)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete(labels = c("0", "1"))

# Display the plot
HD_History

#'*HD_BMI*

# Create ggplot
HD_BMI <- ggplot(data, aes(x = factor(HD), y = BMI, fill = factor(HD))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  scale_x_discrete(labels = c("0", "1"))

HD_BMI

#'*HD_Age*

# Create ggplot
HD_Age <- ggplot(data, aes(x = factor(HD), y = Age, fill = factor(HD))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  scale_x_discrete(labels = c("0", "1"))

# Display the plot
HD_Age

# Shapiro-Wilk Normality Test for checking normality of continuous data
shapiro_test <- shapiro.test(data$Weight)
print(shapiro_test)
shapiro_test <- shapiro.test(data$Height)
print(shapiro_test)

# Create a pairplot with 'HD' as the color dimension
ggpairs(data[, new_numerical_columns], aes(colour = as.factor(HD)))

# Remove duplicate column names
colnames(correlation_matrix) <- make.unique(colnames(correlation_matrix))

# Plot heatmap
ggplot(data = reshape2::melt(correlation_matrix), aes(x=Var1, y=Var2)) +
  geom_tile(aes(fill=value), color='white') +
  geom_text(aes(label=round(value, 2)), size=3) +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1), space="Lab", name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, vjust=1, size=10, hjust=1),
        axis.text.y = element_text(size=10)) +
  coord_fixed()

# chi-squared test for categorical columns
chi_squared_results <- data.frame(
  Column = character(), Chi_Squared = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)
for (col in categorical_columns) {
  contingency_table <- table(data[[col]], data[[target_column]])
  test_result <- chisq.test(contingency_table)
  chi_squared_results <- rbind(chi_squared_results, data.frame(
    Column = col, Chi_Squared = test_result$statistic, P_Value = test_result$p.value))
}
chi_squared_results

# |============================================================================|
# | Feature Engineering                                                        |
# |============================================================================|

# Add the target column to the list of categorical columns, ensuring it is also a factor
data$HD <- as.factor(data$HD)

# Ensure all columns are now factors (including the newly added target column)
categorical_data[] <- lapply(categorical_data, factor)

# Convert the updated categorical data to transactions
transactions_data <- as(categorical_data, "transactions")

# Setting seed for reproducibility
set.seed(220)

# Run the apriori algorithm
associa_rules <- apriori(data = transactions_data, 
                         parameter = list(support = 0.15, confidence = 0.7, maxlen = 15))

# Prune rules by setting a minimum threshold for support, confidence, and count
pruned_rules <- subset(associa_rules, support > 0.15 & confidence > 0.7 & count > 15)

# Keep rules which contain "HD" as rhs and lift value more than 1.35
rules_with_HD <- subset(pruned_rules, subset=rhs %pin% 'HD')
rules_with_HD <- subset(rules_with_HD, subset = lift > 1.35)

# Inspect rules contain "HD" in rhs
inspect(rules_with_HD)

# |============================================================================|
# | Model Training                                                             |
# |============================================================================|

# Determine numerical and categorical features that are crucial in model training, 
# based on the best association rules
numerical <- data[c(4, 15)]
categorical <- data[c(5, 6, 16, 21)]

# Standardize Numerical Variables
data_standardized <- as.data.frame(lapply(numerical, scale))

# Label Encoding Categorical Variables
data_categorical <- data[]
data_categorical <- as.data.frame(lapply(categorical, function(x) as.numeric(as.factor(x))))

# Make sure the target column is a factor
data[[target_column]] <- as.factor(data[[target_column]])

# Combine Numerical Data, Encoded Categorical Data, and Target Variable
combined_data <- cbind(data_standardized, data_categorical, Target = data[,target_column])

# Split into Training and Testing Sets
set.seed(123)
splitIndex <- createDataPartition(combined_data[,target_column], p = .80, list = FALSE, times = 1)
trainData <- combined_data[splitIndex,]
trainData[[target_column]] <- as.factor(trainData[[target_column]])
testData <- combined_data[-splitIndex,]
testData[[target_column]] <- as.factor(testData[[target_column]])

# Logistic Regression Training and Prediction
logitModel <- glm(as.formula(paste(target_column, "~ .")), data = trainData, family = "binomial")
logitPred <- predict(logitModel, testData, type = "response")
logitPred <- ifelse(logitPred > 0.5, 1, 0)

# Naive Bayes Training and Prediction
nbModel <- naiveBayes(HD ~ ., data = trainData)
nbPred <- predict(nbModel, testData)

# Random Forest Training and Prediction
rfModel <- randomForest(HD ~ ., data = trainData)
rfPred <- predict(rfModel, testData)

# |============================================================================|
# | Model Evaluation                                                           |
# |============================================================================|

# Logistic Regression Evaluation with Confusion Matrix
confusionMatrix(factor(logitPred), factor(testData$HD))

# Naive Bayes Evaluation with Confusion Matrix
confusionMatrix(factor(nbPred), factor(testData$HD))

# Random Forest Evaluation with Confusion Matrix
confusionMatrix(factor(rfPred), factor(testData$HD))

# |============================================================================|
# | Model Training with Feature Importance                                     |
# |============================================================================|

# Implement feature importance to determine which attributes to adopt in model training
set.seed(100)
rrfMod <- train(HD ~ ., data=data, method="RRF")
rrfImp <- varImp(rrfMod, scale=F)
rrfImp

# Determine numerical and categorical features that are crucial in model training, 
# based on the result outcome of feature importance execution
numerical_fi <- data[c(8)]
categorical_fi <- data[c(5, 6, 16, 18, 24)]

# Standardize Numerical Variables
data_standardized_fi <- as.data.frame(lapply(numerical_fi, scale))

# Label Encoding Categorical Variables
data_categorical_fi <- data[]
data_categorical_fi <- as.data.frame(lapply(categorical_fi, function(x) as.numeric(as.factor(x))))

# Make sure the target column is a factor
data[[target_column]] <- as.factor(data[[target_column]])

# Combine Numerical Data, Encoded Categorical Data, and Target Variable
combined_data_fi <- cbind(data_standardized_fi, data_categorical, Target = data[,target_column])

# Split into Training and Testing Sets
set.seed(123)
splitIndex_fi <- createDataPartition(combined_data_fi[,target_column], p = .80, list = FALSE, times = 1)
trainData_fi <- combined_data_fi[splitIndex,]
trainData_fi[[target_column]] <- as.factor(trainData_fi[[target_column]])
testData_fi <- combined_data_fi[-splitIndex,]
testData_fi[[target_column]] <- as.factor(testData_fi[[target_column]])

# Logistic Regression Training and Prediction
logitModel_fi <- glm(as.formula(paste(target_column, "~ .")), data = trainData_fi, family = "binomial")
logitPred_fi <- predict(logitModel_fi, testData_fi, type = "response")
logitPred_fi <- ifelse(logitPred_fi > 0.5, 1, 0)

# Gaussian Naive Bayes Training and Prediction
nbModel_fi <- naiveBayes(HD ~ ., data = trainData_fi)
nbPred_fi <- predict(nbModel_fi, testData_fi)

# Random Forest Training and Prediction
rfModel_fi <- randomForest(HD ~ ., data = trainData_fi)
rfPred_fi <- predict(rfModel_fi, testData_fi)

# |============================================================================|
# | Model Evaluation                                                           |
# |============================================================================|

# Logistic Regression Evaluation with Confusion Matrix
confusionMatrix(factor(logitPred_fi), factor(testData_fi$HD))

# Naive Bayes Evaluation with Confusion Matrix
confusionMatrix(factor(nbPred_fi), factor(testData_fi$HD))

# Random Forest Evaluation with Confusion Matrix
confusionMatrix(factor(rfPred_fi), factor(testData_fi$HD))

# |============================================================================|
# | Model Training with all attributes                                         |
# |============================================================================|

# Include all features in dataset (Except for id column which is column 1 in initial dataset)
numerical_features <- data[c(4, 7, 8, 9, 10, 11, 12, 13, 15, 17)]
categorical_features <- data[c(2, 3, 5, 6, 16, 18, 19, 20, 21, 22, 23, 24)] 
target_column <- colnames(data)[14] 

# Standardize Numerical Variables
data_standardized_all <- as.data.frame(lapply(numerical_features, scale))

# Label Encoding Categorical Variables
data_categorical_all <- data[]
data_categorical_all <- as.data.frame(lapply(categorical_features, function(x) as.numeric(as.factor(x))))

# Make sure the target column is a factor
data[[target_column]] <- as.factor(data[[target_column]])

# Combine Numerical Data, Encoded Categorical Data, and Target Variable
combined_data_all <- cbind(data_standardized_all, data_categorical_all, Target = data[,target_column])

# Split into Training and Testing Sets
set.seed(123)
splitIndex_all <- createDataPartition(combined_data_all[,target_column], p = .80, list = FALSE, times = 1)
trainData_all <- combined_data_all[splitIndex_all,]
trainData_all[[target_column]] <- as.factor(trainData_all[[target_column]])
testData_all <- combined_data_all[-splitIndex_all,]
testData_all[[target_column]] <- as.factor(testData_all[[target_column]])

# Check for any columns with no variation (all values are the same) in training and test data
no_variation_cols_trainD <- sapply(trainData_all, function(x) length(unique(x)) == 1)
no_variation_cols_testD <- sapply(testData_all, function(x) length(unique(x)) == 1)

# Identify columns with no variation in either training or test data
no_variation_cols <- no_variation_cols_trainD | no_variation_cols_testD

# Print columns with no variation
print(names(no_variation_cols[no_variation_cols]))

# Remove columns with no variation from both training and test data
trainData_all <- trainData_all[, !no_variation_cols]
testData_all <- testData_all[, !no_variation_cols]

# Logistic Regression Training and Prediction
logitModel_all <- glm(as.formula(paste(target_column, "~ .")), data = trainData_all, family = "binomial")
logitPred_all <- predict(logitModel_all, testData_all, type = "response")
logitPred_all <- ifelse(logitPred_all > 0.5, 1, 0)

# Naive Bayes Training and Prediction
nbModel_all <- naiveBayes(HD ~ ., data = trainData_all)
nbPred_all <- predict(nbModel_all, testData_all)

# Random Forest Training and Prediction
rfModel_all <- randomForest(HD ~ ., data = trainData_all)
rfPred_all <- predict(rfModel_all, testData_all)

# |============================================================================|
# | Model Evaluation                                                           |
# |============================================================================|

# Logistic Regression Evaluation with Confusion Matrix
confusionMatrix(factor(logitPred_all), factor(testData_all$HD))

# Naive Bayes Evaluation with Confusion Matrix
confusionMatrix(factor(nbPred_all), factor(testData_all$HD))

# Random Forest Training and Prediction
confusionMatrix(factor(rfPred_all), factor(testData_all$HD))