# Load necessary libraries
library(tidyverse)      # For data manipulation
library(lubridate)      # For date manipulation
library(caret)          # For data preprocessing and evaluation
library(Metrics)        # For additional evaluation metrics
library(randomForest)   # For random forest model
library(nnet)           # Alternative neural network implementation
library(glmnet)         # For regularized linear models

# Read the data
data_train <- read.csv("Data_Train.csv", stringsAsFactors = FALSE)

# Function to preprocess data
preprocess_data <- function(data) {
  # Convert date to proper format and extract features
  data$Date_of_Journey <- dmy(data$Date_of_Journey)
  data$Day <- day(data$Date_of_Journey)
  data$Month <- month(data$Date_of_Journey)
  
  # Create binary variables for each airline and make sure column names are valid
  airlines <- model.matrix(~ Airline - 1, data)
  colnames(airlines) <- paste0("Airline_", make.names(colnames(airlines)))
  
  # Create binary variables for source and destination
  source <- model.matrix(~ Source - 1, data)
  colnames(source) <- paste0("Source_", make.names(colnames(source)))
  
  destination <- model.matrix(~ Destination - 1, data)
  colnames(destination) <- paste0("Dest_", make.names(colnames(destination)))
  
  # Process time information
  # Function to convert time to minutes
  convert_time_to_minutes <- function(time_str) {
    parts <- unlist(strsplit(time_str, ":"))
    as.numeric(parts[1]) * 60 + as.numeric(parts[2])
  }
  
  # Extract departure time in minutes
  data$Dep_Time_Minutes <- sapply(data$Dep_Time, convert_time_to_minutes)
  
  # Process duration
  # Function to convert duration to minutes
  convert_duration_to_minutes <- function(duration_str) {
    hours <- 0
    minutes <- 0
    
    # Extract hours and minutes using regex
    hour_match <- regexpr("(\\d+)h", duration_str)
    if (hour_match > 0) {
      hours <- as.numeric(substr(duration_str, hour_match, hour_match + attr(hour_match, "match.length") - 2))
    }
    
    min_match <- regexpr("(\\d+)m", duration_str)
    if (min_match > 0) {
      minutes <- as.numeric(substr(duration_str, min_match, min_match + attr(min_match, "match.length") - 2))
    }
    
    hours * 60 + minutes
  }
  
  data$Duration_Minutes <- sapply(data$Duration, convert_duration_to_minutes)
  
  # Process stops
  data$Total_Stops <- gsub("non-stop", "0", data$Total_Stops)
  data$Total_Stops <- gsub("\\D", "", data$Total_Stops)
  data$Total_Stops <- as.numeric(data$Total_Stops)
  
  # Process Additional_Info
  # Replace "No info" with "No Info" for consistency
  data$Additional_Info <- gsub("No info", "No Info", data$Additional_Info)
  
  # Create binary variables for Additional_Info
  additional_info <- model.matrix(~ Additional_Info - 1, data)
  colnames(additional_info) <- paste0("Info_", make.names(colnames(additional_info)))
  
  # Combine features
  features <- cbind(
    data.frame(
      Day = data$Day,
      Month = data$Month,
      Dep_Time_Minutes = data$Dep_Time_Minutes,
      Duration_Minutes = data$Duration_Minutes,
      Total_Stops = data$Total_Stops
    ),
    airlines, source, destination, additional_info
  )
  
  return(features)
}

# Preprocess the data
cat("Preprocessing data...\n")
all_features <- preprocess_data(data_train)
all_target <- data_train$Price

# Check for missing values and handle them
if(any(is.na(all_features))) {
  cat("Found missing values in features. Imputing...\n")
  # Using median imputation for numeric features
  for(col in colnames(all_features)) {
    if(is.numeric(all_features[[col]])) {
      all_features[[col]][is.na(all_features[[col]])] <- median(all_features[[col]], na.rm = TRUE)
    } else {
      # For non-numeric columns, use the most frequent value
      tab <- table(all_features[[col]])
      all_features[[col]][is.na(all_features[[col]])] <- names(tab)[which.max(tab)]
    }
  }
}

# Create a data frame with features and target
all_data <- cbind(all_features, Price = all_target)

# Set seed for reproducibility
set.seed(123)

# Split data into training (80%) and testing (20%) sets
train_indices <- createDataPartition(all_data$Price, p = 0.8, list = FALSE)
train_data <- all_data[train_indices, ]
test_data <- all_data[-train_indices, ]

# Separate features and target for both sets
train_features <- train_data[, -which(names(train_data) == "Price")]
train_target <- train_data$Price
test_features <- test_data[, -which(names(test_data) == "Price")]
test_target <- test_data$Price

# Feature selection to reduce dimensionality
cat("Performing feature selection...\n")
set.seed(123)
quick_rf <- randomForest(
  x = train_features,
  y = train_target,
  ntree = 50,  # Quick run just for feature selection
  importance = TRUE
)

# Get feature importance
importance_scores <- importance(quick_rf)
feature_importance <- data.frame(
  Feature = rownames(importance_scores),
  Importance = importance_scores[, "%IncMSE"]
)
feature_importance <- feature_importance[order(feature_importance$Importance, decreasing = TRUE), ]

# Select top features (adjust the number based on your needs)
top_n_features <- 15  # Using top 15 most important features
top_features <- as.character(feature_importance$Feature[1:top_n_features])
cat("Selected top", top_n_features, "features for modeling\n")

# Subset data to only include top features
train_features_subset <- train_features[, top_features]
test_features_subset <- test_features[, top_features]

# Normalize the features
preproc_params <- preProcess(train_features_subset, method = c("center", "scale"))
train_features_scaled <- predict(preproc_params, train_features_subset)
test_features_scaled <- predict(preproc_params, test_features_subset)

# Check for any remaining missing values after preprocessing
any_na_train <- any(is.na(train_features_scaled))
any_na_test <- any(is.na(test_features_scaled))

if(any_na_train) {
  cat("Warning: Still have missing values in training data after preprocessing. Imputing...\n")
  train_features_scaled <- as.data.frame(train_features_scaled)
  for(col in colnames(train_features_scaled)) {
    if(sum(is.na(train_features_scaled[[col]])) > 0) {
      train_features_scaled[[col]][is.na(train_features_scaled[[col]])] <- 0
    }
  }
}

if(any_na_test) {
  cat("Warning: Still have missing values in test data after preprocessing. Imputing...\n")
  test_features_scaled <- as.data.frame(test_features_scaled)
  for(col in colnames(test_features_scaled)) {
    if(sum(is.na(test_features_scaled[[col]])) > 0) {
      test_features_scaled[[col]][is.na(test_features_scaled[[col]])] <- 0
    }
  }
}

# Add scaled target for some models
train_scaled_target <- scale(train_target)
train_data_scaled <- cbind(train_features_scaled, Price = train_scaled_target)

# Create control parameters for hyperparameter tuning
control <- trainControl(
  method = "cv",        # Cross-validation
  number = 5,           # 5-fold cross-validation
  verboseIter = TRUE,   # Print progress
  returnResamp = "all", # Save all the resampling results
  savePredictions = "final"
)

# 1. Hyperparameter tuning for Neural Network model
cat("\nHyperparameter tuning for Neural Network model...\n")
set.seed(123)

# Define the grid of parameters to search over for nnet
nn_grid <- expand.grid(
  size = c(3, 5, 8, 10),   # Number of hidden nodes
  decay = c(0.001, 0.01, 0.1, 0.5)   # Weight decay parameter
)

# Train the nnet model with hyperparameter tuning
nn_tune <- train(
  x = as.data.frame(train_features_scaled),
  y = train_target,
  method = "nnet",
  tuneGrid = nn_grid,
  trControl = control,
  maxit = 500,         # Maximum iterations
  linout = TRUE,       # Linear output for regression
  trace = FALSE        # Don't print training progress
)

# Display the best parameters
print(nn_tune$bestTune)
cat("Best Neural Network parameters: size =", nn_tune$bestTune$size, 
    ", decay =", nn_tune$bestTune$decay, "\n")

# Make predictions with the best neural network model
nn_predictions <- predict(nn_tune, as.data.frame(test_features_scaled))

# 2. Random Forest tuning
cat("\nHyperparameter tuning for Random Forest model...\n")
set.seed(123)

# Define the grid of parameters to search over for Random Forest
rf_grid <- expand.grid(
  mtry = c(3, 5, 8, floor(sqrt(ncol(train_features_subset))))
)

# Train the Random Forest model with hyperparameter tuning
rf_tune <- train(
  x = train_features_subset,
  y = train_target,
  method = "rf",
  tuneGrid = rf_grid,
  trControl = control,
  importance = TRUE,
  ntree = 200
)

# Display the best parameter
print(rf_tune$bestTune)
cat("Best Random Forest mtry parameter:", rf_tune$bestTune$mtry, "\n")

# Make predictions with the best Random Forest model
rf_predictions <- predict(rf_tune, test_features_subset)

# 3. Regularized Linear Models (elastic net)
cat("\nHyperparameter tuning for Elastic Net Linear Model...\n")
set.seed(123)

# Define the grid of parameters to search over for elastic net
glmnet_grid <- expand.grid(
  alpha = c(0, 0.25, 0.5, 0.75, 1),    # Mixing parameter (0 = ridge, 1 = lasso)
  lambda = 10^seq(-3, 1, length = 20)  # Regularization parameter
)

# Train the elastic net model with hyperparameter tuning
lm_tune <- train(
  x = as.matrix(train_features_scaled),
  y = train_target,
  method = "glmnet",
  tuneGrid = glmnet_grid,
  trControl = control
)

# Display the best parameters
print(lm_tune$bestTune)
cat("Best Elastic Net parameters: alpha =", lm_tune$bestTune$alpha, 
    ", lambda =", lm_tune$bestTune$lambda, "\n")

# Make predictions with the best elastic net model
lm_predictions <- predict(lm_tune, as.matrix(test_features_scaled))

# Evaluate neural network performance
nn_rmse <- rmse(test_target, nn_predictions)
nn_mae <- mae(test_target, nn_predictions)
nn_r2 <- cor(test_target, nn_predictions)^2

cat("\nNeural Network Evaluation:\n")
cat("RMSE:", nn_rmse, "\n")
cat("MAE:", nn_mae, "\n")
cat("R²:", nn_r2, "\n\n")

# Evaluate Random Forest performance
rf_rmse <- rmse(test_target, rf_predictions)
rf_mae <- mae(test_target, rf_predictions)
rf_r2 <- cor(test_target, rf_predictions)^2

cat("\nRandom Forest Evaluation:\n")
cat("RMSE:", rf_rmse, "\n")
cat("MAE:", rf_mae, "\n")
cat("R²:", rf_r2, "\n\n")

# Evaluate Elastic Net Model performance
lm_rmse <- rmse(test_target, lm_predictions)
lm_mae <- mae(test_target, lm_predictions)
lm_r2 <- cor(test_target, lm_predictions)^2

cat("\nElastic Net Model Evaluation:\n")
cat("RMSE:", lm_rmse, "\n")
cat("MAE:", lm_mae, "\n")
cat("R²:", lm_r2, "\n\n")

# Compare all models
cat("Model Comparison Summary:\n")
model_comparison <- data.frame(
  Model = c("Neural Network (Tuned)", "Random Forest (Tuned)", "Elastic Net (Tuned)"),
  RMSE = c(nn_rmse, rf_rmse, lm_rmse),
  MAE = c(nn_mae, rf_mae, lm_mae),
  R_squared = c(nn_r2, rf_r2, lm_r2)
)

# Sort by RMSE (lower is better)
model_comparison <- model_comparison[order(model_comparison$RMSE), ]
print(model_comparison)




