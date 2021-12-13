################################################################################
# Regression models: fitting them and evaluating their performance
################################################################################

# In-sample RMSE for linear regression on diamonds -----------------------------

# Fit lm model: model
model = lm(price ~., diamonds)

# Predict on full data: p
p = predict(model, diamonds, type = "response")

# Compute errors: error
error = p - diamonds[,"price"]

# Calculate RMSE
sqrt(mean(error^2))


# Randomly order the data frame-------------------------------------------------
# Set seed
set.seed(42)

# Shuffle row indices: rows
rows = sample(nrow(diamonds))

# Randomly order data
shuffled_diamonds = diamonds[rows,]

# Try an 80/20 split------------------------------------------------------------

# Determine row to split on: split
split <- round(nrow(diamonds) * 0.8)

# Create train
train <- diamonds[1:split,]

# Create test
test <- diamonds[(split+1):nrow(diamonds),]

# Predict on test set-----------------------------------------------------------

# Fit lm model on train: model
model <- lm(price ~., train)

# Predict on test: p
p <- predict(model, test)

# Calculate test set RMSE by hand-----------------------------------------------

# Compute errors: error
error = p - test[,"price"]

# Calculate RMSE
sqrt(mean(error^2))

# 10-fold cross-validation------------------------------------------------------
# Fit lm model using 10-fold CV: model
model <- train(
  price~., 
  diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  )
)

# Print model to console
summary(model)

# 5-fold cross-validation-------------------------------------------------------

# Fit lm model using 5-fold CV: model
model <- train(
  medv~., 
  Boston,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 5,
    verboseIter = TRUE
  )
)

# Print model to console
summary(model)


# 5 x 5-fold cross-validation---------------------------------------------------

# Fit lm model using 5 x 5-fold CV: model
model <- train(
  medv ~ ., 
  Boston,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv", 
    number = 5,
    repeats = 5, 
    verboseIter = TRUE
  )
)

# Print model to console
summary(model)

# Making predictions on new data------------------------------------------------
# Predict on full Boston dataset
predict(model, Boston)

################################################################################
# Classification models: fitting them and evaluating their performance
################################################################################

# Try a 60/40 split-------------------------------------------------------------

# Get the number of observations
n_obs <- nrow(Sonar)

# Shuffle row indices: permuted_rows
permuted_rows <- sample(n_obs)

# Randomly order data: Sonar
Sonar_shuffled <- Sonar[permuted_rows,]

# Identify row to split on: split
split <- round(n_obs * 0.6)

# Create train
train <- Sonar_shuffled[1:split,]

# Create test
test <- Sonar_shuffled[(split+1):n_obs,]


# Fit a logistic regression model-----------------------------------------------

# Fit glm model: model
model <- glm(Class ~., family = "binomial", train)

# Predict on test: p
p <- predict(model, test, type = "response")


# Calculate a confusion matrix--------------------------------------------------

# If p exceeds threshold of 0.5, M else R: m_or_r
m_or_r <- ifelse(p > 0.5, "M", "R")


# Convert to factor: p_class
p_class <- factor(m_or_r, levels = levels(test[["Class"]]))

# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])


# Try another threshold---------------------------------------------------------

# If p exceeds threshold of 0.9, M else R: m_or_r
m_or_r <- ifelse(p > 0.9, "M", "R")

# Convert to factor: p_class
p_class <- factor(m_or_r, levels = levels(test[["Class"]]))

# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])


# From probabilites to confusion matrix-----------------------------------------

# If p exceeds threshold of 0.1, M else R: m_or_r
m_or_r <- ifelse(p > 0.1, "M", "R")

# Convert to factor: p_class
p_class <- factor(m_or_r, levels = levels(test[["Class"]]))

# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])

# Plot an ROC curve-------------------------------------------------------------

# Predict on test: p
p <- predict(model, test, type = "response")

# Make ROC curve
colAUC(p, test[["Class"]], plotROC = T)

# Customizing trainControl------------------------------------------------------

# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Using custom trainControl-----------------------------------------------------

# Train glm with custom trainControl: model
model = train(Class ~., data = Sonar, method = "glm", trControl = myControl)


# Print model to console
summary(model)

################################################################################
# Tuning model parameters to improve performance
################################################################################

# Fit a random forest-----------------------------------------------------------

# Fit random forest: model
model <- train(
  quality ~.,
  tuneLength = 1,
  data = wine, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)

# Print model to console
summary(model)


# Try a longer tune length------------------------------------------------------

# Fit random forest: model
model <- train(
  quality ~.,
  tuneLength = 3,
  data = wine, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)

# Print model to console
summary(model)

# Plot model
plot(model)

# Fit a random forest with custom tuning----------------------------------------

# Define the tuning grid: tuneGrid
tuneGrid <- data.frame(
  .mtry = c(2,3,7),
  .splitrule = "variance",
  .min.node.size = 5
)


# Make a custom trainControl----------------------------------------------------

# Create custom trainControl: myControl-----------------------------------------
myControl <- trainControl(
  method = "cv", 
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = T, # IMPORTANT!
  verboseIter = TRUE
)


# Fit glmnet with custom trainControl-------------------------------------------

# Fit glmnet model: model
model <- train(y ~., 
               data = overfit,
               method = "glmnet",
               trControl = myControl
)

# Print model to console
summary(model)

# Print maximum ROC statistic
print(max(model[["results"]]$ROC))

# glmnet with custom trainControl and tuning------------------------------------

# Train glmnet with custom trainControl and tuning: model
model <- train(
  y ~ ., 
  data = overfit,
  tuneGrid = expand.grid(
    alpha = 0:1,
    lambda = seq(0.0001, 1, length = 20)
  ),
  method = "glmnet",
  trControl = myControl
)

# Print model to console
summary(model)

# Print maximum ROC statistic
max(model[["results"]]$ROC)


################################################################################
# Preprocessing your data
################################################################################

# Apply median imputation-------------------------------------------------------

# Apply median imputation: median_model
median_model <- train(
  x = breast_cancer_x, 
  y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "medianImpute"
)

# Print median_model to console
print(median_model)

# Use KNN imputation------------------------------------------------------------

# Apply KNN imputation: knn_model
knn_model <- train(
  x = breast_cancer_x, 
  y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "knnImpute"
)

# Print knn_model to console
print(knn_model)


# Combining preprocessing methods-----------------------------------------------

# Fit glm with median imputation
model <- train(
  x = breast_cancer_x, 
  y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = c("medianImpute", "center", "scale"),
  family = "binomial"
)

# Print model
print(model)

# Remove near zero variance predictors------------------------------------------

# Identify near zero variance predictors: remove_cols
remove_cols <- nearZeroVar(bloodbrain_x, names = TRUE, 
                           freqCut = 2, uniqueCut = 20)

# Get all column names from bloodbrain_x: all_cols
all_cols <- colnames(bloodbrain_x)

# Remove from data: bloodbrain_x_small
bloodbrain_x_small <- bloodbrain_x[ , setdiff(all_cols,remove_cols)]


# Using PCA as an alternative to nearZeroVar()----------------------------------
# Fit glm model using PCA: model
model <- train(
  x = bloodbrain_x, 
  y = bloodbrain_y,
  method = "glm", 
  preProcess = "pca"
)

# Print model to console
model

################################################################################
# Selecting models: a case study in churn prediction
################################################################################

# Make custom train/test indices------------------------------------------------
# Create custom indices: myFolds
myFolds <- createFolds(churn_y, k = 5)

# Create reusable trainControl object: myControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

# Fit the baseline model--------------------------------------------------------

# Fit glmnet model: model_glmnet
model_glmnet <- train(
  x = churn_x, 
  y = churn_y,
  metric = "ROC",
  method = "glmnet",
  trControl = myControl
)

# Random forest with custom trainControl----------------------------------------

# Fit random forest: model_rf
model_rf <- train(
  x = churn_x, 
  y = churn_y,
  metric = "ROC",
  method = "ranger",
  trControl = myControl
)


# Create a resamples object-----------------------------------------------------

# Create model_list
model_list <- list(item1 = model_glmnet, item2 = model_rf)

# Pass model_list to resamples(): resamples
resamples <- resamples(model_list)

# Summarize the results
summary(resamples)

# Create bwplot
bwplot(resamples, metric = "ROC")

# Create xyplot
xyplot(resamples, metric = "ROC")

# Create ensemble model: stack
stack <- caretStack(model_list, method = "glm")

# Look at summary
summary(stack)

