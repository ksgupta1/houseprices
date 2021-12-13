# https://www.youtube.com/watch?v=83NbLRlWZ_k

train = read.csv("./house-prices-advanced-regression-techniques/train.csv")
test = read.csv("./house-prices-advanced-regression-techniques/test.csv")

dim(train)
dim(test)

str(train)

train = read.csv("./house-prices-advanced-regression-techniques/train.csv", stringsAsFactors = FALSE)
test = read.csv("./house-prices-advanced-regression-techniques/test.csv", stringsAsFactors = FALSE)


SalePrice = train$SalePrice
train$SalePrice = NULL

full_data = rbind(train, test)
for (col in colnames(full_data))
{
  if (typeof(full_data[,col]) == "character")
  {
    new_col = full_data[,col]
    new_col[is.na(new_col)] = "missing"
    full_data[,col]= as.factor(new_col)
  }
}

train = full_data[1:nrow(train),]
train$SalePrice = SalePrice
test = full_data[(nrow(train)+1):nrow(full_data),]
summary(train)


train[is.na(train)] = -1
test[is.na(test)] = -1

# find the variables that are highly correlated to it
for (col in colnames(train))
{
  if (is.numeric(train[,col]))
  {
    corrr = cor(train[,col], train[,"SalePrice"])
    if (abs(corrr) > 0.5)
    {
      print(col)
      print(corrr)
    }
  }
}

# find the variables that are least correlated to it
for (col in colnames(train))
{
  if (is.numeric(train[,col]))
  {
    corrr = cor(train[,col], train[,"SalePrice"])
    if (abs(corrr) < 0.1)
    {
      print(col)
      print(corrr)
    }
  }
}

cors = cor(train[, sapply(train, is.numeric)])
high_cor = which(abs(cors) > 0.6 & abs(cors) < 1)
rows = rownames(cors)[((high_cor-1)%/% 38)+1]
cols = colnames(cors)[ifelse(high_cor%%38 == 0, 38, high_cor%%38)]
vals = cors[high_cor]
cor_data = data.frame(cols = cols, rows = rows, correlation = vals)


for(col in colnames(train))
{
  if (is.numeric(train[,col]))
  {
    # plot(density(train[,col]), main = col)
    plot(hist(train[,col]), xlab = col, main = col)
  }
}


# create some input features
train$total_sq_footage <- train$GrLivArea + train$TotalBsmtSF
test$total_sq_footage <- test$GrLivArea + test$TotalBsmtSF

train$total_baths <- train$BsmtFullBath + train$FullBath + 0.5 * (train$BsmtHalfBath + train$HalfBath)
test$total_baths <- test$BsmtFullBath + test$FullBath + 0.5 * (test$BsmtHalfBath + test$HalfBath)

train$Id <- NULL
test$Id <- NULL


#Method 1: do the learning here
library(caret) # https://topepo.github.io/caret/model-training-and-tuning.html
library(plyr)
library(xgboost)
library(Metrics)


# Method 1------

method1 <- FALSE
if (method1)
{
  custom_summary <- function(data, lev = NULL , model = NULL)
  {
    out <- rmsle(data[, "obs"], data[, "pred"])
    names(out) <- c("rmsle")
    return (out)
  }
  
  
  
  control <- trainControl(method = "cv", 
                          number = 5, 
                          summaryFunction = custom_summary)
  
  
  grid <- expand.grid(nrounds = c(1000, 1200, 1500), # test values for boosting rounds
                      max_depth = c(6, 8, 10), # test values for tree depth
                      eta = c(0.025, 0.01), # test values for learning rate
                      gamma = c(0.1), #minimum loss reduction to do a split node
                      colsample_bytree = c(1),
                      min_child_weight = c(1),
                      subsample = c(0.8))
  
  # https://cran.r-project.org/web/packages/xgboost/vignettes/discoverYourData.html
  # note that caret package converts all factors to dummy variables so we don't have to do it manually
  set.seed(12)
  xgb_tree_model <- train(SalePrice~.,
                          data = train,
                          method = "xgbTree",
                          trControl = control,
                          tuneGrid=grid, 
                          metric="rmsle",
                          maximize = FALSE)
  
  
  
  xgb_tree_model$results
  xgb_tree_model$bestTune
  varImp(xgb_tree_model)
  testpredictions <- predict(xgb_tree_model, newdata = test)
  testpredictions
  
}


# Method 2: Ridge Regression
# log transformation for skewed features, 
# good idea to log transform the skewed features
# also glmnet needs numeric values so we need to convert categorical variables to dummy variables
# or we can use the caret package which converts all factors to dummy variables
# https://stackoverflow.com/questions/57712116/regarding-preprocessing-in-lasso-using-caret-package-in-r
# http://rstudio-pubs-static.s3.amazonaws.com/373113_ba8a4073956f4587b8f5036435e877d6.html
# https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net
# https://campus.datacamp.com/courses/machine-learning-with-caret-in-r/regression-models-fitting-them-and-evaluating-their-performance?ex=1

# browser()

method2 <- TRUE
if (method2)
{
  set.seed(12)
  control <- trainControl(method = "cv", 
                          number = 5,
                          savePredictions = T, verboseIter = TRUE)
  
  grid <- expand.grid(.alpha = 0, .lambda = 10^seq(-5, 5, length = 10))
  
  ridge_model <- train(
    SalePrice ~ .,
    data = train,
    method = "glmnet",
    trControl = control,
    tuneGrid=grid,
    family = "gaussian"
  )
}
# Method 3: Lasso Regression
