# rm(list = ls())

library(modeldata)
data(ames, package = "modeldata")
ames <- as.data.frame(ames)

# do the box plot for visualization
ames %>% ggplot(aes(x = 1, y = Sale_Price)) + geom_boxplot()

# remove non - variable columns
remove_cols <- nearZeroVar(ames, names = TRUE, 
                           freqCut = 2, uniqueCut = 20)

# select unique in a column
ames %>% select(Street) %>% unique()

# selecting only numeric columns
ames %>% select(where(is.numeric))

# selection only character columns
ames %>% select(where(is.factor))

# sum the square footage of total basement
ames %>% mutate(TotBsmt = BsmtFin_SF_1 + BsmtFin_SF_2 + Bsmt_Unf_SF)

# using caret on the 

## case 1 : linear model
# lets split the data into train and test split
reorder_rows = sample(nrow(ames))
splitval = round(0.60*nrow(ames))

ames_reorder = ames[reorder_rows,]

ames_train = ames_reorder[1:splitval,]
ames_test = ames_reorder[(splitval+1):nrow(ames),]


trControl = trainControl(
  method = "cv", 
  number = 5,
  verboseIter = TRUE
)

# mdl_linear = train(Sale_Price ~., data = ames_train, method = "lm", trainControl = trControl, preProc = c("nzv", "medianImpute", "center", "scale", "pca"))
# mdl_linear = train(Sale_Price ~., data = ames_train, method = "lm", trainControl = trControl, preProc = c("corr", "medianImpute", "center", "scale"))
mdl_linear = train(Sale_Price ~., data = ames_train, method = "lm", trainControl = trControl, preProc = c("nzv", "medianImpute", "center", "scale"))

mdl_linear

summary(mdl_linear)

prd <- stats::predict(mdl_linear, newdata = ames_test)

Metrics::rmse(ames_test$Sale_Price, prd)

rmse_manual = sqrt(mean((ames_test$Sale_Price - prd)^2))

# case 2:

# ridge, lasso and elasti-net

mdl_ridge = train(Sale_Price ~., data = ames_train, method = "ridge", trainControl = trControl, preProc = c("nzv", "medianImpute", "center", "scale"))

mdl_ridge

summary(mdl_ridge)

coefRidge <- predict(mdl_ridge$finalModel, type='coef', 
        mode='norm')$coefficients
coefRidge[nrow(coefRidge),]

# using caret glmnet
grid <- expand.grid(alpha = c(0.1, 0.5, 0.9),
                    lambda = seq(0.001, 0.1, length = 10))

# mdl_ridge2 = train(Sale_Price ~., data = ames_train, method = "glmnet", tuneLength = 10, trainControl = trControl, preProc = c("nzv", "medianImpute", "center", "scale"))
mdl_ridge2 = train(Sale_Price ~., data = ames_train, method = "glmnet", tuneGrid = grid, trainControl = trControl, preProc = c("nzv", "medianImpute", "center", "scale"))

mdl_ridge2

summary(mdl_ridge2)

min(mdl_ridge2[["results"]]$RMSE)

prd_ridge <- predict(mdl_ridge2, ames_test)

Metrics::rmse(ames_test$Sale_Price, prd_ridge)

# comparig models --------------------------------------------------------------

# https://www.tmwr.org/resampling.html

# Create model_list
model_list <- list(item1 = mdl_linear, item2 = mdl_ridge2)

# Pass model_list to resamples(): resamples
resamples <- resamples(model_list)

# Summarize the results
summary(resamples)

# Create bwplot
bwplot(resamples, metric = "RMSE")

# Create xyplot
xyplot(resamples, metric = "RMSE")

# Create bwplot
bwplot(resamples, metric = "Rsquared")

# Create xyplot
xyplot(resamples, metric = "Rsquared")


# coefRidge <- predict(mdl_ridge$finalModel, type='coef', 
#                      mode='norm')$coefficients
# coefRidge[nrow(coefRidge),]


# case 3 ----------------------------------------------------------------------
# # try to dummyVars
dv <- dummyVars("~.",ames, fullRank = T)
ames_d <- predict(dv, ames)

ames <- ames %>% mutate(Sale_Price_C = ifelse(Sale_Price > median(Sale_Price), 1, 0))
mdl_logistic <- train(Sale_Price_C ~., data = ames, method = "glm", family = "binomial", preProc = c("nzv", "medianImpute", "center", "scale"))
# mdl_logistic2 <- glm(Sale_Price_C ~., family = "binomial", ames)

# the model fails to fit because of too many variables
