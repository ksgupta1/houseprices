
# --------------------------------------------------------------

# https://r-lang.com/scale-function-in-r/
scale(car, center = T, scale = T)

# Interpreting linear regression -------------------------------

# Glimpse on the car dataset
glimpse(car)

# Build a multivariate regression model: car_lr
car_lr <- lm(consume ~ ., data = car)

# Summarize the model and display its coefficients
summary(car_lr)$coefficients

# Predict with linear regression model
predict(car_lr, test_instance)


# Interpreting decision tree-----------------------------------

# Build a regression tree: car_dt
car_dt <- rpart(consume ~ ., data = car)

# Fancy tree plot
fancyRpartPlot(car_dt)

# Extract rules from the tree
rpart.rules(car_dt)

# Predict test instance with decision tree
predict(car_dt, test_instance)


# Ridge regression--------------------------------------------------------------
# Glimpse at the dataset
glimpse(fifa19_scaled)

# Ridge regression: mdlRidge
mdlRidge <- train(PlayerValue ~ ., data = fifa19_scaled,
                  method = "ridge", tuneLength = 8)

# Plot ridge train object
plot(mdlRidge)

# Ridge regression coefficients
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/predict
coefRidge <- predict(mdlRidge$finalModel, type='coef', 
                     mode='norm')$coefficients
coefs$RidgeAll <- coefRidge[nrow(coefRidge),]
print(coefs)

# 