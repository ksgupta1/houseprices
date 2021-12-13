# https://www.youtube.com/watch?v=WtwMj9PakF8


# just need to do this once
list.of.packages <- c("modeldata", "glmnet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

# read in the data and convert to data frame
data(ames, package = "modeldata")
ames <- as.data.frame(ames)

# peek at the column names
sort(colnames(ames))

# subset the data 
ames <- subset(ames, ames$Sale_Condition == "Normal" & ames$Bldg_Type == "OneFam")

# create some new variables
ames$Year_Frac <- ames$Year_Sold + ames$Mo_Sold/12
ames$Bsmt_Fin_SF <- ames$Total_Bsmt_SF - ames$Bsmt_Unf_SF

# make some exploratory plots of the data
plot(ames$Year_Frac, ames$Sale_Price, xlab = "YearFrac", ylab = "Sale Price")
plot(ames$Gr_Liv_Area, ames$Sale_Price)
plot(ames$Bedroom_AbvGr, ames$Sale_Price)
plot(ames$Full_Bath, ames$Sale_Price)
plot(ames$Half_Bath, ames$Sale_Price)
plot(ames$TotRms_AbvGrd, ames$Sale_Price)
plot(ames$Total_Bsmt_SF, ames$Sale_Price)
plot(ames$Bsmt_Unf_SF, ames$Sale_Price)

# another way to plot the rows
plot(ames[["Year_Frac"]], ames[["Sale_Price"]])

# quick refresher on interpretation of regression coefficients
summary(lm(Sale_Price ~ Bsmt_Unf_SF + Bsmt_Fin_SF, data = ames))
summary(lm(Sale_Price ~ Bsmt_Fin_SF + Total_Bsmt_SF, data = ames))
summary(lm(Sale_Price ~ Bsmt_Unf_SF + Bsmt_Fin_SF + Total_Bsmt_SF, data = ames))

# fit a linear model with several variables
m1 <- lm(Sale_Price ~ Gr_Liv_Area + Bsmt_Unf_SF + Bsmt_Fin_SF + Full_Bath + Half_Bath + Bedroom_AbvGr + TotRms_AbvGrd, data = ames)
summary(m1)

# now lets focus on oe neighborhood

old_town <- subset(ames, ames$Neighborhood == "Old_Town")

# same model fit to old town
m2 <- lm(Sale_Price ~ Gr_Liv_Area + Bsmt_Unf_SF + Bsmt_Fin_SF + Full_Bath + Half_Bath + Bedroom_AbvGr + TotRms_AbvGrd, data = old_town)
summary(m2)


# model without bathroom info
m21 <- lm(Sale_Price ~ Gr_Liv_Area + Bsmt_Unf_SF + Bsmt_Fin_SF + Bedroom_AbvGr + TotRms_AbvGrd, data = old_town)


# creating an F-test manually
rss0 <- sum(m21$residuals^2)
df0 <- m21$df.residual
rss1 <- sum(m2$residuals^2)
df1 <- m2$df.residual

f <- ((rss0 -rss1)/(df0 - df1))/(rss1 /df1)

pvalue <- 1 - pf(f, df1 = df0 - df1, df2 = df1)
pvalue

# another way to do all of this is using anova
anova(m21, m2)


###-------------------------------------------
# https://rstatisticsblog.com/data-science-in-action/machine-learning/lasso-regression/
# http://rstudio-pubs-static.s3.amazonaws.com/373113_ba8a4073956f4587b8f5036435e877d6.html

# now trying another model that is lasso regression on the same data
# lasso helps eliminate the variables that we do not need
# using and installing the glmnet package for this
# shrinkage models penalize the model coefficient estimates using lambda tuning parameter to reduce their variance which result in a better model fitting


library(glmnet)
