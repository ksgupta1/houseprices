# https://topepo.github.io/caret/model-training-and-tuning.html#choosing-the-final-model

library(ggplot2)
library(tidyr)
library(dplyr)
library(Metrics)
library(naniar)
library(tibble)
library(tidyverse)
library(caret)
library(stats)

# other ml packages

# library("rattle")
# library(glmnet)
# library("rpart")
# library(regclass)


# flatten the correlation matrix
# https://stackoverflow.com/questions/7074246/show-correlations-as-an-ordered-list-not-as-a-large-matrix

# plot the decision tree
# https://stackoverflow.com/questions/24020666/how-to-make-a-tree-plot-in-caret-package

flattenCorr <- function(z)
{
  data = as.data.frame( as.table( z ) )
  combinations = combn( colnames( z ) , 2 , FUN = function( x ) { paste( x , collapse = "_" ) } )
  data = data[ data$Var1 != data$Var2 , ]
  data = data[ paste( data$Var1 , data$Var2 , sep = "_" ) %in% combinations , ]
  data = data[order(data$Freq),]
  return (data)
}


usehclust <- function(df)
{
  corDF = cor(df)
  dissimilarity <- 1 - abs(corDF)
  distance <- as.dist(dissimilarity)
  hc <- hclust(distance) 
  # clusterV = cutree(hc,h=0.75)
  clusterV = cutree(hc,k = 5)
  
  return (list("clusterV" = clusterV, "hc" = hc))
}

# example usage
if (FALSE)
{
  vals <- usehclust(ames %>% select(where(is.numeric)))
  clustervals <- vals["clusterV"]
  clustervals[order(clustervals)]
  plot(vals$hc)
}

# divide into 5 clusters and then take the mean value after center and scaling these values



# pre-processing steps includes - https://topepo.github.io/caret/pre-processing.html#the-preprocess-function

# 1. imputation
# 2. removing collinear variables
# 3. remove near zero variance variables
# 4. center and scaling the variables
# 