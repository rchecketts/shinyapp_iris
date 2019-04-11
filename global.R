
#--------------------------------------------------------------------------------#
# Load Libraries
#--------------------------------------------------------------------------------#
library(tidyverse)
library(data.table)
library(xgboost)


#--------------------------------------------------------------------------------#
# Prepare data
#--------------------------------------------------------------------------------#
y <- as.numeric(iris$Species) - 1
x <- iris %>% select(-Species)
## save out variable names
var.names = names(x)
x <- as.matrix(x)


runIrismodel <- function() {
  param <- list(
    "objective" = "multi:softprob"
    ,"eval_metric" = "mlogloss"
    ,"num_class" = length(table(y))
    ,"eta" = .3
    ,"max_depth" = 50
    # ,"lambda" = 1
    # ,"alpha" = .8
    # ,"min_child_weight" = 3
    # ,"subsample" = .9
    # ,"colsample_bytree" = .6
  )
  
  
  cv.nround = 200
  bst.cv <- xgb.cv(
    params = param
    , data = x
    , label = y
    , nfold = 5
    , nrounds = cv.nround
    , missing = NA
    , prediction = TRUE
    , early_stopping_rounds = 15
  )
  
  opt_nround <- which.min(bst.cv$evaluation_log$test_mlogloss_mean)
  
  IrisClassifier <- xgboost(
    params = param
    , data = x
    , label = y
    , nrounds = opt_nround
    , missing = NA
    , early_stopping_rounds = 15
  )
  
  
  xgb.importance(feature_names = var.names, model = IrisClassifier)
  
  xgb.save(model = IrisClassifier, fname = "inst/iris_xgboost_model")
}


if (exists('IrisClassifier')) {
  print("Model already loaded...")
} else {
  try(
    IrisClassifier <- xgb.load(modelfile = "inst/iris_xgboost_model")
  )
  if (!exists('IrisClassifier')) {
    IrisClassifier <- runIrismodel()
  }
}


get_probs <- function(input) {
  # Input is a dataframe or matrix where the columns are in this order:
  #   sepal_length, sepal_width, petal_length, petal_width
  if (is.data.frame(input)){
    x <- sapply(x, as.numeric)
    x <- as.matrix(input)
  } else if (is.data.table(input)) {
    x <- sapply(x, as.numeric)
    x <- as.matrix(input)
  }
  p <- predict(IrisClassifier, x)
  p <- round(p, 4)
  p <- matrix(p, ncol = length(table(y)), byrow = TRUE)
  p <- data.frame(p)
  names(p) <- levels(iris$Species)
  p$rowid <- seq(nrow(p))
  p <- as.data.table(p)
  setnames(p, old=c('setosa','versicolor','virginica'), new=c('setosa_prob','versicolor_prob','virginica_prob'))
  probs <- melt.data.table(data = p, id.vars = 'rowid', variable.name = 'prediction', value.name = 'probability')
  probs[, max_prob := frankv(-probability, ties.method = 'random'), by=c('rowid')]
  probs <- as.data.table(probs)
  probs <- probs[max_prob == 1, ]
  p <- merge(p, probs, by = 'rowid')
  p[, c('probability', 'max_prob', 'rowid') := NULL]
  p$prediction <- gsub(pattern = "_prob", replacement = "", x = p$prediction)
  p <- as.data.table(p)
  return(p)
}


df_points <- data.frame('Sepal.Length' = numeric(), 'Sepal.Width' = numeric(), 'Petal.Length' = numeric(), 'Petal.Width' = numeric(),stringsAsFactors = FALSE)

iris_long <- as.data.table(iris)
iris_long$rowid <- seq(nrow(iris_long))
iris_long[, Species := NULL]
iris_long <- melt.data.table(data = iris_long, id.vars = 'rowid')

get_max_row <- function(){
  nrow(df_points)
}


