---
title: "Using bayesian optimisation to tune a XGBOOST model in R"
subtitle: "ML series (Post #1)"
summary: "How to use bayesian optimisation to tune hyperparameters in a XGBOOST model in R"
author: "royr2"
date: 2022-01-09
categories: ["R", "risk analytics", "xgboost", "bayesian optimisation", "machine learning"]
tags: ["R", "analytics", "machine learning"]  
comments: true
---



My first post in 2022! A very happy new year to anyone reading this. :smile:

I was looking for a simple and effective way to tune `xgboost` models in `R` and came across this package called [ParBayesianOptimization](https://github.com/AnotherSamWilson/ParBayesianOptimization). Here's a quick tutorial on how to use it to tune a `xgboost` model. 


```r
# Pacman is a package management tool 
install.packages("pacman")
```


```r
library(pacman)

# p_load automatically installs packages if needed
p_load(xgboost, ParBayesianOptimization, mlbench, dplyr, skimr, recipes, resample)
```

## Data prep


```r
# Load up some data
data("BostonHousing2")
```


```r
# Data summary
skim(BostonHousing2)
```


Table: Table 1: Data summary

|                         |               |
|:------------------------|:--------------|
|Name                     |BostonHousing2 |
|Number of rows           |506            |
|Number of columns        |19             |
|_______________________  |               |
|Column type frequency:   |               |
|factor                   |2              |
|numeric                  |17             |
|________________________ |               |
|Group variables          |None           |


**Variable type: factor**

|skim_variable | n_missing| complete_rate|ordered | n_unique|top_counts                         |
|:-------------|---------:|-------------:|:-------|--------:|:----------------------------------|
|town          |         0|             1|FALSE   |       92|Cam: 30, Bos: 23, Lyn: 22, Bos: 19 |
|chas          |         0|             1|FALSE   |        2|0: 471, 1: 35                      |


**Variable type: numeric**

|skim_variable | n_missing| complete_rate|    mean|      sd|     p0|     p25|     p50|     p75|    p100|hist                                     |
|:-------------|---------:|-------------:|-------:|-------:|------:|-------:|-------:|-------:|-------:|:----------------------------------------|
|tract         |         0|             1| 2700.36| 1380.04|   1.00| 1303.25| 3393.50| 3739.75| 5082.00|▅▂▂▇▂ |
|lon           |         0|             1|  -71.06|    0.08| -71.29|  -71.09|  -71.05|  -71.02|  -70.81|▁▂▇▂▁ |
|lat           |         0|             1|   42.22|    0.06|  42.03|   42.18|   42.22|   42.25|   42.38|▁▃▇▃▁ |
|medv          |         0|             1|   22.53|    9.20|   5.00|   17.02|   21.20|   25.00|   50.00|▂▇▅▁▁ |
|cmedv         |         0|             1|   22.53|    9.18|   5.00|   17.02|   21.20|   25.00|   50.00|▂▇▅▁▁ |
|crim          |         0|             1|    3.61|    8.60|   0.01|    0.08|    0.26|    3.68|   88.98|▇▁▁▁▁ |
|zn            |         0|             1|   11.36|   23.32|   0.00|    0.00|    0.00|   12.50|  100.00|▇▁▁▁▁ |
|indus         |         0|             1|   11.14|    6.86|   0.46|    5.19|    9.69|   18.10|   27.74|▇▆▁▇▁ |
|nox           |         0|             1|    0.55|    0.12|   0.38|    0.45|    0.54|    0.62|    0.87|▇▇▆▅▁ |
|rm            |         0|             1|    6.28|    0.70|   3.56|    5.89|    6.21|    6.62|    8.78|▁▂▇▂▁ |
|age           |         0|             1|   68.57|   28.15|   2.90|   45.02|   77.50|   94.07|  100.00|▂▂▂▃▇ |
|dis           |         0|             1|    3.80|    2.11|   1.13|    2.10|    3.21|    5.19|   12.13|▇▅▂▁▁ |
|rad           |         0|             1|    9.55|    8.71|   1.00|    4.00|    5.00|   24.00|   24.00|▇▂▁▁▃ |
|tax           |         0|             1|  408.24|  168.54| 187.00|  279.00|  330.00|  666.00|  711.00|▇▇▃▁▇ |
|ptratio       |         0|             1|   18.46|    2.16|  12.60|   17.40|   19.05|   20.20|   22.00|▁▃▅▅▇ |
|b             |         0|             1|  356.67|   91.29|   0.32|  375.38|  391.44|  396.22|  396.90|▁▁▁▁▇ |
|lstat         |         0|             1|   12.65|    7.14|   1.73|    6.95|   11.36|   16.96|   37.97|▇▇▅▂▁ |

Looks like there is are two factor variables. We'll need to convert them into numeric variables before we proceed. I'll use the `recipes` package to one-hot encode them. 


```r
# Predicting median house prices
rec <- recipe(cmedv ~ ., data = BostonHousing2) %>%
  
  # Collapse categories where population is < 3%
  step_other(town, chas, threshold = .03, other = "Other") %>% 
  
  # Create dummy variables for all factor variables 
  step_dummy(all_nominal_predictors())

# Train the recipe on the data set
prep <- prep(rec, training = BostonHousing2)

# Create the final model matrix
model_df <- bake(prep, new_data = BostonHousing2)
```


```r
# All levels have been one hot encoded and separate columns have been appended to the model matrix
colnames(model_df)
##  [1] "tract"                  "lon"                    "lat"                   
##  [4] "medv"                   "crim"                   "zn"                    
##  [7] "indus"                  "nox"                    "rm"                    
## [10] "age"                    "dis"                    "rad"                   
## [13] "tax"                    "ptratio"                "b"                     
## [16] "lstat"                  "cmedv"                  "town_Boston.Savin.Hill"
## [19] "town_Cambridge"         "town_Lynn"              "town_Newton"           
## [22] "town_Other"             "chas_X1"
```

Next, we can use the `resample` package to create test/train splits.


```r
splits <- rsample::initial_split(model_df, prop = 0.7)

# Training set
train_df <- rsample::training(splits)

# Test set
test_df <- rsample::testing(splits)
```


```r
dim(train_df)
## [1] 354  23
```


```r
dim(test_df)
## [1] 152  23
```
## Finding optimal parameters

Now we can start to run some optimisations using the `ParBayesianOptimization` package.


```r
# The xgboost interface accepts matrices 
X <- train_df %>%
  # Remove the target variable
  select(!medv, !cmedv) %>%
  as.matrix()

# Get the target variable
y <- train_df %>%
  pull(cmedv)
```


```r
# Cross validation folds
folds <- list(fold1 = as.integer(seq(1, nrow(X), by = 5)),
              fold2 = as.integer(seq(2, nrow(X), by = 5)))
```

We'll need an objective function which can be fed to the optimiser. We'll use the value of the evaluation metric from `xgb.cv()` as the value that needs to be optimised. 


```r
# Function must take the hyper-parameters as inputs
obj_func <- function(eta, max_depth, min_child_weight, subsample, lambda, alpha) {
  
  param <- list(
    
    # Hyter parameters 
    eta = eta,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    lambda = lambda,
    alpha = alpha,
    
    # Tree model 
    booster = "gbtree",
    
    # Regression problem 
    objective = "reg:squarederror",
    
    # Use the Mean Absolute Percentage Error
    eval_metric = "mape")
  
  xgbcv <- xgb.cv(params = param,
                  data = X,
                  label = y,
                  nround = 50,
                  folds = folds,
                  prediction = TRUE,
                  early_stopping_rounds = 5,
                  verbose = 0,
                  maximize = F)
  
  lst <- list(
    
    # First argument must be named as "Score"
    # Function finds maxima so inverting the output
    Score = -min(xgbcv$evaluation_log$test_mape_mean),
    
    # Get number of trees for the best performing model
    nrounds = xgbcv$best_iteration
  )
  
  return(lst)
}
```

Once we have the objective function, we'll need to define some bounds for the optimiser to search within.  


```r
bounds <- list(eta = c(0.001, 0.2),
               max_depth = c(1L, 10L),
               min_child_weight = c(1, 50),
               subsample = c(0.1, 1),
               lambda = c(1, 10),
               alpha = c(1, 10))
```

We can now run the optimiser to find a set of optimal hyper-parameters. 




```r
set.seed(1234)
bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = length(bounds) + 2, iters.n = 3)
```




```r
# Show relevant columns from the summary object 
bayes_out$scoreSummary[1:5, c(3:8, 13)]
##           eta max_depth min_child_weight subsample   lambda    alpha      Score
## 1: 0.13392137         8         4.913332 0.2105925 4.721124 3.887629 -0.0902970
## 2: 0.19400811         2        25.454160 0.9594105 9.329695 3.173695 -0.1402720
## 3: 0.16079775         2        14.035652 0.5118349 1.229953 5.093530 -0.1475580
## 4: 0.08957707         4        12.534842 0.3844404 4.358837 1.788342 -0.1410245
## 5: 0.02876388         4        36.586761 0.8107181 6.137100 6.039125 -0.3061535
```


```r
# Get best parameters
data.frame(getBestPars(bayes_out))
##         eta max_depth min_child_weight subsample lambda alpha
## 1 0.1905414         8         1.541476 0.8729207      1     1
```

## Fitting the model

We can now fit a model and check how well these parameters work.


```r
# Combine best params with base params
opt_params <- append(list(booster = "gbtree", 
                          objective = "reg:squarederror", 
                          eval_metric = "mae"), 
                     getBestPars(bayes_out))

# Run cross validation 
xgbcv <- xgb.cv(params = opt_params,
                data = X,
                label = y,
                nround = 100,
                folds = folds,
                prediction = TRUE,
                early_stopping_rounds = 5,
                verbose = 0,
                maximize = F)

# Get optimal number of rounds
nrounds = xgbcv$best_iteration

# Fit a xgb model
mdl <- xgboost(data = X, label = y, 
               params = opt_params, 
               maximize = F, 
               early_stopping_rounds = 5, 
               nrounds = nrounds, 
               verbose = 0)
```


```r
# Evaluate performance 
actuals <- test_df$cmedv
predicted <- test_df %>%
  select_at(mdl$feature_names) %>%
  as.matrix %>%
  predict(mdl, newdata = .)
```


```r
# Compute MAPE
mean(abs(actuals - predicted)/actuals)
## [1] 0.008325121
```
## Compare with grid search 


```r
grd <- expand.grid(
  eta = seq(0.001, 0.2, length.out = 5),
  max_depth = seq(2L, 10L, by = 1),
  min_child_weight = seq(1, 25, length.out = 3),
  subsample = c(0.25, 0.5, 0.75, 1),
  lambda = c(1, 5, 10),
  alpha = c(1, 5, 10))

dim(grd)
```


```r
grd_out <- apply(grd, 1, function(par){
  
    par <- append(par, list(booster = "gbtree",objective = "reg:squarederror",eval_metric = "mae"))
    mdl <- xgboost(data = X, label = y, params = par, nrounds = 50, early_stopping_rounds = 5, maximize = F, verbose = 0)
    lst <- data.frame(par, score = mdl$best_score)

    return(lst)
  })

grd_out <- do.call(rbind, grd_out)
```






```r
best_par <- grd_out %>%
  data.frame() %>%
  arrange(score) %>%
  .[1,]
```


```r
# Fit final model
params <- as.list(best_par[-length(best_par)])
xgbcv <- xgb.cv(params = params,
                data = X,
                label = y,
                nround = 100,
                folds = folds,
                prediction = TRUE,
                early_stopping_rounds = 5,
                verbose = 0,
                maximize = F)

nrounds = xgbcv$best_iteration

mdl <- xgboost(data = X, 
               label = y, 
               params = params, 
               maximize = F, 
               early_stopping_rounds = 5, 
               nrounds = nrounds, 
               verbose = 0)
```


```r
# Evaluate on test set
act <- test_df$medv
pred <- test_df %>%
  select_at(mdl$feature_names) %>%
  as.matrix %>%
  predict(mdl, newdata = .)
```


```r
mean(abs(act - pred)/act)
## [1] 0.009543707
```

While both the methods offer similar final results, the bayesian optimiser completed its search in less than a minute where as the grid search took over seven minutes. Also, I find that I can use bayesian optimisation to search a larger parameter space more quickly than a traditional grid search. 

*Thoughts? Comments? Helpful? Not helpful? Like to see anything else added in here? Let me know!* 
