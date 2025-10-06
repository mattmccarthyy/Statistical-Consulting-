rm(list = ls())

# XGBoost Example
# Link in description of this Video: https://www.youtube.com/watch?v=gKyUucJwD8U

# install.packages("modeldata")
library(modeldata)
data("stackoverflow")
head(stackoverflow)

# isolate X and Y
# install.packages("dplyr")
library(dplyr)
y <- as.numeric(stackoverflow$Remote) - 1 # Just 1's and 2's, the -1 makes it 1's and 0's, this is a classification problem so this form is ideal 
x <- stackoverflow %>% select(-Remote) # dplyr uses pipe operator, -Remote means select every predictor but Remote
str(x) # XGBoost does not deal with factors, so every single factor has to be transformed into a dummy variable. 

# Transforming factor into dummy variables
# Using library "fastDummies" as it does this fast, can also just do it manually, if using this we have to look into it a great deal, may be wiser to go manual. 
# install.packages("fastDummies")
library(fastDummies)
x <- dummy_cols(x, remove_first_dummy = TRUE) # Now we have dummy variables, but we still need to remove the "Country" column, as that remains a factor.
x <- x %>% select(-Country) # takes every column in x besides "Country". 

# Setting the parameters, telling XGBoost we are doing a classification problem.
params <- list(set.seed = 1502, 
               eval_metric = "auc", # area under curve metric in order to evaluate classification problems
               objective = "binary:logistic") # re-enforce the logistics, i.e. what kind of depended variable we have, we have 1's and 0's => logistic

# Running XGBoost
# install.packages("xgboost")
library(xgboost)
model <- xgboost(data = as.matrix(x), # XGBoost requires all predictors be fed in the form of a matrix
                 label = y, # Our dependent variable
                 params = params, # Created above
                 nrounds = 20, # No. of times we want x to be run, each model is built or trained on previous one
                 verbose = 1, # For XGBoost to communicate how it is doing
) # Look into using early_stopping_rounds, stops the loop if the model is no longer learning, can use this for efficiency purposes.

# Can't really assess accuracy here as we didn't split the data into a training set and a test set, will look into this soon.  
# Looking at shap values to see the characteristics that correlate the most with the people that work remotely vs. those that do not. 
xgb.plot.shap(data = as.matrix(x), #again, needs to be done as.matrix in XGBoost library
              model = model,
              top_n = 5, # taking the top 5, can do others too
)

# XGBoost is really good at dealing with non-linear relationships.
# Any shap values below 0 decreases likelihood, any above 0 increases likelihood, need to look into magnitude next. 
# Can see: more years coded => less likely to work remotely. 
# Can see: if you earn less, more liekly to work remotely; if you earn more, up to a certain point, you are less likely to work remotely; however, as you earn significantly more, the likelihood you work remotely begins increasing once more.
