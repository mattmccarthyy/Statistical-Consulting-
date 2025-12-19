rm(list = ls())

set.seed(100)

library(rpart)
library(rpart.plot)

policy_frequency_derived <- readRDS(
  url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/derived/policy_frequency_derived.rds")
)


#########################################################################################
# Using our "split_data" function with same seed to ensure comparability with our GLM's
# Including here for auditability, so that script does not need to be run before this
#########################################################################################
split_data = function(data){
  n = nrow(data)
  indices = sample(1:n) #Randomly shuffles row indexes
  
  #Compute split sizes
  train_size = floor(0.6*n) 
  validation_size = floor(0.2*n)
  test_size = n-train_size-validation_size
  
  #Split indices
  train_index = indices[1:train_size]
  validation_index = indices[(train_size + 1):(train_size + validation_size)]
  test_index = indices[(train_size + validation_size +1):n]
  
  #Create splits in data
  train = data[train_index, ,drop=FALSE]
  validation = data[validation_index, , drop=FALSE]
  test = data[test_index, , drop=FALSE]
  
  #Assign to global environment
  assign("train", train, envir = .GlobalEnv)
  assign("validation", validation, envir = .GlobalEnv)
  assign("test", test, envir = .GlobalEnv)
}



###############################################################################
# Split data (creates train / validation / test)
###############################################################################
split_data(policy_frequency_derived)



###############################################################################
# Tree formula
###############################################################################
tree_formula <- n_claims ~
  occupation_risk5 + 
  ncd_level + 
  age + 
  vehicle_age + 
  vehicle_power + 
  num_drivers +
  province + 
  area + 
  fuel + 
  employment_missing + 
  security_device +
  marital + 
  employment + 
  body_type + 
  transmission + 
  primary_usage + 
  occasional_commercial +
  reported_mileage_missing + 
  engine_cc_missing +
  engine_cc + 
  vehicle_value + 
  reported_mileage



###############################################################################
# Helper fn for 1-SE pruning, prevents us including the same code block twice
###############################################################################
prune_1se <- function(fit){
  cp_tab <- fit$cptable
  i_min <- which.min(cp_tab[, "xerror"])
  x_min <- cp_tab[i_min, "xerror"]
  x_se <- cp_tab[i_min, "xstd"]
  
  i_1se <- which(cp_tab[, "xerror"] <= x_min + x_se)[1]
  cp_1se <- cp_tab[i_1se, "CP"]
  
  list(
    pruned = prune(fit, cp = cp_1se),
    cp_1se = cp_1se
  )
}



###############################################################################
# TREE 1: "Simple" specification (more conservative)
###############################################################################
tree_simple <- rpart(
  tree_formula,
  data = train,
  method  = "poisson",
  weights = exposure,
  control = rpart.control(
    maxdepth = 3,
    minsplit = 8000,
    minbucket = 4000,
    cp = 0.0005,
    xval = 10
  )
)

tmp1 <- prune_1se(tree_simple)
tree_simple_pruned <- tmp1$pruned

printcp(tree_simple)
plotcp(tree_simple)

rpart.plot(
  tree_simple_pruned,
  main = "Poisson Regression Tree (Simple; 1-SE pruned)",
  type = 2,
  extra = 101,
  fallen.leaves = TRUE,
  under = TRUE
)


###############################################################################
# TREE 2: "Deeper" specification (less conservative; still credible)
###############################################################################
tree_deep <- rpart(
  tree_formula,
  data = train,
  method = "poisson",
  weights = exposure,
  control = rpart.control(
    maxdepth  = 5,
    minsplit  = 4000,
    minbucket = 2000,
    cp = 0.0003,
    xval = 10
  )
)

tmp2 <- prune_1se(tree_deep)
tree_deep_pruned <- tmp2$pruned

printcp(tree_deep)
plotcp(tree_deep)

rpart.plot(
  tree_deep_pruned,
  main = "Poisson Regression Tree (Deeper; 1-SE pruned)",
  type = 2,
  extra = 101,
  fallen.leaves = TRUE,
  under = TRUE
)


###############################################################################
# Checking if splits differ at all between trees
###############################################################################
vars_simple <- setdiff(unique(tree_simple_pruned$frame$var), "<leaf>")
vars_deep <- setdiff(unique(tree_deep_pruned$frame$var), "<leaf>")

vars_simple
vars_deep
# Identical splits

###############################################################################
# Checking if extra depth helps at all 
###############################################################################
mu_s <- as.numeric(predict(tree_simple_pruned, newdata = validation))
mu_d <- as.numeric(predict(tree_deep_pruned,   newdata = validation))

val_comp <- rbind(
  simple = c(
    exposure = sum(validation$exposure),
    obs_rate_mean = sum(validation$n_claims) / sum(validation$exposure),
    pred_rate_mean = sum(mu_s) / sum(validation$exposure),
    pois_nll = -sum(dpois(validation$n_claims, lambda = mu_s, log = TRUE))
  ),
  deep = c(
    exposure = sum(validation$exposure),
    obs_rate_mean = sum(validation$n_claims) / sum(validation$exposure),
    pred_rate_mean = sum(mu_d) / sum(validation$exposure),
    pois_nll = -sum(dpois(validation$n_claims, lambda = mu_d, log = TRUE))
  )
)

round(val_comp, 6)
all.equal(mu_s, mu_d)



###############################################################################
# Save objects
###############################################################################
saveRDS(tree_simple, file = "poisson_tree_train_unpruned_simple.rds")
saveRDS(tree_simple_pruned, file = "poisson_tree_train_pruned_1se_simple.rds")

saveRDS(tree_deep, file = "poisson_tree_train_unpruned_deep.rds")
saveRDS(tree_deep_pruned, file = "poisson_tree_train_pruned_1se_deep.rds")



###############################################################################
# Plot for report
###############################################################################
par(mfrow = c(1,1))

purple_grad <- c(
  "#f2e6ff",  
  "#d4b3ff",
  "#b266ff",
  "#8d17f1" 
)

rpart.plot(
  tree_simple_pruned,
  type = 2,
  extra = 1,                
  fallen.leaves = TRUE,
  under = FALSE,
  tweak = 1.25,           
  box.palette = purple_grad,
  branch.col = "grey40",
  branch.lwd = 1.2,
  shadow.col = "grey90",
  faclen = 0,
  varlen = 0, 
  digits = 2, # fewer digits
  nn = TRUE,                
  cex = 0.9,
  main = NULL
)

