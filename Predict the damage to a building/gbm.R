library(tidyverse)
library(useful)
library(h2o)

buildings <- as_tibble(read.csv("Predict the damage to a building/buildings_final.csv"))

#buildings$X <- NULL #remove the index column
#buildings$damage_grade <- as.factor(buildings$damage_grade_id)
train <- subset(buildings, !is.na(buildings$buildings_all.damage_grade))
test <- subset(buildings, is.na(buildings$buildings_all.damage_grade))

localH2O <- h2o.init(nthreads = -1)
h2o.init()
train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)

y.dep <- 81
x.indep <- c(2:19, 32:79)
ntrees_opt <- c(900, 1000, 1100, 1200)
maxdepth_opt <- c(10, 12, 14, 16)
hyper_parameters <- list(ntrees=ntrees_opt,
                         max_depth=maxdepth_opt)
r <- h2o.runif(train.h2o)
trainHex.split <- h2o.splitFrame(train.h2o, ratios=.8)

gbm.model <- h2o.gbm(y=y.dep,
                     x=x.indep,
                     nfolds = 5,
                     training_frame = trainHex.split[[1]],
                     validation_frame = trainHex.split[[2]],
                     ntrees = 1200, 
                     max_depth = 16,
                     learn_rate = 0.001,
                     learn_rate_annealing=0.995,
                     sample_rate = 0.8,
                     col_sample_rate = 0.8,
                     col_sample_rate_per_tree = 0.8,
                     seed = 1122)

# grid <- h2o.grid("gbm", hyper_params = hyper_parameters,
#                  y = y.dep, x = x.indep,
#                  distribution="AUTO",
#                  training_frame = trainHex.split[[1]],
#                  validation_frame = trainHex.split[[2]])
# grid
# 
# # print out the mse for all of the models
# model_ids <- grid@model_ids
# mse <- vector(mode="numeric", length=0)
# grid_models <- lapply(model_ids, function(model_id) { model = h2o.getModel(model_id) })
# for (i in 1:length(grid_models)) {
#   print(sprintf("mse: %f", h2o.mse(grid_models[[i]])))
#   mse[i] <- h2o.mse(grid_models[[i]])
# }
# 
# 
# best_id <- model_ids[order(mse,decreasing=F)][1]
# best_id

fit.best <- gbm.model #h2o.getModel(model_id = best_id[[1]])
h2o.varimp(fit.best)
RMPSE<- function(predicted, actual) {
  rmpse <- sqrt(mean((actual/predicted-1)^2))
  return(list(metric = "RMPSE", value = rmpse))
}
predict.gbm <- as.data.frame(h2o.predict(fit.best, test.h2o))

sub_gbm <- data.frame(building_id = test$buildings_all.building_id, damage_grade = predict.gbm$predict)
write.csv(sub_gbm, file = "sub_gbm_3.csv", quote = FALSE, row.names=FALSE)
