library(tidyverse)
library(useful)
library(h2o)

buildings <- as_tibble(read.csv("Predict the damage to a building/buildings_final.CSV"))

#buildings$X <- NULL #remove the index column
#buildings$damage_grade <- as.factor(buildings$damage_grade_id)
train <- subset(buildings, !is.na(buildings$buildings_all.damage_grade))
test <- subset(buildings, is.na(buildings$buildings_all.damage_grade))

train_splits <- split(train, train$buildings_all.district_id)
test_splits <- split(test, test$buildings_all.district_id)

localH2O <- h2o.init(nthreads = -1)
h2o.init()

y.dep <- 81
x.indep <- c(2:19, 32:79, 82)
train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)
ntrees_opt <- c(1200)
maxdepth_opt <- c(16)
hyper_parameters <- list(ntrees=ntrees_opt,
                         max_depth=maxdepth_opt)
sub_gbm <- as_tibble()
gbm_modller <- function(district) {
  district_id <- unique(district$buildings_all.district_id)
  message("Working on district: ", district_id)
  train.h2o <- as.h2o(district)
  r <- h2o.runif(train.h2o)
  trainHex.split <- h2o.splitFrame(train.h2o, ratios=.75)
  grid <- h2o.grid("gbm", hyper_params = hyper_parameters,
                 seed = 123,   
                 y = y.dep, x = x.indep,
                 distribution="AUTO",
                 training_frame = trainHex.split[[1]],
                 validation_frame = trainHex.split[[2]])
  print(grid)
  # print out the mse for all of the models
  model_ids <- grid@model_ids
  mse <- vector(mode="numeric", length=0)
  grid_models <- lapply(model_ids, function(model_id) { model = h2o.getModel(model_id) })
  for (i in 1:length(grid_models)) {
   print(sprintf("mse: %f", h2o.mse(grid_models[[i]])))
   mse[i] <- h2o.mse(grid_models[[i]])
  }


  best_id <- model_ids[order(mse,decreasing=F)][1]
  				 
  fit.best <- h2o.getModel(model_id = best_id[[1]])
  
  message("Best Model is: ", best_id) 
    
  district_id_str <- toString(district_id)
  district_test <- test_splits[[district_id_str]]
  test.h2o <- as.h2o(district_test)
  predict.gbm <- as.data.frame(h2o.predict(fit.best, test.h2o))
  sub_gbm <- data.frame(building_id = district_test$buildings_all.building_id,
                                        damage_grade = predict.gbm$predict)
  
  file_name <- paste0("sub_gbm_",district_id_str,".csv")
  message(file_name)
  write.csv(sub_gbm, file = file_name, quote = FALSE, row.names=FALSE)
  #sub_gbm
}


lapply(train_splits, gbm_modller)
gbm_modller(train_splits$`7`)
gbm_modller(train_splits$`9`)
gbm_modller(train_splits$`10`)
gbm_modller(train_splits$`11`)
gbm_modller(train_splits$`12`)
gbm_modller(train_splits$`13`)
gbm_modller(train_splits$`20`)
gbm_modller(train_splits$`21`)
gbm_modller(train_splits$`22`)
gbm_modller(train_splits$`23`)
gbm_modller(train_splits$`24`)
gbm_modller(train_splits$`25`)
gbm_modller(train_splits$`26`)
gbm_modller(train_splits$`27`)
gbm_modller(train_splits$`28`)
gbm_modller(train_splits$`29`)
gbm_modller(train_splits$`30`)
gbm_modller(train_splits$`31`)
gbm_modller(train_splits$`35`)
gbm_modller(train_splits$`36`)
gbm_modller(train_splits$`37`)
gbm_modller(train_splits$`38`)
gbm_modller(train_splits$`39`)
gbm_modller(train_splits$`40`)
gbm_modller(train_splits$`43`)
gbm_modller(train_splits$`44`)
gbm_modller(train_splits$`45`)
gbm_modller(train_splits$`46`)
gbm_modller(train_splits$`47`)
gbm_modller(train_splits$`48`)
gbm_modller(train_splits$`51`)
#write.csv(sub_gbm, file = "sub_gbm_dist.csv", quote = FALSE, row.names=FALSE)
test_building_ids <- test$buildings_all.building_id
gbm_all <- read.csv("final_output_2.csv")
final_output <- gbm_all[match(test_building_ids, gbm_all$building_id),]
write.csv(final_output, file = "final_output_2.csv", quote = FALSE, row.names=FALSE)

# system.time(
#   dlearning.model <- h2o.deeplearning(y = y.dep,
#                                       x = x.indep,
#                                       training_frame = train.h2o,
#                                       epoch = 100,
#                                       hidden = c(150,150),
#                                       activation = "Rectifier",
#                                       seed = 1122
#   )
# )
# 
# predict.dl <- as.data.frame(h2o.predict(dlearning.model, test.h2o))
# sub_dl <- data.frame(building_id = test$buildings_all.building_id, damage_grade = predict.dl$predict)
# write.csv(sub_dl, file = "sub_dl.csv", quote = FALSE, row.names=FALSE)