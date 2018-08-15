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
x.indep <- c(2, 4:5, 16, 33:38, 51:56)
ntrees_opt <- c(1000)
maxdepth_opt <- c(12)
hyper_parameters <- list(ntrees=ntrees_opt,
                         max_depth=maxdepth_opt)
sub_gbm <- as_tibble()

gbm_modller <- function(district) {
  district_id <- unique(district$buildings_all.district_id)
  message("Working on district: ", district_id)
  train.h2o <- as.h2o(district)
  r <- h2o.runif(train.h2o)
  trainHex.split <- h2o.splitFrame(train.h2o, ratios=.75)
  gbm.model <- h2o.gbm(y=y.dep,
                       x=x.indep,
                       #nfolds = 5,
                       training_frame = trainHex.split[[1]],
                       validation_frame = trainHex.split[[2]],
                       ntrees = 900, 
                       max_depth = 8,
                       learn_rate = 0.01,
                       #learn_rate_annealing=0.995,
                       #sample_rate = 0.8,
                       #col_sample_rate = 0.8,
                       #col_sample_rate_per_tree = 0.8,
                       seed = 1122)
  fit.best <- gbm.model #h2o.getModel(model_id = best_id[[1]])
  # h2o.varimp(fit.best)
  # RMPSE<- function(predicted, actual) {
  #   rmpse <- sqrt(mean((actual/predicted-1)^2))
  #   return(list(metric = "RMPSE", value = rmpse))
  # }
  district_test <- test_splits[[toString(district_id)]]
  test.h2o <- as.h2o(district_test)
  predict.gbm <- as.data.frame(h2o.predict(fit.best, test.h2o))
  sub_gbm <- merge(sub_gbm, data.frame(building_id = district_test$buildings_all.building_id,
                                        damage_grade = predict.gbm$predict))
  sub_gbm
}


sub_gbm <- lapply(train_splits, gbm_modller)
# data <- tibble(building_id = sub_gbm$`7`$building_id, damage_grade = sub_gbm$`7`$damage_grade)
# data <- data %>% full_join(tibble (building_id = sub_gbm$`9`$building_id, damage_grade = sub_gbm$`9`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`10`$building_id, damage_grade = sub_gbm$`10`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`11`$building_id, damage_grade = sub_gbm$`11`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`12`$building_id, damage_grade = sub_gbm$`12`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`13`$building_id, damage_grade = sub_gbm$`13`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`20`$building_id, damage_grade = sub_gbm$`20`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`21`$building_id, damage_grade = sub_gbm$`21`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`22`$building_id, damage_grade = sub_gbm$`22`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`23`$building_id, damage_grade = sub_gbm$`23`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`24`$building_id, damage_grade = sub_gbm$`24`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`25`$building_id, damage_grade = sub_gbm$`25`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`26`$building_id, damage_grade = sub_gbm$`26`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`27`$building_id, damage_grade = sub_gbm$`27`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`28`$building_id, damage_grade = sub_gbm$`28`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`29`$building_id, damage_grade = sub_gbm$`29`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`30`$building_id, damage_grade = sub_gbm$`30`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`31`$building_id, damage_grade = sub_gbm$`31`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`35`$building_id, damage_grade = sub_gbm$`35`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`36`$building_id, damage_grade = sub_gbm$`36`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`37`$building_id, damage_grade = sub_gbm$`37`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`38`$building_id, damage_grade = sub_gbm$`38`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`39`$building_id, damage_grade = sub_gbm$`39`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`40`$building_id, damage_grade = sub_gbm$`40`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`43`$building_id, damage_grade = sub_gbm$`43`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`44`$building_id, damage_grade = sub_gbm$`44`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`45`$building_id, damage_grade = sub_gbm$`45`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`46`$building_id, damage_grade = sub_gbm$`46`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`47`$building_id, damage_grade = sub_gbm$`47`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`48`$building_id, damage_grade = sub_gbm$`48`$damage_grade))
# data <- data %>% full_join(tibble (building_id = sub_gbm$`51`$building_id, damage_grade = sub_gbm$`51`$damage_grade))


write.csv(data, file = "dist.csv", quote = FALSE, row.names=FALSE)

target_ids <- test$