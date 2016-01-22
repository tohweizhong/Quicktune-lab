
# test_tlt_student.R
# test on student dataset

library(caret)

Xtt <- read.csv("data/student-mat.csv", header = T, stringsAsFactors = F, strip.white = T, sep = ";")
tr_idx <- createDataPartition(Xtt$G3, p = 0.7, list = F)
Xtrain <- Xtt[tr_idx,]; Xtest <- Xtt[-tr_idx,]

# ====
# tlt
tlt <- function(){
    tr_ctrl <- trainControl(method = "boot", number = 1,
                            verboseIter = TRUE, returnData = FALSE)
    
    tg <- expand.grid(nrounds = c(15),
                      eta = c(0.1),
                      max_depth        = seq(1, 10, by = 1),
                      min_child_weight = seq(5, 50, by = 5),
                      colsample_bytree = seq(0.8, 1, by = 0.1),
                      gamma            = 1)
    nrow(tg)
    mods <- TwoLevelTune(data = Xtrain, frml = "G3 ~.",
                         prop_config = 0.15, tr_ctrl1 = tr_ctrl, tr_ctrl2 = tr_ctrl,
                         tg1 = tg, nrounds = seq(100, 1000, by = 100),
                         eta = c(0.1, 0.05, 0.01), nthread = 4, verbose = 1, metric = "RMSE",
                         objective = "reg:linear")
    return(mods)
}
tlt.time <- system.time(tlt_mods <- tlt())
save(list = "tlt_mods", file = "R/tlt_mods_student.RData")

tlt.time # 104.51 seconds
tlt_pred <- predict(tlt_mods[[2]], subset(Xtest, select = -G3))
Metrics::rmse(predicted = tlt_pred, actual = Xtest$G3) # 1.615632


# ====
# Exhaustive search
exh <- function(){
    
    tr_ctrl <- trainControl(method = "boot", number = 1,
                            verboseIter = TRUE, returnData = FALSE)
    
    tg <- expand.grid(nrounds = seq(100, 1000, by = 100),
                      eta = c(0.1, 0.05, 0.01),
                      max_depth        = seq(1, 10, by = 1),
                      min_child_weight = seq(5, 50, by = 5),
                      colsample_bytree = seq(0.8, 1, by = 0.1),
                      gamma            = 1)
    nrow(tg)
    
    xgb <- train(form = G3 ~.,
                 data = Xtrain,
                 method = "xgbTree",
                 trControl = tr_ctrl,
                 tuneGrid = tg,
                 objective = "reg:linear",
                 nthread = 4,
                 verbose = 1,
                 metric = "RMSE")
    
    return(xgb)
}
exh.time <- system.time(exh_mod <- exh())
save(list = "exh_mod", file = "R/exh_mod_student.RData")
exh.time # 1202.94 secs
exh_pred <- predict(exh_mod, subset(Xtest, select = -G3))
Metrics::rmse(predicted = exh_pred, actual = Xtest$G3) # 1.42573

# ====
# tlt + 1x PopulateNext() + grid search

pn <- function(){
    
    load("testing_tlt/RData/tlt_mods_student.RData")
    tg <- PopulateNext(tg0 = tlt_mods[[2]]$results[1:7], xgb = tlt_mods[[2]])
    
    tr_ctrl <- trainControl(method = "boot", number = 1,
                            verboseIter = TRUE, returnData = FALSE)
    nrow(tg)
    
    xgb <- train(form = G3 ~.,
                 data = Xtrain,
                 method = "xgbTree",
                 trControl = tr_ctrl,
                 tuneGrid = tg,
                 objective = "reg:linear",
                 nthread = 4,
                 verbose = 1,
                 metric = "RMSE")
}
pn.time <- system.time(pn_mod <- pn())
save(list = "pn_mod", file = "R/pn_mod_student.RData")

pn.time # 139.36 seconds

pn_pred <- predict(pn_mod, subset(Xtest, select = -G3))
Metrics::rmse(predicted = pn_pred, actual = Xtest$G3) # 1.418791


# ====
# ====

# post mortem

tlt_mods[[2]]$bestTune
exh_mod$bestTune

tlt_df <- tlt_mods[[2]]$results
exh_df <- exh_mod$results



tlt_df <- tlt_df[order(tlt_df$RMSE, decreasing = FALSE),]
exh_df <- exh_df[order(exh_df$RMSE, decreasing = FALSE),]
head(tlt_df); head(exh_df)

summary(exh_df$RMSE)
