
# test_tlt_student.R
# test on student dataset

library(caret)
data("GermanCredit")


Xtt <- GermanCredit
tr_idx <- createDataPartition(Xtt$Class, p = 0.7, list = F)
Xtrain <- Xtt[tr_idx,]; Xtest <- Xtt[-tr_idx,]

# ====
# tlt
tlt <- function(){
    tr_ctrl <- trainControl(method = "boot", number = 1,
                            verboseIter = TRUE, returnData = FALSE,
                            classProbs = TRUE, summaryFunction = twoClassSummary)
    tg <- expand.grid(nrounds = c(15),
                      eta = c(0.1),
                      max_depth        = seq(1, 10, by = 1),
                      min_child_weight = seq(5, 50, by = 5),
                      colsample_bytree = seq(0.8, 1, by = 0.1),
                      gamma            = 1)
    nrow(tg)
    mods <- TwoLevelTune(data = Xtrain, frml = "Class ~.",
                         prop_config = 0.2, tr_ctrl1 = tr_ctrl, tr_ctrl2 = tr_ctrl,
                         tg1 = tg, nrounds = seq(100, 1000, by = 100),
                         eta = c(0.1, 0.05, 0.01), nthread = 4, verbose = 1, metric = "ROC")
}


tlt.time <- system.time(tlt_mods <- tlt())


save(list = "tlt_mods", file = "R/tlt_mods.RData")

tlt.time # 104.51 seconds
tlt_pred <- predict(tlt_mods[[2]], subset(Xtest, select = -Class))
220 / sum(table(tlt_pred, Xtest$Class) )


# ====
# Exhaustive search
exh <- function(){
    
    tr_ctrl <- trainControl(method = "boot", number = 1,
                            verboseIter = TRUE, returnData = FALSE,
                            classProbs = TRUE, summaryFunction = twoClassSummary)
    
    tg <- expand.grid(nrounds = seq(100, 1000, by = 100),
                      eta = c(0.1, 0.05, 0.01),
                      max_depth        = seq(1, 10, by = 1),
                      min_child_weight = seq(5, 50, by = 5),
                      colsample_bytree = seq(0.8, 1, by = 0.1),
                      gamma            = 1)
    nrow(tg)
    
    xgb <- train(form = Class ~.,
                 data = Xtrain,
                 method = "xgbTree",
                 trControl = tr_ctrl,
                 tuneGrid = tg,
                 objective = "binary:logistic",
                 nthread = 4,
                 verbose = 1,
                 metric = "ROC")
    
    return(xgb)
}
exh.time <- system.time(exh_mod <- exh())
save(list = "exh_mod", file = "R/exh_mod.RData")
exh.time # 1202.94 secs
exh_pred <- predict(exh_mod, subset(Xtest, select = -G3))
Metrics::rmse(predicted = exh_pred, actual = Xtest$G3) # 1.42573

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
