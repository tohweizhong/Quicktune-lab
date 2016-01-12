
# test_tlt_adult.R
# test on adult dataset

library(caret)
data(iris)

Xtt <- iris[union(which(iris$Species == "setosa"), which(iris$Species == "versicolor")),]
Xtt$Species <- droplevels(Xtt$Species)
tr_idx <- createDataPartition(Xtt$Species, p = 0.7, list = F)
Xtrain <- Xtt[tr_idx,]; Xtest <- Xtt[-tr_idx,]



# ====
# tlt
tlt <- function(){
    tr_ctrl <- trainControl(method = "boot", number = 1,
                            verboseIter = TRUE, returnData = FALSE,
                            classProbs = TRUE)
    tg <- expand.grid(nrounds = c(15),
                      eta = c(0.1),
                      max_depth        = seq(1, 10, by = 1),
                      min_child_weight = seq(5, 50, by = 5),
                      colsample_bytree = seq(0.8, 1, by = 0.1),
                      gamma            = seq(0, 120, by = 20))
    nrow(tg)
    mods <- TwoLevelTune(data = Xtrain, frml = "Species ~.",
                         prop_config = 0.05, tr_ctrl1 = tr_ctrl, tr_ctrl2 = tr_ctrl,
                         tg1 = tg, nrounds = seq(100, 1000, by = 100),
                         eta = c(0.1, 0.05, 0.01), nthread = 4, verbose = 1, metric = "Accuracy",
                         objective = "binary:logistic")
}
tlt.time <- system.time(tlt_mod <- tlt())

tlt.time # 189.14 seconds
tlt_pred <- predict(tlt_mod[[2]], subset(Xtest, select = -Species))
table(tlt_pred, Xtest$Species)

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
                      gamma            = seq(0, 120, by = 20))
    nrow(tg)
    
    xgb <- train(form = Species ~.,
                 data = Xtrain,
                 method = "xgbTree",
                 trControl = tr_ctrl,
                 tuneGrid = tg,
                 objective = "binary:logistic",
                 nthread = 4,
                 verbose = 1,
                 metric = "Accuracy")
}
exh.time <- system.time(exh_mod <- exh())
