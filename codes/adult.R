
library(caret)

Xtt <- read.csv("data/adult.csv", header = T, stringsAsFactors = F, strip.white = T)

## Try to rename values of the response variable to legal R variable names
## Doing predictions using caret pkg demands this
Xtt$income <- unlist(sapply(Xtt$income, FUN = function(x){
    if(x == ">50K") return("MoreThan50K")
    else if (x == "<=50K") return("NotMoreThan50K")
}))
Xtt$income <- factor(Xtt$income)

tr_idx <- createDataPartition(Xtt$income, p = 0.7, list = F)
Xtrain <- Xtt[tr_idx,]; Xtest <- Xtt[-tr_idx,]


tr_ctrl <- trainControl(method = "cv", number = 5, repeats = 5)



tg0 <- expand.grid(nrounds          = seq(100, 1500, by = 100),
                   eta              = c(0.1, 0.05, 0.03, 0.01, 0.005, 0.001),
                   max_depth        = seq(1, 10, by = 1),
                   min_child_weight = seq(5, 50, by = 5),
                   colsample_bytree = seq(0.5, 1, by = 0.1),
                   gamma            = seq(0, 20, by = 1))

xgb0 <- train(data = Xtrain, income ~.,
              method = "xgbTree",
              trControl = tr_ctrl,
              tuneGrid = tg0,
              objective = "binary:logistic",
              nthread = 4)

# [xgb0] Best parameter config from first round of tuning
bst0 <- xgb0$finalModel$tuneValue

# [xgb0] tuning dataset (for RegressTune)
tune_df0 <- xgb0$results

# [xgb0] Save
save(file = "scripts_WZ/bst0.RData", bst0)
save(file = "Scripts_WZ/xgb0.RData", xgb0)
save(file = "Scripts_WZ/tune_df0.RData", tune_df0)