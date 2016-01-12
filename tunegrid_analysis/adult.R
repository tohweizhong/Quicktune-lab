
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



tr_ctrl <- trainControl(method = "boot", number = 1,
                        verboseIter = TRUE, returnData = FALSE)

tg0 <- expand.grid(nrounds          = seq(100, 900, by = 100),
                   eta              = c(0.1, 0.05, 0.01),
                   max_depth        = seq(1, 7, by = 2),
                   min_child_weight = seq(5, 30, by = 5),
                   colsample_bytree = seq(0.8, 1, by = 0.1),
                   gamma            = seq(0, 70, by = 10))
nrow(tg0) # 15552


xgb0 <- train(data = Xtrain, income ~.,
              method = "xgbTree",
              trControl = tr_ctrl,
              tuneGrid = tg0,
              objective = "binary:logistic",
              nthread = 4,
              verbose = 1)

# save grid data
grid_adult <- xgb0$results
grid_adult <- subset(grid_adult, select = -c(AccuracySD, KappaSD))
save(file = "data/RData/grid_adult.RData", grid_adult)
