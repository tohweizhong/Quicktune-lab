
# http://archive.ics.uci.edu/ml/datasets/Diabetic+Retinopathy+Debrecen+Data+Set#

library(caret)

Xtt <- read.csv("data/dr.csv")
Xtt$class <- factor(Xtt$class)


tr_ctrl <- trainControl(method = "boot", number = 1,
                        verboseIter = TRUE, returnData = FALSE)

tg0 <- expand.grid(nrounds          = seq(100, 900, by = 100),
                   eta              = c(0.1, 0.05, 0.01),
                   max_depth        = seq(1, 7, by = 2),
                   min_child_weight = seq(5, 30, by = 5),
                   colsample_bytree = seq(0.8, 1, by = 0.1),
                   gamma            = seq(0, 16, by = 2))
nrow(tg0) # 17496


xgb0 <- train(data = Xtt, class ~.,
              method = "xgbTree",
              trControl = tr_ctrl,
              tuneGrid = tg0,
              objective = "binary:logistic",
              nthread = 4,
              verbose = 1)

# grid data
grid_dr <- xgb0$results
grid_dr <- subset(grid_dr, select = -c(AccuracySD, KappaSD))
save(file = "data/RData/grid_dr.RData", grid_dr)
