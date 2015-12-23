tune <- function(xtrain, ytrain, xtune = xtrain, ytune = ytrain,
                 model, trainCtrl.caret,
                 tuneGrid.caret, tuneGrid.rt,
                 metric, ...){
    
    require(caret)
    
    
    # intitial tuning to collect data
    tune.performances <- NULL
    
    for(i in seq(nrow(tuneGrid.rt))){
        if(i %% 10 == 0) cat(paste("Iteration ", i, " :: ", Sys.time(), "\n" ,sep = ""))
        mod <- caret::train(x = xtrain, y = ytrain,
                            method = model,
                            trControl = trainCtrl.caret,
                            tuneGrid = tuneGrid.caret
                            ## tuning the following:
                            ,min_child_weight = tuneGrid.manual$min_child_weight[i]
                            ,subsample        = tuneGrid.manual$subsample[i]
                            ,colsample_bytree = tuneGrid.manual$colsample_bytree[i])
        
        # measure performance on the tuning set
        mod.pred.class <- predict(mod, newdata = xtune, type = "raw")
        tab <- table(mod.pred.class, ytune)
        acc <- sum(diag((tab))) / sum(tab)
        tune.performances <- c(tune.performances, acc)
    }
    
    tune.df <- cbind(tune.performances, tuneGrid.rt)
    colnames(tune.df)[1] <- "metric"
    
    rt.mod <- lm(data = tune.df, metric ~.)
    
    print(summary(rt.mod))
    
    print(tune.df)
    
}

#====


trainCtrl <- trainControl(method = "cv",
                          number = 10,
                          repeats = 10)

tuneGrid.caret <- expand.grid(nrounds = c(15),
                              eta = c(0.1),
                              max_depth = c(1)
)

min_child_weight_grid <- seq(5, 50, by = 10)
subsample_grid        <- seq(0.5, 1, by = 0.2)
colsample_bytree_grid <- seq(0.2, 1, by = 0.1)

tuneGrid.manual <- expand.grid(min_child_weight = min_child_weight_grid,
                               subsample = subsample_grid,
                               colsample_bytree = colsample_bytree_grid)

nrow(tuneGrid.manual)

tune(xtrain = xtrain, ytrain = ytrain, xtune = xtune, ytune = ytune,
     model = "xgbTree", trainCtrl.caret = trainCtrl,
     tuneGrid.caret = tuneGrid.caret, tuneGrid.rt = tuneGrid.manual)