tune <- function(xtrain, ytrain, xtune = xtrain, ytune = ytrain,
                 model, trainCtrl.caret,
                 tuneGrid.caret, tuneGrid.rt,
                 metric, ...){
    
    require(caret)
    
    
    # intitial tuning to collect data
    tune.performances <- NULL
    
    for(i in seq(nrow(tuneGrid.rt))){
        
        mod <- caret::train(x = xtrain, y = train,
                            method = model,
                            trControl = trainCtrl.caret,
                            tuneGrid = tuneGrid.caret,
                            ...)
        
        # measure performance on the tuning set
        mod.pred.class <- predict(mod, newdata = xtune, type = "raw")
        tab <- table(mod0.pred.class, ytune)
        acc <- sum(diag((tab))) / sum(tab)
        tune.performances <- c(tune.performances, acc)
    }
    
    tune.df <- cbind(tune.performances, tuneGrid.rt)
    colnames(tune.df)[1] <- "metric"
    
    rt.mod <- lm(data = tune.df, metric ~.)
    
    summary(rt.mod)
}