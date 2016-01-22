


# function to make multiple barplots on variable importances
# accepted models: OLS, RF, PCA, PLS



VIbar <- function(models, .horiz, .title){
    
    ols0 <- models[[1]]
    rf0 <- models[[2]]
    pca0 <- models[[3]]
    pls0 <- models[[4]]
    
    par(mfrow=c(2,3), las = 2)
    
    barplot(sort(summary(ols0)[[4]][-1,1]), main = "OLS", names.arg = rownames(summary(ols0)[[4]][-1]), horiz = .horiz)
    barplot(sort(rf0$importance[,1]), main = "RF %IncMSE", horiz = .horiz)
    barplot(sort(rf0$importance[,2]), main = "RF IncNodePurity", horiz = .horiz)
    barplot(sort(pca0$rotation[,"PC1"]), main = "PCA PC1", horiz = .horiz)
    barplot(sort(pca0$rotation[,"PC2"]), main = "PCA PC2", horiz = .horiz)
    barplot(sort(varImp(pls0)$importance$Overall), main = "PLS", names.arg = rownames(varImp(pls0)$importance), horiz = .horiz)
    
    #mtext(.title, outer = TRUE, line = -21, side = 3, adj )
}



# not working
ggVIbar <- function(models, .horiz){
    
    ols0 <- models[[1]]
    rf0 <- models[[2]]
    pca0 <- models[[3]]
    pls0 <- models[[4]]
    
    hp_order <- c("eta", "max_depth", "gamma", "colsample_bytree",
                  "min_child_weight", "nrounds", "Accuracy")
    
    plotdf <- data.frame(rbind(c(summary(ols0)[[4]][-1,1], NA),
                               c(rf0$importance[,1], NA),
                               c(rf0$importance[,2], NA),
                               pca0$rotation[,"PC1"],
                               pca0$rotation[,"PC2"],
                               c(varImp(pls0)$importance$Overall, NA)))
    
    plotdf <- cbind(c("OLS", "RF (%IncMSE)", "RF (IncNodePurity)",
                      "PCA (PC1)", "PCA (PC2)", "PLS"), plotdf)
    
    colnames(plotdf) <- c( "Model", hp_order)
    plotdf_melt <- melt(plotdf, "Model")
    
    g <- ggplot(plotdf_melt, aes(x = variable, y = value)) + geom_bar(stat = "identity") + facet_grid(.~ Model)
    g
    
    
}