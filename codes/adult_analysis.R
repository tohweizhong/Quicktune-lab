
library(caret)
library(randomForest)

load(file = "data/RData/grid_adult.RData")

# take a look
featurePlot(x = grid_adult[1:6], y = grid_adult$Accuracy, type = c("p", "smooth"))

# Want to look at variable importance
# Try the following models:
# @ OLS
# @ RF
# @ PCA
# @ PLS

# OLS
ols0 <- lm(data = grid_adult[, -8], Accuracy ~.)
summary(ols0)
barplot(summary(ols0)[[4]][-1,1],
        names.arg = rownames(summary(ols0)[[4]][-1]),
        main = "OLS")

# RF
rf0 <- randomForest(data = grid_adult[,-8], Accuracy ~., importance = TRUE, ntree = 300)
varImpPlot(rf0)
barplot(rf0$importance[,1], main = "RF %IncMSE")
barplot(rf0$importance[,2], main = "RF IncNodePurity")

# PCA
pca0 <- prcomp(grid_adult[,-8], scale = TRUE, center = TRUE)
print(pca0)
plot(pca0)
summary(pca0)
biplot(pca0)
dotplot(sort(pca0$rotation[,"PC1"]))
barplot(sort(pca0$rotation[,"PC1"]), main = "PCA PC1")
barplot(sort(pca0$rotation[,"PC2"]), main = "PCA PC2")

# PLS (using caret)
tr_ctrl <- trainControl(method = "boot", number = 1, verboseIter = TRUE, returnData = FALSE)

pls0 <- train(data = grid_adult[,-8], Accuracy ~.,
              method = "pls",
              metric = "RMSE",
              tuneGrid = expand.grid(ncomp = 1:6),
              trControl = trainControl(classProbs = FALSE),
              nthread = 4,
              verbose = 1)
ggplot(pls0)
varImp(pls0)
barplot(varImp(pls0)$importance$Overall, main = "PLS", names.arg = rownames(varImp(pls0)$importance))

# ====
# barplots

# function to make multiple barplots on variable importances

VIbar <- function(models, .horiz){
    
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
}

VIbar(models = list(ols0, rf0, pca0, pls0), .horiz = TRUE)
