
library(caret)
library(randomForest)

load(file = "data/RData/grid_adult.RData")

# mean center and scale
grid_adult <- data.frame(scale(grid_adult, center = TRUE, scale = TRUE))

# take a look
featurePlot(x = grid_adult[1:6], y = grid_adult$Accuracy, type = c("p", "smooth"))

# Cross-validation to do predictions
train_idx <- createDataPartition(grid_adult$Accuracy, p = 0.7, groups = 5, list = FALSE)
train <- grid_adult[train_idx,]
test <- grid_adult[-train_idx,]

# Want to look at variable importance
# Try the following models:
# @ OLS
# @ RF
# @ PCA
# @ PLS

# OLS
ols0 <- lm(data = train[, -8], Accuracy ~.)
summary(ols0)
barplot(summary(ols0)[[4]][-1,1],
        names.arg = rownames(summary(ols0)[[4]][-1]),
        main = "OLS")

save(list = "ols0", file = "data/RData/adult_ols0.RData")

ols0_pred <- predict(ols0, newdata = test)

sqrt(mean((ols0_pred - test$Accuracy) ^ 2))


# RF
rf0 <- randomForest(data = grid_adult[,-8], Accuracy ~., importance = TRUE, ntree = 300)
varImpPlot(rf0)
barplot(rf0$importance[,1], main = "RF %IncMSE")
barplot(rf0$importance[,2], main = "RF IncNodePurity")

save(list = "rf0", file = "data/RData/adult_rf0.RData")

# PCA
pca0 <- prcomp(grid_adult[,-8])
print(pca0)
plot(pca0)
summary(pca0)
biplot(pca0)
dotplot(sort(pca0$rotation[,"PC1"]))
barplot(sort(pca0$rotation[,"PC1"]), main = "PCA PC1")
barplot(sort(pca0$rotation[,"PC2"]), main = "PCA PC2")

save(list = "pca0", file = "data/RData/adult_pca0.RData")


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

save(list = "pls0", file = "data/RData/adult_pls0.RData")


# ====
# barplots


VIbar(models = list(ols0, rf0, pca0, pls0), .horiz = TRUE, .title = "adult")
ggVIbar(models = list(ols0, rf0, pca0, pls0), .horiz = TRUE)





# ====




library(Standard)
str(grid_adult)
summary(grid_adult)

GetUnique(subset(grid_adult, select = c(eta, max_depth, gamma, colsample_bytree, min_child_weight, nrounds)))


# OLS
ols <- lm(data = grid_adult[, -8],
          Accuracy ~ (eta + max_depth + gamma + colsample_bytree + min_child_weight + nrounds)^2)

stepmod <- step(lm(data = grid_adult[,-8], Accuracy ~.), direction = "both")




tg0 <- expand.grid(nrounds          = seq(100, 900, by = 100),
                   eta              = c(0.1, 0.05, 0.01),
                   max_depth        = seq(1, 7, by = 2),
                   min_child_weight = seq(5, 30, by = 5),
                   colsample_bytree = seq(0.8, 1, by = 0.1),
                   gamma            = seq(0, 70, by = 10))



means <- apply(tg0, MARGIN = 2, FUN = function(x){
    x %>% mean
})

tg1 <- apply(tg0, MARGIN = 2, FUN = function(x){
    m <- mean(x)
    x <- x + m
    return(x)
})s
