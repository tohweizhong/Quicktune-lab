
plot(data = grid_dr, Accuracy ~ eta)
plot(data = grid_dr, Accuracy ~ nrounds)
plot(data = grid_dr, Accuracy ~ max_depth)
plot(data = grid_dr, Accuracy ~ gamma)
plot(data = grid_dr, Accuracy ~ colsample_bytree)
plot(data = grid_dr, Accuracy ~ min_child_weight)

ols0 <- lm(data = grid_dr[, -8], Accuracy ~.)
summary(ols0)
barplot(summary(ols0)[[4]][-1,1])

rf0 <- randomForest(data = grid_dr[,-8], Accuracy ~.,
                    importance = TRUE, ntree = 300)
varImpPlot(rf0)

pca0 <- prcomp(grid_dr[,-8], scale = TRUE, center = TRUE)
print(pca0)
plot(pca0)
summary(pca0)
biplot(pca0)

barplot(pca0$rotation[,"PC1"])


# max_depth and gamma most important
# the higher gamma is, the less accurate
# the higher max_depth is, the more accurate

tr_ctrl <- trainControl(method = "boot", number = 1,
                        verboseIter = TRUE, returnData = FALSE)

pls0 <- train(data = grid_dr[,-8], Accuracy ~.,
              method = "pls",
              metric = "RMSE",
              tuneGrid = expand.grid(ncomp = 1:6),
              trControl = trainControl(classProbs = FALSE),
              nthread = 4,
              verbose = 1)

ggplot(pls0)
featurePlot(x = grid_dr[1:6], y = grid_dr$Accuracy, type = c("p", "smooth"))
