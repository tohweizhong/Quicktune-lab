
# Comparision between whether scaling the hyperparameters range
# vs. not scaling

load("grid_adult_unscaled")

grid_adult_unscaled <- grid_adult
grid_adult_scaled <- data.frame(scale(grid_adult, center = TRUE, scale = TRUE))

summary(grid_adult_unscaled)
summary(grid_adult_scaled)

# create data partition for each dataset
train_idx <- createDataPartition(grid_adult_unscaled$Accuracy, p = 0.7, groups = 5, list = FALSE)
train_unscaled <- grid_adult_unscaled[train_idx,]
test_unscaled <- grid_adult_unscaled[-train_idx,]

train_idx <- createDataPartition(grid_adult_scaled$Accuracy, p = 0.7, groups = 5, list = FALSE)
train_scaled <- grid_adult_scaled[train_idx,]
test_scaled <- grid_adult_scaled[-train_idx,]

# linear models
lm_unscaled <- lm(data = train_unscaled[, -8], Accuracy ~.)
lm_scaled <- lm(data = train_scaled[, -8], Accuracy ~.)

summary(lm_unscaled)
summary(lm_scaled)

# predictions
lm_unscaled_pred <- predict(lm_unscaled, newdata = test_unscaled)
Metrics::rmse(actual = test_unscaled$Accuracy, predicted = lm_unscaled_pred)

lm_scaled_pred <- predict(lm_scaled, newdata = test_scaled)
Metrics::rmse(actual = test_scaled$Accuracy, predicted = lm_scaled_pred)
