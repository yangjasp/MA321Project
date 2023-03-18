########
#### Question 3 ----
########

source("DataCleaning.R")


#####
### A. Random forest to predict sale price
######

set.seed(123)

### Run model on  data
fit2 <- randomForest(SalePrice ~.,data = dplyr::select(house_data,
                                                       LotArea, 
                                                       LotConfig,
                                                       Condition1,
                                                       BldgType,
                                                       HouseStyle,
                                                       OverallQual,
                                                       YearBuilt,
                                                       X1stFlrSF,
                                                       KitchenQual,
                                                       SalePrice, OverallCondCat
), estimator = "cv")


##### Now estimate error using k-fold cv. We choose 10-fold
#####

## Step 1: Split data randomly into one of k groups
folds <- sample(rep(1:10, length.out = nrow(house_data)), replace = FALSE)
house_data$k <- folds


### Step 2: For each fold, run analysis and find 10-fold cv error rate
RMSE_vec_kfold <- rep(0, times = 10)
MAE_vec_kfold <- rep(0, times = 10)
for (i in 1:10){
  # spit training and test data
  training_data <- dplyr::filter(house_data, k == i)
  test_data <- dplyr::filter(house_data, k != i)
  
  # Run model
  fit2 <- randomForest(SalePrice ~.,data = dplyr::select(training_data,
                                                         LotArea, 
                                                         LotConfig,
                                                         Condition1,
                                                         BldgType,
                                                         HouseStyle,
                                                         OverallQual,
                                                         YearBuilt,
                                                         X1stFlrSF,
                                                         KitchenQual,
                                                         SalePrice, OverallCondCat
  ), estimator = "cv")
  
  # Generate predicted values for test data
  predictions <- predict(fit2, newdata = test_data)
  
  # Calculate MSE and store it in error_vec
  RMSE_vec_kfold[i] <- sqrt(mean((predictions - test_data$SalePrice)^2))
  MAE_vec_kfold[i] <- mean(abs(predictions - test_data$SalePrice))
  
}

##### Now find mean errors across all k iterations
mean(RMSE_vec_kfold) # 39995.81
mean(MAE_vec_kfold) # 25200.23


#### Step 3: Find bootstrap error rate

# We will also use B = 10, so run 10 bootstrap samples
bootstrap_samples <- list()
for (i in 1:10){
  bootstrap_samples[[i]] <- sample(1:nrow(house_data), 
                                   size = nrow(house_data), 
                                   replace = TRUE)
}

# Now for each bootstrap sample, run model and calculate errorr
size_of_test <- rep(0, times = 10) # initialize vec to hold test data size
RMSE_vec_bootstrap <- rep(0, times = 10) # initialize vec to hold error for each
MAE_vec_bootstrap <- rep(0, times = 10)

for (i in 1:10){
  training_data <- house_data[house_data$Id %in% bootstrap_samples[[i]],]
  test_data <- house_data[!(house_data$Id %in% bootstrap_samples[[i]]),]
  size_of_test[i] <- nrow(test_data)
  
  # Run model
  fit2 <- randomForest(SalePrice ~.,data = dplyr::select(training_data,
                                                         LotArea, 
                                                         LotConfig,
                                                         Condition1,
                                                         BldgType,
                                                         HouseStyle,
                                                         OverallQual,
                                                         YearBuilt,
                                                         X1stFlrSF,
                                                         KitchenQual,
                                                         SalePrice, OverallCondCat
  ), estimator = "cv")
  
  # Generate predicted values for test data
  predictions <- predict(fit2, newdata = test_data)
  
  # Calculate MSE and store it in error_vec
  RMSE_vec_bootstrap[i] <- sqrt(mean((predictions - test_data$SalePrice)^2))
  MAE_vec_bootstrap[i] <- mean(abs(predictions - test_data$SalePrice))
  
}

# See mean errors
mean(RMSE_vec_bootstrap) # 33240.84
mean(MAE_vec_bootstrap) # 21016.23

#######
#### B. CART: Tree-based regression 
#######

# Fit model
fit1 <- tree(SalePrice ~., data = dplyr::select(house_data,
                                                LotArea, 
                                                "LotConfiguration" = LotConfig,
                                                Condition1,
                                                "BuildingType" = BldgType,
                                                HouseStyle  ,
                                                "OverallQuality" = OverallQual,
                                                YearBuilt ,
                                                "FirstFlrSF" = X1stFlrSF,
                                                "KitchenQuality" = KitchenQual ,
                                                "SalePrice" = SalePrice, 
                                                "OverallCondition" = OverallCondCat 
))
fit1

summary(fit1)
plot(fit1)
text(fit1)

#### Perform 10-fold cross validation
####

# Step 0: Set a new seed
set.seed(234)

## Step 1: Split data randomly into one of k groups
folds <- sample(rep(1:10, length.out = nrow(house_data)), replace = FALSE)
house_data$k <- folds


### Step 2: For each fold, run analysis and find 10-fold cv error rate
RMSE_vec_kfold <- rep(0, times = 10)
MAE_vec_kfold <- rep(0, times = 10)
for (i in 1:10){
  # spit training and test data
  training_data <- dplyr::filter(house_data, k == i)
  test_data <- dplyr::filter(house_data, k != i)
  
  # Run model
  fit1 <- tree(SalePrice ~.,data = dplyr::select(training_data,
                                                         LotArea, 
                                                         LotConfig,
                                                         Condition1,
                                                         BldgType,
                                                         HouseStyle,
                                                         OverallQual,
                                                         YearBuilt,
                                                         X1stFlrSF,
                                                         KitchenQual,
                                                         SalePrice, OverallCondCat
  ))
  
  # Generate predicted values for test data
  predictions <- predict(fit1, newdata = test_data)
  
  # Calculate MSE and store it in error_vec
  RMSE_vec_kfold[i] <- sqrt(mean((predictions - test_data$SalePrice)^2))
  MAE_vec_kfold[i] <- mean(abs(predictions - test_data$SalePrice))
  
}

##### Now find mean errors across all k iterations
mean(RMSE_vec_kfold) # 49322.68
mean(MAE_vec_kfold) # 33722.26


###### Now find bootstrap error rate
######

# We will also use B = 10, so run 10 bootstrap samples
bootstrap_samples <- list()
for (i in 1:10){
  bootstrap_samples[[i]] <- sample(1:nrow(house_data), 
                                   size = nrow(house_data), 
                                   replace = TRUE)
}

# Now for each bootstrap sample, run model and calculate errorr
size_of_test <- rep(0, times = 10) # initialize vec to hold test data size
RMSE_vec_bootstrap <- rep(0, times = 10) # initialize vec to hold error for each
MAE_vec_bootstrap <- rep(0, times = 10)

for (i in 1:10){
  training_data <- house_data[house_data$Id %in% bootstrap_samples[[i]],]
  test_data <- house_data[!(house_data$Id %in% bootstrap_samples[[i]]),]
  size_of_test[i] <- nrow(test_data)
  
  # Run model
  fit1 <- tree(SalePrice ~.,data = dplyr::select(training_data,
                                                         LotArea, 
                                                         LotConfig,
                                                         Condition1,
                                                         BldgType,
                                                         HouseStyle,
                                                         OverallQual,
                                                         YearBuilt,
                                                         X1stFlrSF,
                                                         KitchenQual,
                                                         SalePrice, OverallCondCat
  ))
  
  # Generate predicted values for test data
  predictions <- predict(fit1, newdata = test_data)
  
  # Calculate MSE and store it in error_vec
  RMSE_vec_bootstrap[i] <- sqrt(mean((predictions - test_data$SalePrice)^2))
  MAE_vec_bootstrap[i] <- mean(abs(predictions - test_data$SalePrice))
  
}

# See mean errors
mean(RMSE_vec_bootstrap) # 45215.75
mean(MAE_vec_bootstrap) # 30833.37