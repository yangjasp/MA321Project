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
MSE_vec_kfold <- rep(0, times = 10)
MAD_vec_kfold <- rep(0, times = 10)
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
  MSE_vec_kfold[i] <- mean((predictions - test_data$SalePrice)^2)
  MAD_vec_kfold[i] <- mean(abs(predictions - test_data$SalePrice))
  
}

##### Now find mean errors across all k iterations
mean(MSE_vec_kfold)
mean(MAD_vec_kfold)


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
MSE_vec_bootstrap <- rep(0, times = 10) # initialize vec to hold error for each
MAD_vec_bootstrap <- rep(0, times = 10)

for (i in 1:10){
  training_data <- house_data[house_data$Id %in% bootstrap_samples[i],]
  test_data <- house_data[!(house_data$Id %in% bootstrap_samples[i]),]
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
  MSE_vec_bootstrap[i] <- mean((predictions - test_data$SalePrice)^2)
  MAD_vec_bootstrap[i] <- mean(abs(predictions - test_data$SalePrice))
  
}


#######
#### A. CART: Tree-based regression 
#######

# b 
fit1 <- tree(SalePrice ~., data = dplyr::select(house_data,
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
fit1

summary(fit1)
plot(fit1)
text(fit1)

### Now perform 