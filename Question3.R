########
#### Question 3 ----
########

fit2 <- glm(log(SalePrice) ~ .,
            data = dplyr::select(house_data,
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

par(mfrow = c(2,2))
plot(fit2)



#########
#### Question 3 --------------
#########

#####
### A. Random forest to predict sale price
######

set.seed(123)
## Split into train and test
test.indices <- sample(1:nrow(house_data), size = nrow(house_data)*.2, replace = FALSE)

training_data <- house_data[-test.indices,]
test_data <- house_data[test.indices,]

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


plot(fit2)
text(fit2)

mypredict <- function(object, newdata){
  predict(object, newdata = newdata, type = c("response"))
}

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
