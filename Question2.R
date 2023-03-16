#######
### Question 2: 
########

####
## A. 
####
## Multinomial logistic regression
fit1 <- nnet::multinom(OverallCondCat ~ ., data = dplyr::select(house_data,
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
), family = "binomial")


####
## B. 
####
house_data$OverallCondCat <- as.factor(house_data$OverallCondCat)

fit.tree <- tree(OverallCondCat ~ ., data = dplyr::select(house_data,
                                                          HouseStyle,
                                                          YearBuilt,
                                                          X1stFlrSF,
                                                          SalePrice, OverallCondCat))


fit.tree <- tree(OverallCondCat ~ ., data = dplyr::select(house_data,
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

## summary
summary(fit.tree)

## plot
plot(fit.tree)
text(fit.tree)