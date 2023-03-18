#######
### Question 2: 
########

####
## A. 
####
## Multinomial logistic regression
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
mydata <- (round(coef(fit1),4))
View(confint(fit1)[,1,1])
confint(fit1)[,2,1]
mydata2 <- data.frame(t(mydata))
mydata2$names <- row.names(mydata2)
mydata2$goodL <- paste("(", round(confint(fit1)[,1,2],3))
mydata2$goodU <- paste(", ", round(confint(fit1)[,2,2],3), ")")
mydata2$poorL <- paste("(", round(confint(fit1)[,1,1],3))
mydata2$poorU <- paste(", ",round(confint(fit1)[,2,1],3), ")")
mydata2 <- mydata2[,c(3,1,4,5,2,6,7)]
mydata2 <- mydata2 %>% unite("good",c("good","goodL","goodU"), sep = " ")
mydata2 <- mydata2 %>% unite("poor",c("poor","poorL","poorU"), sep = " ")
flextable(mydata2)


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