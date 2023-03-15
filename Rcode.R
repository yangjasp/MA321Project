######
### MA321 Project -------
#######

####### Data --------
house_data <- read.csv("house-data.csv")

####### Libraries -----------
library(dplyr)
library(ggplot2)
library(nnet)
library(ipred)
library(tree)
library(randomForest)
library(ggsci)

####### Data cleaning --------

## 1. Make new column for house condition category
house_data <- house_data %>%
  dplyr::mutate(OverallCondCat = case_when(OverallCond <= 3 ~ "poor",
                                           OverallCond <= 6 ~ "average",
                                           OverallCond >6 ~ "good"),
                OverallCondGood = ifelse(OverallCondCat == "good", 1, 0))
table(house_data$OverallCondCat)

########
### Question 1 ---------------
########

# Continuing with the exploratory analysis, we are plotting LE in different ways

## Plot 1: Histogram of sale price
p1 <- ggplot(house_data, aes(SalePrice)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_bw()+
  xlab("Sale Price")+
  ylab("Frequency")+ 
  theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))

## Plot 2: boxplot by Health Spending quantiles
# Setting the quantiles
sale_price_quantiles <- quantile(house_data$SalePrice, c(0.25, 0.5, 0.75), na.rm = TRUE)
temp <- house_data %>%
  dplyr::mutate(SP_quantile = case_when(
    SalePrice < sale_price_quantiles[1] ~ "0-25%",
    SalePrice < sale_price_quantiles[2] ~ "25-50%",
    SalePrice < sale_price_quantiles[3] ~ "50-75%",
    SalePrice >= sale_price_quantiles[3] ~ "70-100%"))
# Plotting
p2 <- ggplot(temp, aes(x = SP_quantile, y = OverallCond, 
                       color = SP_quantile)) +
  geom_boxplot(show.legend = FALSE) +
  ggsci::scale_color_jama()+
  theme(axis.text.x = element_text(angle = 45))+
  theme_bw()+
  xlab("Sale price quantile")+
  ylab("Overall Condition") + 
  theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))

## Plot 3: Healthcare per capita (plus visualization of size per continent)
p3 <- ggplot(data = house_data, aes(x = OverallCond, 
                                 y =SalePrice))+
  geom_point(aes(size = log(X1stFlrSF), color = HouseStyle),
             show.legend = c(TRUE))  + 
  ggsci::scale_color_jama()+
  scale_x_log10() + 
  theme_bw()+
  ylab("Overall Condition") +
  xlab("Sale Price") +
  guides(size = "none") +
  theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))

# Plot in a grid with cowplot::plot_grid() and also give titles
top <- cowplot::plot_grid(p1, p2, labels = c("A. Histogram of Life Expectancy", "B. Life Expectancy by Healthcare Spending"),
                          hjust = -0.1)
bottom <- cowplot::plot_grid(p3, labels = c("C. Life Expectancy by Population and GDP per capita"),
                             hjust = -0.1)
cowplot::plot_grid(top, bottom, ncol = 1)


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

########
#### Question 3
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
