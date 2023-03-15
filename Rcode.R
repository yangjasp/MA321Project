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

## Plot 1: Histogram of life expectancy
p1 <- ggplot(house_data, aes(x = OverallCond)) +
  geom_histogram(stat = "count") +
  theme_bw()+
  xlab("Overall House Condition")+
  ylab("Frequency")+ 
  theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))

## Plot 2: boxplot by Health Spending quantiles
# Setting the quantiles
health_expend_quantiles <- quantile(LE_data$health_expend_perc_gdp, c(0.25, 0.5, 0.75), na.rm = TRUE)
temp <- LE_data %>%
  dplyr::mutate(HE_quantile = case_when(
    health_expend_perc_gdp < health_expend_quantiles[1] ~ "0-25%",
    health_expend_perc_gdp < health_expend_quantiles[2] ~ "25-50%",
    health_expend_perc_gdp < health_expend_quantiles[3] ~ "50-75%",
    health_expend_perc_gdp >= health_expend_quantiles[3] ~ "70-100%")) %>%
  dplyr::filter(!(is.na(health_expend_perc_gdp)))
# Plotting
p2 <- ggplot(temp, aes(x = HE_quantile, y = life_expectancy, 
                       color = HE_quantile)) +
  geom_boxplot(show.legend = FALSE) +
  ggsci::scale_color_jama()+
  theme(axis.text.x = element_text(angle = 45))+
  theme_bw()+
  xlab("Health expenditure per capita (% GDP) quantile")+
  ylab("Life Expectancy (Years)") + 
  theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))

## Plot 3: Healthcare per capita (plus visualization of size per continent)
p3 <- ggplot(data = LE_data, aes(x = gdp_pc, 
                                 y = life_expectancy))+
  geom_point(aes(size = pop_total, color = Continent),
             show.legend = c(TRUE))  + 
  ggsci::scale_color_jama()+
  scale_x_log10() + 
  theme_bw()+
  ylab("Life Expectancy (Years)") +
  xlab("GDP Per Capita ($)") +
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
#########