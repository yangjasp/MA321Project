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

########
#### Data cleaning --------
########

## 1. Make new column for house condition category
house_data <- house_data %>%
  dplyr::mutate(OverallCondCat = case_when(OverallCond <= 3 ~ "poor",
                                           OverallCond <= 6 ~ "average",
                                           OverallCond >6 ~ "good"),
                OverallCondGood = ifelse(OverallCondCat == "good", 1, 0))
table(house_data$OverallCondCat)

## 2. Merge two house style categories for plotting: 2.5 stories
house_data <- house_data %>%
  dplyr::mutate(HouseStyleCat = case_when(HouseStyle %in% c("1.5Fin", "1.5Unf") ~ "1.5 Story",
                                          HouseStyle == "1Story" ~ "1 Story",
                                          HouseStyle == "2Story" ~ "2 Story",
                                          HouseStyle %in% c("2.5Fin", "2.5Unf") ~ "2.5 Story",
                                          HouseStyle %in% c("SFoyer", "SLvl") ~ "Split Level/Foyer"))

## 3. Add 1st + 2nd floor square feet
house_data <- house_data %>%
  dplyr::mutate(TotalSF = X1stFlrSF + X2ndFlrSF)

########
### Question 1 ---------------
########
  ####
  #### Explore data through plots and tables

## Plot 1: Histogram of sale price
p1 <- ggplot(house_data, aes(SalePrice)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  scale_x_log10(labels = label_comma(), 
                breaks = c(0, 50000, 100000, 200000, 300000, 400000, 500000)) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=1))+
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
p2 <- ggplot(temp, aes(x = HouseStyleCat, y = SalePrice, 
                       color = HouseStyleCat)) +
  geom_boxplot(show.legend = FALSE) +
  ggsci::scale_color_jama()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_y_log10(labels = label_comma(), 
                breaks = c(0, 50000, 100000, 200000, 300000, 400000, 500000))+
  theme_bw()+
  xlab("House Style")+
  ylab("Sale Price") + 
  theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))

## Plot 3: Healthcare per capita (plus visualization of size per continent)
p3 <- ggplot(data = house_data, aes(x = TotalSF, 
                                 y = SalePrice))+
  geom_point(aes(color = HouseStyleCat),
             show.legend = c(TRUE))  + 
  ggsci::scale_color_jama()+
  scale_x_log10(breaks = c(500,1000,2000,3000))+
  scale_y_log10(labels = label_comma(), 
                breaks = c(0, 50000, 100000, 200000, 300000, 400000, 500000))+
  theme_bw()+
  ylab("Sale Price") +
  xlab("Total Square Feet") +
  guides(size = "none") +
  theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))

# Plot in a grid with cowplot::plot_grid() and also give titles
top <- cowplot::plot_grid(p1, p2, labels = c("A. Histogram of Sale Price", "B. Overall Condition by Sale Price"),
                          hjust = -0.1)
bottom <- cowplot::plot_grid(p3, labels = c("C. "),
                             hjust = -0.1)
cowplot::plot_grid(top, bottom, ncol = 1)



#######
#### Question 4
#######

### What variables would be important 


### 
