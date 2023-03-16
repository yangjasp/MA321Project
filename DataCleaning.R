######
### MA321 Project: Data Cleaning File --------------
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