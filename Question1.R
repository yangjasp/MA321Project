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

#######
## Data Cleaning ---------
#######
source("DataCleaning.R")

########
### Question 1 ---------------
########
  ####
  #### Explore data through plots and tables

######
## Plot 1: Histogram of sale price
######
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
house_data$OverallCondCat <- factor(house_data$OverallCondCat, 
                                    levels = c("good", "average", "poor"))
p3 <- ggplot(data = house_data, aes(x = GrLivArea, 
                                 y = SalePrice))+
  geom_point(aes(color = OverallCondCat),
             show.legend = c(TRUE))  + 
  ggsci::scale_color_jama()+
  scale_x_log10(breaks = c(500,1000,2000,3000))+
  scale_y_log10(labels = label_comma(), 
                breaks = c(0, 50000, 100000, 200000, 300000, 400000, 500000))+
  theme_bw()+
  ylab("Sale Price") +
  xlab("Above Ground Square Feet") +
  guides(size = "none") +
  theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))

# Plot in a grid with cowplot::plot_grid() and also give titles
top <- cowplot::plot_grid(p1, p2, labels = c("A. Histogram of Sale Price", "B. Sale Price by House Style"),
                          hjust = -0.1)
bottom <- cowplot::plot_grid(p3, labels = c("C. Sale Price and Above Ground Square Feet"),
                             hjust = -0.1)
cowplot::plot_grid(top, bottom, ncol = 1)

######
### Plot 2: Correlation of continuous variables
######

# Step 1: Select numeric columns and also remove ID 
class_vec <- rep("character", times = ncol(house_data))
for (i in 1:ncol(house_data)){
  class_vec[i] <- class(house_data[,i])
}

house_data_numeric <- house_data[,class_vec == "integer"]
house_data_numeric <- subset(house_data_numeric, select = -Id)

# Step 2: Select columns that aren't 90% or more of zeroes
perc_zero_vec <- rep(0, times = ncol(house_data_numeric))

for (i in 1:ncol(house_data_numeric)){
  perc_zero_vec[i] <- table(house_data_numeric[,i] == 0)[2]/nrow(house_data_numeric)
}

perc_zero_vec[is.na(perc_zero_vec)] <- 0 # If no zeroes, was NA. Make it 0

house_data_numeric <- house_data_numeric[,perc_zero_vec < 0.9]

# Update Names
names(house_data_numeric) <- c("Lot frontage", "Lot area", "Overall quality",
                               "Overall condition", "Year built", "Masonry area",
                               "Basement sq. feet", "First floor sq. feet",
                               "Second floor sq. feet", "Living area sq. feet",
                               "Full baths", "Bedrooms above ground", 
                               "Kitchens above ground", "No. rooms above ground",
                               "Fireplaces", "Garage area", "Month sold", 
                               "Year sold", "Sale Price")

# Now plot correlations with corrplot

#corrplot(cor(house_data_numeric, use = "pairwise.complete.obs"),
         #method = "color", 
         #addCoef.col="black",  
         #number.cex=0.5) # this had text labes with cor coefficients

corrplot(cor(house_data_numeric, use = "pairwise.complete.obs"),
         tl.col='black')

# Look at correlations for reporting
View(cor(house_data_numeric, use = "pairwise.complete.obs"))


######
#### Table 1: 
######
    ## Note that this is only the code to produce the results of the table
    ## We made the table itself in word


# Number of continuous or ordinal variables (excluding ID):
num_numeric <- ncol(house_data_numeric) + sum(perc_zero_vec > 0.9) # because we removed these cols
perc_numeric <- num_numeric/(ncol(house_data) - 4) # subtract ID column and three we created when counting
num_numeric
perc_numeric


# Number of categorical variables
num_cat <- (ncol(dplyr::select(house_data, -c(HouseStyleCat,OverallCondCat,
                                OverallCondGood))) - 1) - num_numeric
perc_cat <- num_cat/(ncol(house_data) - 4) # sutract ID col and 3 we created

# Categorical variables with over 90% in one category
house_data_cat <- house_data[,class_vec != "integer"]

house_data_cat <- dplyr::select(house_data_cat, -c(HouseStyleCat,OverallCondCat,
                             OverallCondGood))


max_cat_size_vec <- rep(0, times = ncol(house_data_cat))

for (i in 1:ncol(house_data_cat)){
  max_cat_size_vec[i] <- max(table(house_data_cat[,i]))/nrow(house_data_numeric)
}

table(max_cat_size_vec > 0.9)

#######
### Supplemental figure 1
#######

S1 <- ggplot(house_data, aes(OverallCond)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  scale_x_continuous(labels = label_comma(), 
                breaks = 1:10) +
  theme_bw()+
  xlab("Overall Condition")+
  ylab("Count")+ 
  theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))

table(house_data$OverallCond == 5)/nrow(house_data)



