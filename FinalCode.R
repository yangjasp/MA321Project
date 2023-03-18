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

## 4. Change NAs that are actually a category "None" (as opposed to missing).
# Confirmed this from data dictionary
house_data <- house_data %>%
  dplyr::mutate(Alley = ifelse(is.na(Alley), "None", Alley),
                BsmtQual = ifelse(is.na(BsmtQual), "None", BsmtQual),
                BsmtCond = ifelse(is.na(BsmtCond), "None", BsmtCond),
                GarageType = ifelse(is.na(GarageType), "None", GarageType),
                GarageCond = ifelse(is.na(GarageCond), "None", GarageCond),
                PoolQC = ifelse(is.na(PoolQC), "None", PoolQC),
                Fence = ifelse(is.na(Fence), "None", Fence))

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
house_data_numeric_IDs <- house_data_numeric$Id
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
# View(cor(house_data_numeric, use = "pairwise.complete.obs"))


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

house_data_cat_original <- dplyr::select(house_data_cat, -c(HouseStyleCat,OverallCondCat,
                                                            OverallCondGood))


max_cat_size_vec <- rep(0, times = ncol(house_data_cat_original))

for (i in 1:ncol(house_data_cat_original)){
  max_cat_size_vec[i] <- max(table(house_data_cat_original[,i]))/nrow(house_data_numeric)
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

########
#### Question 3 ----
########


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

#######
#### Question 4
#######

####
library(factoextra)
library(cluster)
library(NbClust)

### What variables would be important to group the houses into clusters
### We will use K-means clustering

###
### Step 1. Set seed
set.seed(123)

###
### Step 2. Run kmeans

house_data_numeric_noNA <- dplyr::filter(bind_cols(as.data.frame(house_data_numeric_IDs), house_data_numeric), !(is.na(`Lot frontage`)) &
                                           !(is.na(`Masonry area`)))

# Extract IDs for this dataset, then remove the column
house_data_numeric_noNA_IDs <- house_data_numeric_noNA$house_data_numeric_IDs
house_data_numeric_noNA <- subset(house_data_numeric_noNA, 
                                  select = -house_data_numeric_IDs)

house_data_numeric_noNA_scaled <- as.data.frame(scale(as.matrix(house_data_numeric_noNA)))

my.km <- kmeans(as.matrix(house_data_numeric_noNA_scaled), centers = 3, nstart = 20)

table(my.km$cluster)

house_data_numeric_noNA$cluster <- as.factor(my.km$cluster)


###
### 3. Plot
p4 <- ggplot(house_data_numeric_noNA, aes(x = `Sale Price`, `Living area sq. feet`))+
  geom_point(aes(color = cluster)) +
  theme_bw() +
  ggsci::scale_color_jama()
p4

# For next plots, merge categorical data back in
house_data_numeric_noNA$Id <- house_data_numeric_noNA_IDs

house_data_cat$Id <- house_data$Id # since no rows were removed from cat
house_data_noNA <- dplyr::left_join(house_data_numeric_noNA, house_data_cat,
                                    by = "Id")


p5 <- ggplot(house_data_numeric_noNA, aes(x = `Sale Price`, `Living area sq. feet`))+
  geom_point(aes(color = cluster)) +
  scale_x_log10(labels = label_comma())+
  theme_bw() +
  theme(plot.margin = unit(c(1.3,0.5,0.5,1.3), "cm")) +
  ggsci::scale_color_jama()

p6 <- ggplot(house_data_noNA, aes(x = `HouseStyle`, `Year built`))+
  geom_point(aes(color = cluster), position = "jitter") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 80), 
        plot.margin = unit(c(1,0.5,0.5,1.3), "cm"))+
  ggsci::scale_color_jama()

p7 <- ggplot(house_data_noNA, aes(x = `GarageType`, y = `Overall condition`))+
  geom_point(aes(color = cluster), position = position_jitter(width = 0.1)) +
  xlab("Garage Type") +
  scale_y_log10()+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 80),
        plot.margin = unit(c(1,0.5,0.5,1.3), "cm"))+
  ggsci::scale_color_jama()

p8 <- fviz_cluster(my.km, data = subset(house_data_numeric_noNA, select = -cluster),
                   main = "", geom = "point")

cowplot::plot_grid(p5, p6, p7, p8, labels = c("A. Clusters by Sale Price and Living Area SF", "B. Clusters by Neighborhood and Year Built", "C. Clusters by Garage Type and Overall Condition", "D. Clusters by First Two Principal Components"),
                   hjust = -0.1, vjust = 1.3, label_size = 12)


options(scipen=999)
p9 <- fviz_nbclust(house_data_numeric_noNA, kmeans, method = "wss")
p9

p10 <- fviz_nbclust(house_data_numeric_noNA,
                    kmeans,
                    method = "silhouette")
p10

gap_stat <- clusGap(subset(house_data_numeric_noNA, select = -cluster), FUNcluster = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
p11 <- fviz_gap_stat(gap_stat)
p11

top <- cowplot::plot_grid(p10, p11, labels = c("A. Average Silhoutte Method", "       B. Gap Statistic Method"),
                          hjust = -1.2)
bottom <- cowplot::plot_grid(p9, labels = c("C. Elbow Method"),
                             hjust = -6.0,
                             vjust = 2.5)
cowplot::plot_grid(top, bottom, ncol = 1)

