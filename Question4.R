#######
#### Question 4
#######

#### Load data
source("DataCleaning.R")
source("Question1.R")

####
library(factoextra)


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

my.km <- kmeans(as.matrix(house_data_numeric_noNA_scaled), centers = 5, nstart = 20)

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

