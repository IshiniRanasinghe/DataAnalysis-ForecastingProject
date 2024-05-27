#Part1 subtask2 # PCA

#install the packages
library(readxl)
#import the dataset
dataSet <- read_excel("Whitewine_v6.xlsx")
#Remove 12th column from data set(Output column)
data_excluded_clomn12 <- dataSet[, -12]
#Function for find outliers using IQR
find_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  outliers <- x < lower_bound | x > upper_bound
  return(outliers)
}
# use find_outliers function to each columns of the data set
outlier_index <- apply(data_excluded_clomn12, 2, find_outliers)

outlier_rows <- rowSums(outlier_index) > 0
#scale the data set that cleaned
cleaned_data_step1 <- data_excluded_clomn12[!outlier_rows, ]

scaled_cleaned_data_step1 <- as.data.frame(scale(cleaned_data_step1))
#check the boxplot in step1
boxplot (scaled_cleaned_data_step1)
# Apply the outliers find function to the 2,3,4,6,9 column of the scaled_cleaned_data
outlier_column_2 <- find_outliers(scaled_cleaned_data_step1[[2]])

outlier_column_3 <- find_outliers(scaled_cleaned_data_step1[[3]])

outlier_column_4 <- find_outliers(scaled_cleaned_data_step1[[4]])

outlier_column_6 <- find_outliers(scaled_cleaned_data_step1[[6]])

outlier_column_9 <- find_outliers(scaled_cleaned_data_step1[[9]])
# Remove outliers from the scaled_cleaned_data dataframe for the specified columns
scaled_cleaned_data_step2 <- scaled_cleaned_data_step1[
  !outlier_column_2 &
    !outlier_column_3 &
    !outlier_column_4 &
    !outlier_column_6 &
    !outlier_column_9, 
]
# Apply the outliers detection function to the 3rd column of the cleaned_scaled_data
outlier_column_3_again <- find_outliers(scaled_cleaned_data_step2[[3]])
# Remove outliers from the cleaned_scaled_data dataframe for the third column
cleaned_scaled_data_last <- scaled_cleaned_data_step2[!outlier_column_3_again, ]

# Create a boxplot to visualize outliers for the specified columns after removal
boxplot(cleaned_scaled_data_last)

# Compute the Covariance Matrix
wine_cov <- cov(cleaned_scaled_data_last)
wine_cov

# Compute the Eigenvalues and eigenvectors
wine_eigen <- eigen(wine_cov)
str(wine_eigen)
#List of 2
#$ values : num [1:11] 3.47 1.449 1.186 1.029 0.966 ...
#$ vectors: num [1:11, 1:11] 0.13904 0.00614 0.0131 0.40921 0.34279 ...
#- attr(*, "class")= chr "eigen"

# Access the Eigenvalues
wine_eigen$values
#To access eigenvalues and eigenVectors separately
wine_eigen$vectors

#The Proportion of Variance Explained
PVE <- wine_eigen$values / sum(wine_eigen$values)
round(PVE,2) 

#choose 7 components
components <- 7
(wine_matrix <- wine_eigen$vectors[,1:components])
# Assign row and column names to the dataframe
phi <-- as.data.frame(wine_matrix)
row_nam <- c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")
col_nam <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")

rownames(phi) <- row_nam
colnames(phi) <- col_nam

print(phi)

# Calculate Principal Components scores
x=cleaned_scaled_data_last
PC1 <- as.matrix(x) %*% phi[,1]       
PC2 <- as.matrix(x) %*% phi[,2]
PC3 <- as.matrix(x) %*% phi[,3]      
PC4 <- as.matrix(x) %*% phi[,4]
PC5 <- as.matrix(x) %*% phi[,5]       
PC6 <- as.matrix(x) %*% phi[,6]
PC7 <- as.matrix(x) %*% phi[,7]       

# Create data frame with Principal Components scores
PC <- data.frame(State = rownames(cleaned_scaled_data_last), PC1, PC2,PC3,PC4,PC5,PC6,PC7)
# Assuming your data frame is named 'PC'
# Reset row names as a regular column
PC$State <- rownames(PC)

# Remove the row names
rownames(PC) <- NULL

# Now you can remove the State column
PC_without_state <- subset(PC, select = -State)

head(PC_without_state)

# Plot Principal Components for each State
library(ggplot2)
ggplot(PC_without_state, aes(PC1,PC2,PC3,PC4,PC5,PC6,PC7)) + 
  modelr::geom_ref_line(h = 0) +
  modelr::geom_ref_line(v = 0) +
  geom_text(aes(label = rownames(PC_without_state)), size = 3) +
  xlab("Principal Component 1") + 
  ylab("Principal Component 2") + 
  ggtitle("First 2 Principal Components of Wine Data")

# PVE plot
PVEplot <- qplot(1:11, PVE ) + geom_line() +
  xlab("Principal Component") +
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0,1)

cumPVE <- qplot(c(1:11), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab(NULL) + 
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1)
library(gridExtra)
grid.arrange(PVEplot, cumPVE, ncol = 8)

# e part-Automated Tools on PCA-based Data set
library(NbClust)
#Distance= euclidean
#clusterNo=NbClust(cleaned_scaled_data_last,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")
#Distance= manhattan
#clusterNo=NbClust(cleaned_scaled_data_last, distance="manhattan", min.nc=2,max.nc=10,method="kmeans",index="all")

#Elbow method
x= PC_without_state
y<- data$quality

library(factoextra)
fviz_nbclust(x, kmeans, method = 'wss')
library(factoextra)
#Gap Statistic Algorithm
fviz_nbclust(x, kmeans, method = 'gap_stat')
library(factoextra)
#Average Silhouette Method
fviz_nbclust(x, kmeans, method = 'silhouette')

#kmeans
x=PC_without_state

#cluster sizes
library(dplyr)
cluster_sizes <- cluster_assignments_simple %>%
  as.factor() %>%
  table()
cluster_sizes_df <- data.frame(cluster = names(cluster_sizes),
                               size = as.vector(cluster_sizes))
cluster_sizes_df
#K-means clustering with 2 clusters of sizes 1273, 886
kc2 <- kmeans(x,2)
kc2

#wss1 and wss2 
wss = kc2$withinss #7024.343 10657.264

#total of wss
wss_tot=kc2$tot.withinss #17681.61

#bss
bss = kc2$betweenss # 5458.482

#task h
k=2
# Assuming 'x' is your cleaned and scaled data
library(cluster)
library(factoextra)
x=PC_without_state
sil_width <- silhouette(kc2$cluster, dist(x))
# Calculate silhouette widths
sil <- silhouette(kc2$cluster, dist(x))
fviz_silhouette(sil)

avg_silhouette_width <- mean(sil[,"sil_width"])
cat("average silhouette width score: ",avg_silhouette_width,"\n")




#last task

# Assuming 'x' is your cleaned and scaled data
library(fpc)

# calculate the  Calinski-Harabasz Index
ch_index <- calinhara(x,kc2$cluster,2)

# Print the Calinski-Harabasz index value
cat("Calinski-Harabasz Index:", ch_index, "\n")



