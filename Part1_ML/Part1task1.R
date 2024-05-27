#Part1 subtask1

#install the packages
library(readxl)
#import the dataset
dataSet <- read_excel("Whitewine_v6.xlsx")
summary(data)
boxplot(dataSet)
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
head(cleaned_scaled_data_last)

#task2

#install the " NbClust" package [ NBclust Method]
library(NbClust)
#Number of cluster centers
#Distance= euclidean
clusterNo=NbClust(cleaned_scaled_data_last,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")
#Distance= manhattan
clusterNo=NbClust(cleaned_scaled_data_last, distance="manhattan", min.nc=2,max.nc=10,method="kmeans",index="all")
#Distance= maximum
clusterNo=NbClust(cleaned_scaled_data_last, distance="maximum", min.nc=2,max.nc=10,method="kmeans",index="all")

#Elbow method
x= cleaned_scaled_data_last
y<- data$quality
library(factoextra)
fviz_nbclust(x, kmeans, method = 'wss')
library(factoextra)
#Gap Statistic Algorithm
fviz_nbclust(x, kmeans, method = 'gap_stat')
library(factoextra)
#Average Silhouette Method
fviz_nbclust(x, kmeans, method = 'silhouette')

#task 3
x=cleaned_scaled_data_last
y=data$quality   #the $ sign is associated with the label of that particular column 
kc <- kmeans(x,2)
#wss1 and wss2 
wss = kc$withinss
wss
#total of wss
wss=kc$tot.withinss
wss
#bss
bss = kc$betweenss
#TSS
TSS = kc$tot.withinss + kc$betweenss
TSS

#task 4
k=2
# Assuming 'x' is your cleaned and scaled data
library(cluster)
# Perform kmeans clustering with k=2
kmeans_data <- kmeans(x, centers = 2, nstart = 10)
# Calculate silhouette widths
sil_width <- silhouette(kmeans_data$cluster, dist(x))
sil <- silhouette(kmeans_data$cluster, dist(x))
fviz_silhouette(sil)

avg_silhouette_width <- mean(sil[,"sil_width"])
cat("average silhouette width score: ",avg_silhouette_width,"\n")

