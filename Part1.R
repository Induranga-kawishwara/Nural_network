library(readxl)
library(dplyr)
library(fpc)
library(MASS)
library(ggplot2)
library(ggcorrplot)
library(caret)
library(flexclust)
library(factoextra)
library(NbClust)
library(caret)
library(ggfortify)
library(FactoMineR)
library(cluster)


# Read the XLSX file by opening it. 


Vehicles_dataset <- read_excel("F://SEMESTER 02//5DATA001C.2 Machine Learning and Data Mining//CW//MyCW//vehicles.xlsx")
 


# Delete the first column.

Vehicles_dataset <- Vehicles_dataset[-1]


# Examine the data type to determine which columns are strings.


string_cols <- sapply(Vehicles_dataset, class) == "character"

#Use the subset function to eliminate the string column(s).

data <- Vehicles_dataset[, !string_cols]

View(data)

boxplot(data, main = "Before Outlier Removal", outcol="red")


# Before removing the outliers, count the rows and columns and print the results.

rows_of_dataset <- nrow(data)
columns_of_dataset <- ncol(data)

# print the results
cat("Rows to delete before outliers   :", rows_of_dataset, "\n")
cat("columns to delete before outliers:", columns_of_dataset, "\n")



# Create a function to remove outliers from a single column using the boxplot method.

outliers_remove <- function(x) {
  bp <- boxplot.stats(x)$stats
  x[x < bp[1] | x > bp[5]] <- NA
  return(x)
}

# Apply the function to each data frame column.

whithout_outliers_dataset <- apply(data, 2, outliers_remove)

# Remove any rows with missing values

whithout_outliers_dataset <- na.omit(whithout_outliers_dataset)


# After removing the outliers, count the rows and columns and print the results.

rows_of_dataset <- nrow(whithout_outliers_dataset)
columns_of_dataset <- ncol(whithout_outliers_dataset)

# print the results
cat("Rows to delete before outliers   :", rows_of_dataset, "\n")
cat("Columns to delete before outliers:", columns_of_dataset, "\n")


boxplot(whithout_outliers_dataset,main = "After Outlier Removal")


# Scale up the data set.

scaled_Vehicles_dataset <- scale(whithout_outliers_dataset)
head(scaled_Vehicles_dataset)

boxplot(scaled_Vehicles_dataset)






# Nbclust method

# Set the random seed to 1234 for reproducibility

set.seed(1234)

NBcluster <- NbClust(scaled_Vehicles_dataset, min.nc = 2,max.nc = 10, method = "kmeans")

# table(NBcluster$Best.n[1,])
barplot(table(NBcluster$Best.n[1,]),
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen")



# elbow method

fviz_nbclust(scaled_Vehicles_dataset,kmeans,method = "wss")

# silhouette method

fviz_nbclust(scaled_Vehicles_dataset,kmeans,method = "silhouette")

# gap static method

fviz_nbclust(scaled_Vehicles_dataset,kmeans,method = "gap_stat")





# 2 clusters

k2 <-kmeans(scaled_Vehicles_dataset, 2)
k2
autoplot(k2,scaled_Vehicles_dataset,frame=TRUE)

# When k=2, Extract relevant information.

# Cluster centers
cluster_centers <- k2$centers

# Clustered results
cluster_assignments <- k2$cluster 

# Between-cluster sum of squares
BSSk2 <- k2$betweenss 

# Within-cluster sum of squares
WSSk2 <- k2$tot.withinss 

# Total sum of squares
TSSk2 <- BSSk2 + WSSk2 

# BSS/TSS ratio
BSS_TSS_ratiok2 <- BSSk2 / TSSk2 

# Percentage of variance explained
percent_vark2 <- round(BSS_TSS_ratiok2 * 100, 3) 

# Output results
cat("Cluster centers:\n", cluster_centers, "\n\n")
cat("Cluster assignments:\n", cluster_assignments, "\n\n")
cat("BSS/TSS ratio: ", round(BSS_TSS_ratiok2, 3), "\n\n")
cat("BSS: ", round(BSSk2, 3), "\n\n")
cat("WSS: ", round(WSSk2, 3), "\n\n")
cat("Percentage of variance explained: ", percent_vark2, "%\n")






#3 clusters 

k3 <-kmeans(scaled_Vehicles_dataset, 3)
k3
autoplot(k3,scaled_Vehicles_dataset,frame=TRUE)


# When k=3, Extract relevant information
# Cluster centers
cluster_centers <- k3$centers

# Clustered results
cluster_assignments <- k3$cluster 

# Between-cluster sum of squares
BSSk3 <- k3$betweenss 

# Within-cluster sum of squares
WSSk3 <- k3$tot.withinss 

# Total sum of squares
TSSk3 <- BSSk3 + WSSk3 

# BSS/TSS ratio
BSS_TSS_ratiok3 <- BSSk3 / TSSk3 

# Percentage of variance explained
percent_vark3 <- round(BSS_TSS_ratiok3 * 100, 3) 

# Output results
cat("Cluster centers:\n", cluster_centers, "\n\n")
cat("Cluster assignments:\n", cluster_assignments, "\n\n")
cat("BSS/TSS ratio: ", round(BSS_TSS_ratiok3, 3), "\n\n")
cat("BSS: ", round(BSSk3, 3), "\n\n")
cat("WSS: ", round(WSSk3, 3), "\n\n")
cat("Percentage of variance explained: ", percent_vark3, "%\n")





# When Fit k-means model with k=2

k <- 2

part01_kmeans_model_2 <- kmeans(scaled_Vehicles_dataset, centers = k, nstart = 25)

# Generate silhouette plot

silhouette_plot2 <- silhouette(part01_kmeans_model_2$cluster, dist(scaled_Vehicles_dataset))

# Determine the typical silhouette's width.

avg_silhouette_width <- mean(silhouette_plot2[, 3])

# Plot the silhouette plot

plot(silhouette_plot2, main = paste0("Silhouette Plot for k =", k),
     xlab = "Silhouette Width", ylab = "Cluster", border = NA)

# As a vertical line, add the average silhouette width.

abline(v = avg_silhouette_width, lty = 2, lwd = 2, col = "red")





# When Fit k-means model with k=3

k <- 3
part01_kmeans_model_3 <- kmeans(scaled_Vehicles_dataset, centers = k, nstart = 25)

# Generate silhouette plot

silhouette_plot3 <- silhouette(part01_kmeans_model_3$cluster, dist(scaled_Vehicles_dataset))

# Determine the typical silhouette's width.

avg_silhouette_width <- mean(silhouette_plot3[, 3])

# Plot the silhouette plot

plot(silhouette_plot3, main = paste0("Silhouette Plot for k =", k),
     xlab = "Silhouette Width", ylab = "Cluster", border = NA)

# As a vertical line, add the average silhouette width.

abline(v = avg_silhouette_width, lty = 2, lwd = 2, col = "red")





####################################################################################################################################################################################################################




# Conduct a PCA analysis.

pca <- prcomp(scaled_Vehicles_dataset)


# Print the eigenvectors and eigenvalues

print(summary(pca))

# Calculate the principal component (PC) cumulative score.

pca_var <- pca$sdev^2
pca_var_prop <- pca_var / sum(pca_var)
pca_var_cumprop <- cumsum(pca_var_prop)

# Cumulative plot score for each PC

plot(pca_var_cumprop, xlab = "Principal Component", ylab = "Explained Cumulative Proportion of Variance",
     ylim = c(0, 1), type = "b")

# Make a new converted dataset with attributes representing the principal components.

pca_trans <- predict(pca, newdata = scaled_Vehicles_dataset)

# Select computers with a minimum cumulative score of over 92%.

selected_pcs <- which(pca_var_cumprop > 0.92)
transformed_data <- pca_trans[, selected_pcs]
boxplot(transformed_data)

View(transformed_data)






#Nbclust method

set.seed(1234)

NBcluster <- NbClust(transformed_data, min.nc = 2,max.nc = 10, method = "kmeans")

barplot(table(NBcluster$Best.n[1,]),
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen after PCA2")

#elbow method
fviz_nbclust(transformed_data,kmeans,method = "wss")

#silhouette method
fviz_nbclust(transformed_data,kmeans,method = "silhouette")

#gap static method
fviz_nbclust(transformed_data,kmeans,method = "gap_stat")





# Perform k-means clustering with k=2
set.seed(1234)

kmeans_model_2 <- kmeans(transformed_data, centers = 2, nstart = 25)

# Print the k-means output

print(kmeans_model_2)
autoplot(kmeans_model_2,transformed_data,frame=TRUE)


cat("For k=2:\n")
# Calculate the within-cluster sum of squares (WSS)
wss_2 <- sum(kmeans_model_2$withinss)
# Print the WSS
cat("Within-cluster sum of squares (WSS): ", wss_2, "\n")


# Calculate the between-cluster sum of squares (BSS)
bss_2 <- sum(kmeans_model_2$size * dist(rbind(kmeans_model_2$centers, colMeans(transformed_data)))^2)
# Print the BSS
cat("Between-cluster sum of squares (BSS): ", bss_2, "\n")


# Calculate the total sum of squares (TSS)
tss_2 <- sum(dist(transformed_data)^2)
# Print the TSS
cat("Total sum of squares (TSS): ", tss_2, "\n")


# Calculate the ratio of BSS to TSS
bss_tss_ratio_2 <- bss_2 / tss_2
# Print the ratio of BSS to TSS
cat("Ratio of BSS to TSS: ", bss_tss_ratio_2, "\n\n")






# Perform k-means clustering with k=3
set.seed(1234)
kmeans_model_3 <- kmeans(transformed_data, centers = 3, nstart = 25)

# Print the k-means output

print(kmeans_model_3)
autoplot(kmeans_model_3,transformed_data,frame=TRUE)

cat("For k=3:\n")
# Calculate the within-cluster sum of squares (WSS)
wss_3 <- sum(kmeans_model_3$withinss)
# Print the WSS
cat("Within-cluster sum of squares (WSS): ", wss_3, "\n")


# Calculate the between-cluster sum of squares (BSS)
bss_3 <- sum(kmeans_model_3$size * dist(rbind(kmeans_model_3$centers, colMeans(transformed_data)))^2)
# Print the BSS
cat("Between-cluster sum of squares (BSS): ", bss_3, "\n")


# Calculate the total sum of squares (TSS)
tss_3 <- sum(dist(transformed_data)^2)
# Print the TSS
cat("Total sum of squares (TSS): ", tss_3, "\n")


# Calculate the ratio of BSS to TSS
bss_tss_ratio_3 <- bss_3 / tss_3
# Print the ratio of BSS to TSS
cat("Ratio of BSS to TSS: ", bss_tss_ratio_3, "\n")









# Construct a k-means model with k=2

k <- 2
kmeans_model_2 <- kmeans(transformed_data, centers = k, nstart = 25)

# Generate silhouette plot

silhouette_plot <- silhouette(kmeans_model_2$cluster, dist(transformed_data))

# Determine the typical silhouette's width.

avg_silhouette_width <- mean(silhouette_plot[, 3])

# Plot the silhouette plot

plot(silhouette_plot, main = paste0("Silhouette Plot for k =", k),
     xlab = "Silhouette Width", ylab = "Cluster", border = NA)

# As a vertical line, add the average silhouette width.

abline(v = avg_silhouette_width, lty = 2, lwd = 2, col = "red")







# Construct a k-means model with k=3

k <- 3
kmeans_model_3 <- kmeans(transformed_data, centers = k, nstart = 25)

# Generate silhouette plot

silhouette_plot <- silhouette(kmeans_model_3$cluster, dist(transformed_data))

# Determine the typical silhouette's width.

avg_silhouette_width <- mean(silhouette_plot[, 3])

# Plot the silhouette plot

plot(silhouette_plot, main = paste0("Silhouette Plot for k =", k),
     xlab = "Silhouette Width", ylab = "Cluster", border = NA)

# As a vertical line, add the average silhouette width.

abline(v = avg_silhouette_width, lty = 2, lwd = 2, col = "red")







# Define a function to calculate the Calinski-Harabasz index for a given K-means clustering result and data

calinski_harabasz_pca2 <- function(cluster_result, data) {
  
  # Extract the number of clusters from the clustering result
  
  k2 <- length(unique(cluster_result$cluster))
  
  # Extract the number of rows in the data
  n2 <- nrow(data)
  
  # Extract the between-cluster sum of squares (BSS) and within-cluster sum of squares (WSS) from the clustering result
  
  BSS2 <- cluster_result$betweenss
  WSS2 <- cluster_result$tot.withinss
  
  # Calculate the Calinski-Harabasz index
  
  ch_index2 <- ((n2 - k2) / (k2 - 1)) * (BSS2 / WSS2)
  
  # Return the Calinski-Harabasz index
  
  return(ch_index2)
}

# Call the calinski_harabasz_pca function to calculate the index for a specific K-means clustering result and data

ch_index_pca_2 <- calinski_harabasz_pca2(kmeans_model_2, transformed_data)

# Print the calculated index to the console for interpretation

cat("The Calinski-Harabasz index for the K-means (k=2) clustering result is:", ch_index_pca_2, "\n")








# Define a function to calculate the Calinski-Harabasz index for a given K-means clustering result and data

calinski_harabasz_pca3 <- function(cluster_result, data) {
  
  # Extract the number of clusters from the clustering result
  
  k3 <- length(unique(cluster_result$cluster))
  
  # Extract the number of rows in the data
  
  n3 <- nrow(data)
  
  # Extract the between-cluster sum of squares (BSS) and within-cluster sum of squares (WSS) from the clustering result
  
  BSS3 <- cluster_result$betweenss
  WSS3 <- cluster_result$tot.withinss
  
  # Calculate the Calinski-Harabasz index
  
  ch_index3 <- ((n3 - k3) / (k3 - 1)) * (BSS3 / WSS3)
  
  # Return the Calinski-Harabasz index
  
  return(ch_index3)
}

# Call the calinski_harabasz_pca function to calculate the index for kmeans_model_3 and transformed_data

ch_index_pca_3 <- calinski_harabasz_pca3(kmeans_model_3, transformed_data)

# Print the calculated index to the console for interpretation

cat("The Calinski-Harabasz index for the K-means (k=3) clustering result is:", ch_index_pca_3, "\n")




