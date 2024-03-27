############################ Objective 1 #############################3
install.packages("stringi")
install.packages("reshape2")
install.packages("melt")
install.packages("NbClust")
#libraries
library(readxl)
library(tidyverse)
library(NbClust)

#loading the dataset
data<-read_excel("C:/Users/E wis/Desktop/multi assignment/CM 3052_project data (1).xlsx")

# Display the first few rows of the dataset
head(data)
# Get summary statistics for the dataset
summary(data)

str(data)

null_values <- sum(is.null(data))

# Create a correlation matrix
correlation_matrix <- cor(data)

# Display the correlation matrix
correlation_matrix

# Visualize correlations with a heatmap
library(ggplot2)
library(reshape2)

melted_corr_matrix <- melt(correlation_matrix)
ggplot(melted_corr_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  theme_minimal() +
  scale_fill_gradient2(low = "green", mid = "white", high = "blue", midpoint = 0) +
  labs(title = "Correlation Heatmap")


#removing the first column which is well water sample_no
data<-data[-1]

# Performing of PCA
#obtaining results as per the correlation matrix
pca_result <- prcomp(data,scale=TRUE)
pca_result

# Summary of PCA
summary(pca_result)

# Explained variance
pca_result$importance

# Scree plot
plot(pca_result)

# Loadings
pca_result$rotation

# Scores
pca_result$x
biplot(pca_result, scale = 0)

# Extract eigenvalues
eigenvalues <- pca_result$sdev^2

# Create a scree plot
plot(1:length(eigenvalues), eigenvalues, type = "b", 
     xlab = "Principal Component", ylab = "Variance Explained",
     main = "Scree Plot")

# Add a cumulative variance explained line
cumulative_variance <- cumsum(eigenvalues) / sum(eigenvalues)
lines(1:length(eigenvalues), cumulative_variance, type = "b", col = "red")

# Add labels to the points
text(1:length(eigenvalues), eigenvalues, labels = round(eigenvalues, 2), pos = 3)
text(1:length(eigenvalues), cumulative_variance, labels = paste0(round(cumulative_variance * 100, 2), "%"), pos = 1, col = "red")

############################ Objective 2 #####################################

# Assuming you have your data in a data frame named 'water_data'
# Convert your data to a matrix
data_matrix <- as.matrix(data)

# Calculate the Euclidean distance between samples
dist_matrix <- dist(data_matrix)
dist_matrix

# Perform hierarchical clustering
hc <- hclust(dist_matrix)
hc

# Plot the dendrogram
plot(hc, main = "Dendrogram of Well Water Samples")

# Cut the dendrogram to create clusters (adjust the height as needed)
# Here, I'm cutting it into 3 clusters as an example
cluster_cut <- cutree(hc, k = 4)

# Display cluster assignments
print(cluster_cut)

hclust_result <- hclust(dist_matrix, method = "ward.D2")  # You can change the linkage method

# Visualize the hierarchical clustering results with a dendrogram
plot(hclust_result, cex = 0.6, main = "Hierarchical Clustering Dendrogram")

num_clusters <- 4  # Specify the number of clusters (you can adjust this)
clusters <- cutree(hclust_result, num_clusters)

# Add cluster assignments to the original data
data$Cluster <- as.factor(clusters)

# You can further analyze and interpret the clusters based on your study objectives

# Summary of cluster assignments
cluster_summary <- table(data$Cluster)
print(cluster_summary)

# Objective 2:  Well water samples can be cluster into homogeneous groups according the structure of the mixture components.
#determine the no of clusters 
fviz_nbclust(data[-1],kmeans,method = "wss")

head(data)
?hclust

library(NbClust)

NbClust(data = data, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, 
        method = "ward.D2", index = "all", alphaBeale = 0.1)

#optimal no of clusters =3

# Use the NbClust function with the numeric data
result <- NbClust(data = data_numeric, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, method = "ward.D2", index = "all", alphaBeale = 0.1)

# Print the result
print(result)
#2nd method to obtain no of optimal clusters
NbClust(data = data[-1], diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, 
        method = "ward.D2", index = "all", alphaBeale = 0.1)

#optimal no of clusters =3
d_df <- dist(data[-1])
d_df
wardse <- hclust(d_df,"ward.D2")

#dendrogram  
plot(wardse)
plot(wardse, hang = -1)
rect.hclust(wardse, k=3 , border="red")
rect.hclust(wardse, k=3 , border= 2:5)

#data frame with sample no and cluster no
clusters <- cutree(wardse,k=3)
df2 <- data.frame(data$,clusters)
head(df2)
library(factoextra)
library(cluster)

# Assuming 'data_matrix' is your data
# You need to determine the optimal number of clusters
wss <- c()
for (i in 1:10) {
  kmeans_model <- kmeans(data_matrix, centers = i)
  wss[i] <- kmeans_model$tot.withinss
}

# Plot the within-cluster sum of squares
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares")


################################# Objective 3 ##############################################



# Calculate the mean and standard deviation for each chemical component
means <- sapply(data[, -1], mean)
sds <- sapply(data[, -1], sd)

# Display the results
summary_stats <- data.frame(Component = names(means), Mean = means, StdDev = sds)
print(summary_stats)

# Sample mean values of chemical components in well water samples
sample_means <- c(Be = 0.02552, Cr = 0.1995, Fe = 88.809, Ni = 1.13954, Cu = 141.456, As = 11.0786, Cd = 0.028784, Ba = 13.4774, Tl = 0.00485, Pb = 1.62066, U = 56.602)

# USA standard reference values
usa_standards <- c(Be = 0.02, Cr = 0.1, Fe = 10, Ni = 1, Cu = 5, As = 0.01, Cd = 0.1, Ba = 0.5, Tl = 0.05, Pb = 0.5, U = 2)

# Create a data frame to store the comparison results
comparison_results <- data.frame(Chemical_Component = names(sample_means),
                                 Sample_Mean = sample_means,
                                 USA_Standard = usa_standards)
comparison_results

# Perform the comparative analysis
comparison_results$Compliance <- ifelse(comparison_results$Sample_Mean <= comparison_results$USA_Standard, "Meets Standards", "Does Not Meet Standards")
comparison_results$Compliance

# Chemical mixtures in well water samples are in line with the standard accepted values in well water
standard_values <- c(Be = 4, Cr = 100, Fe = 300, Ni = 20 , Cu = 1300, As = 10, Cd =5, Ba = 2000, Tl = 0.5, Pb = 15, U = 30)
standard_values

mean_values <- colMeans(data[, c("Be", "Cr", "Fe", "Ni", "Cu", "As", "Cd", "Ba", "Tl", "Pb", "U")])
mean_values


# Compare mean values of your data with standard values
n=92 #no of samples
p=11 #no of variables
ex1 <- cbind(df[-1])
S <- cov(ex1)  #calculate the co-variance matrix
S

x_bar = matrix(mean_values,c(11,1))
x_bar

mu_note =matrix(standard_values,c(11,1))
mu_note

#Test statistics 
T2_cal <- n*t(x_bar-mu_note)%*%solve(S)%*% (x_bar-mu_note)
T2_cal

# Print the results
print(comparison_results)

