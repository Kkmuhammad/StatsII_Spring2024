# Load required libraries
library(cluster)
library(MASS)
library(ggplot2)

# Function to calculate error percentage
calculate_error_percentage <- function(clusters, labels) {
  error <- sum(clusters != labels)
  error_percentage <- (error / length(labels)) * 100
  return(error_percentage)
}

# Function to calculate the Rand index
calculate_rand_index <- function(clusters, labels) {
  n <- length(clusters)
  a <- sum(outer(clusters, clusters, "==") & outer(labels, labels, "=="))  # Number of pairs with the same cluster and label
  b <- sum(outer(clusters, clusters, "!=") & outer(labels, labels, "!="))  # Number of pairs with different cluster and label
  rand_index <- (a + b) / choose(n, 2)  # Rand index calculation
  return(rand_index)
}

# Function to calculate Wilks' Lambda
calculate_wilks_lambda <- function(data, clusters) {
  lda_data <- cbind(data, clusters)  # Combine data with clusters
  lda_model <- lda(lda_data[, -ncol(lda_data)], clusters)
  wilks_lambda <- lda_model$svd^2 / sum(lda_model$svd^2)
  return(wilks_lambda)
}

# Load Iris dataset
data(iris)

# Set the seed for reproducibility
set.seed(123)

# Approach 1: Random initial centroids with iteration for optimal clustering
kmeans_random <- kmeans(iris[-5], centers = 3, nstart = 20)

# Calculate error percentage and Rand index for approach 1
error_percentage_random <- calculate_error_percentage(kmeans_random$cluster, as.numeric(iris$Species))
rand_index_random <- calculate_rand_index(kmeans_random$cluster, as.numeric(iris$Species))

# Fit LDA model for approach 1 with random centroids
wilks_lambda_random <- calculate_wilks_lambda(iris[-5], kmeans_random$cluster)

# Approach 2: Initial centroids based on maximum variation and minimum correlation
variances <- apply(iris[-5], 2, var)  # Calculate variances for each feature
max_var_index <- which.max(variances)  # Index for maximum variation
min_corr_indices <- which(cor(iris[-5]) == min(cor(iris[-5])))  # Indices for minimum correlation

# Ensure the indices are distinct
if (max_var_index %in% min_corr_indices) {
  min_corr_indices <- min_corr_indices[min_corr_indices != max_var_index]
}

# Initialize centroids based on selected indices
centroids <- iris[c(max_var_index, min_corr_indices), -5]

# Shuffle the order of the data points
shuffled_indices <- sample(nrow(iris))
shuffled_iris <- iris[shuffled_indices, ]

# Perform k-means clustering with shuffled data and centroids based on maximum variation and minimum correlation
kmeans_variation <- kmeans(shuffled_iris[-5], centers = centroids)

# Calculate error percentage and Rand index for approach 2
error_percentage_variation <- calculate_error_percentage(kmeans_variation$cluster, as.numeric(shuffled_iris$Species))
rand_index_variation <- calculate_rand_index(kmeans_variation$cluster, as.numeric(shuffled_iris$Species))

# Fit LDA model for approach 2 with variation-based centroids
wilks_lambda_variation <- calculate_wilks_lambda(shuffled_iris[-5], kmeans_variation$cluster)

# Visualize the clustering results
# Approach 1: Random Initial Centroids
ggplot(iris, aes(Petal.Length, Petal.Width, color = as.factor(kmeans_random$cluster))) +
  geom_point() +
  labs(title = "Approach 1: Random Initial Centroids") +
  theme_minimal()

# Approach 2: Variation-based Initial Centroids
ggplot(shuffled_iris, aes(Petal.Length, Petal.Width, color = as.factor(kmeans_variation$cluster))) +
  geom_point() +
  labs(title = "Approach 2: Variation-based Initial Centroids") +
  theme_minimal()

# Print the results
cat("Approach 1 (Random Initial Centroids):\n")
cat("Error Percentage:", error_percentage_random, "%\n")
cat("Rand Index:", rand_index_random, "\n")
cat("Wilks' Lambda:", wilks_lambda_random, "\n\n")

cat("Approach 2 (Variation-based Initial Centroids):\n")
cat("Error Percentage:", error_percentage_variation, "%\n")
cat("Rand Index:", rand_index_variation, "\n")
cat("Wilks' Lambda:", wilks_lambda_variation, "\n")