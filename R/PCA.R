#' Perform Principal Component Analysis (PCA)
#'
#'This function performs Principal Component Analysis (PCA) on the input data matrix X
#' and returns the projected data onto the selected principal components
#'
#' @param X A numeric matrix data or a data frame representing the input data
#' @param k An integer specifying the number of principal components to retain
#'
#' @return A matrix containing the projected data onto the selected principal components
#' @export
#'
#' @examples
#' # Load the iris dataset
#'
#' data(iris)
#'
#' #Extract the numerical features from the iris dataset
#' X <- iris[, 1:4]
#'
#' #Apply PCA
#' result <- pca(X, k = 2)
#'
#' #Extract the transformed data
#' transformed_data <- result$transformed_data
#'
#' # Plot the transformed data
#' plot(transformed_data, col = iris$Species, pch = 19, xlab = "PC1", ylab = "PC2")
#'
#' # Add legend
#' legend("topright", legend = levels(iris$Species), col = 1:3, pch = 19)
#'
#' # Plot the eigenvectors
#' arrows(0, 0, result$eigenvectors[1, ], result$eigenvectors[2, ], length = 0.1, col = "blue")
#'

pca <- function(X, k) {
  # Step 1: Compute the mean vector
  mu <- colMeans(X)

  # Step 2: Center the data
  X_centered <- scale(X, center = TRUE, scale = FALSE)

  # Step 3: Compute the covariance matrix
  cov_matrix <- cov(X_centered)

  # Step 4: Compute eigenvectors and eigenvalues
  eigen_result <- eigen(cov_matrix)
  eigenvalues <- eigen_result$values
  eigenvectors <- eigen_result$vectors

  # Step 5: Select the top k eigenvectors
  selected_eigenvectors <- eigenvectors[, 1:k]

  # Step 6: Project the data onto the lower-dimensional subspace
  transformed_data <- X_centered %*% selected_eigenvectors

  # Step 7: Reconstruct the data
  reconstructed_data <- transformed_data %*% t(selected_eigenvectors) + mu

  # Return the results
  return(list(
    mean_vector = mu,
    eigenvectors = selected_eigenvectors,
    eigenvalues = eigenvalues[1:k],
    transformed_data = transformed_data,
    reconstructed_data = reconstructed_data
  ))
}


