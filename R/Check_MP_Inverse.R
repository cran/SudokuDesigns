#' Moore Penrose Inverse
#'
#' @param matrix Any matrix
#'
#' @return Provides Moore Penrose inverse of a given matrix
#' @export
#'
#' @examples
#' library(SudokuDesigns)
#' mat<-matrix(c(1,2,3,2,5,3,2,4,6),nrow=3,byrow=TRUE)
#' Check_MP_Inverse(mat)
Check_MP_Inverse <- function(matrix) {
  # Ensure the input is a matrix
  N <- as.matrix(matrix)

  # Perform Singular Value Decomposition (SVD)
  svd_result <- svd(N)

  # Extract singular values, left singular vectors, and right singular vectors
  U <- svd_result$u
  V <- svd_result$v
  sigma <- svd_result$d

  # Threshold for singular values (to handle numerical instability)
  threshold <- max(dim(N)) * max(sigma) * .Machine$double.eps
  positive <- sigma > threshold

  # Compute the reciprocal of non-zero singular values
  sigma_inv <- ifelse(positive, 1 / sigma, 0)

  # Construct the diagonal matrix for the inverse singular values
  Sigma_pinv <- matrix(0, nrow = length(sigma), ncol = length(sigma))
  diag(Sigma_pinv) <- sigma_inv

  # Compute the Moore-Penrose inverse
  mp_inverse <- V %*% Sigma_pinv %*% t(U)

  return(mp_inverse)
}

###################################
