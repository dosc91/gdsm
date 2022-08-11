#' Create Random Matrix
#'
#' @description \code{random_matrix} creates a matrix of randomly generated vectors.
#'
#' @param dims The number of dimensions (columns) of the random matrix. Defaults to \code{50}.
#' @param rows The number of rows of the random matrix. Defaults to \code{25}.
#' @param df Specify whether the matrix is to be created as data frame object. Defaults to \code{FALSE}.
#'
#' @author D. Schmitz
#'
#' @examples
#'
#' ### using default settings
#'
#' matrix_a <- random_matrix()
#' dim(matrix_a)
#'
#' ### as data frame object
#'
#' matrix_b <- random_matrix(df = TRUE)
#' dim(matrix_b)
#'
#' ### more dimensions
#'
#' matrix_c <- random_matrix(dims = 100)
#' dim(matrix_c)
#'
#' ### more rows
#'
#' matrix_d <- random_matrix(rows = 100)
#' dim(matrix_d)
#'
#' @export

random_matrix <- function(dims = 50, rows = 25, df = FALSE){

  matrix <- matrix(ncol = dims, nrow = rows)

  for(i in 1:rows){

    matrix[i,] <- runif(dims)

  }

  rownames(matrix) <- 1:rows
  colnames(matrix) <- paste("X", 1:dims, sep ="")

  if(df == TRUE){

    matrix <- as.data.frame(matrix)

  }

  return(matrix)

}
