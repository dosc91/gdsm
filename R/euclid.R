#' Euclidian Distance
#'
#' @description \code{euclid} computes the Euclidian distance between two vectors.
#' Specify either two individual vectors or a data frame/matrix and two pertinent row names as input.
#'
#' @param data A data frame object or matrix.
#' @param v1 Either an individual vector or a row name of \code{data}.
#' @param v2 Either an individual vector or a row name of \code{data}.
#'
#' @return Nothing.
#'
#' @author D. Schmitz
#'
#' @examples
#'
#' ### two individual vectors
#'
#' var01 <- runif(n = 50, min = 1, max = 10)
#' var02 <- runif(n = 50, min = 1, max = 10)
#'
#' euclid(var01, var02)
#'
#'
#' ### data frame
#'
#' data("gdsm_df")
#'
#' euclid("var03", "var04", gdsm_df)
#'
#'
#' ### matrix
#'
#' data("gdsm_mat")
#'
#' euclid("var05", "var06", gdsm_mat)
#'
#' @export

euclid <- function(v1, v2, data = NULL){

  if(is.null(data)){

    r <- sqrt(sum((v1 - v2)^2))

  }else{

    if(is.data.frame(data)){

      data <- as.matrix(data)

    }

    x <- data[rownames(data) == v1]
    y <- data[rownames(data) == v2]

    r <- sqrt(sum((x - y)^2))

  }

  return(as.numeric(r))

}

