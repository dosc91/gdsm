#' Manhattan Distance
#'
#' @description \code{manhat} computes the Manhattan distance between two vectors.
#' Specify either two individual vectors or a data frame/matrix and two pertinent row names as input.
#'
#' @param data A data frame object or matrix.
#' @param v1 Either an individual vector or a row name of \code{data}.
#' @param v2 Either an individual vector or a row name of \code{data}.
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
#' manhat(var01, var02)
#'
#'
#' ### data frame
#'
#' data("gdsm_df")
#'
#' manhat("var03", "var04", gdsm_df)
#'
#'
#' ### matrix
#'
#' data("gdsm_mat")
#'
#' manhat("var05", "var06", gdsm_mat)
#'
#' @export

manhat <- function(v1, v2, data = NULL){

  if(is.null(data)){

    dist <- abs(v1 - v2)

    r <- sum(dist)

  }else{

    if(is.data.frame(data)){

      data <- as.matrix(data)

    }

    x <- data[rownames(data) == v1]
    y <- data[rownames(data) == v2]

    dist <- abs(x - y)

    r <- sum(dist)

  }

  return(as.numeric(r))

}

