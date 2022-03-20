#' Cosine Similarity
#'
#' @description \code{cosim} computes the cosine similarity of two vectors.
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
#' cosim(var01, var02)
#'
#'
#' ### data frame
#'
#' data("gdsm_df")
#'
#' cosim("var03", "var04", gdsm_df)
#'
#'
#' ### matrix
#'
#' data("gdsm_mat")
#'
#' cosim("var05", "var06", gdsm_mat)
#'
#' @export

cosim <- function(v1, v2, data = NULL){

  if(is.null(data)){

    r <- crossprod(v1, v2)/sqrt(crossprod(v1) * crossprod(v2))

  }else{

    if(is.data.frame(data)){

      data <- as.matrix(data)

    }

    x <- data[rownames(data) == v1]
    y <- data[rownames(data) == v2]

    r <- crossprod(x, y)/sqrt(crossprod(x) * crossprod(y))

  }

  return(as.numeric(r))

}

