#' Calculate Shannon Entropy
#'
#' @description \code{shannon} calculates the Shannon Entropy of a vector. The entropy quantifies the expected value of the information contained in a
#' vector.
#'
#' @param vec The vector for which the Shannon Entropy is to be calculated. This is either a vector saved as object or a row of the \code{neighbors} data frame or matrix. If a row of a data frame or matrix is specified, this row is excluded as competitor for the nearest neighbor (as a row will be closest to itself).
#' @param data A data frame object or matrix.
#' @param base Base of the logarithm to be used, defaults to \code{2}.
#'
#' @author D. Schmitz
#'
#' @examples
#'
#' ### a vector saved as object
#'
#' vector <- runif(50, 0, 10)
#'
#' shannon(vec = vector)
#'
#' shannon(vec = vector, base = 10)
#'
#'
#' ### a vector specified by its name & its matrix
#'
#' data("gdsm_mat")
#'
#' shannon(vec = "var12", data = gdsm_mat)
#'
#' shannon(vec = "var12", data = gdsm_mat, base = 10)
#'
#' @export

shannon <- function (vec, data = NULL, base = 2){

  if(is.numeric(vec)){

    if(!is.null(data)){

      cli::cli_alert_info(glue::glue(paste("The 'data' argument is not meaningful for individual vector objects. Calculating anyway.")))

    }

    vec <- as.matrix(vec)

    quo <- vec/sum(vec)

    res <- -sum(ifelse(quo > 0, quo * log(quo, base = base), 0))

  }else{

    wvec <- data[rownames(data) == vec]


    wvec <- as.matrix(wvec)

    quo <- wvec/sum(wvec)

    res <- -sum(ifelse(quo > 0, quo * log(quo, base = base), 0))

  }

  return(res)

}
