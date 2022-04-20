#' Calculate Neighborhood Density
#'
#' @description \code{n_density} takes the \code{n} most strongly correlated neighbors of a target vector and their correlation coefficients to calculate a mean correlation
#' coefficient. The higher the value of this coefficient, the denser the neighborhood.
#'
#' @param vec The vector for which the neighbor density is to be calculated. This is either a vector saved as object or a row of the \code{neighbors} data frame or matrix. If a row of a data frame or matrix is specified, this row is excluded as competitor for the nearest neighbor (as a row will be closest to itself).
#' @param neighbors The potential neighbors, usually given as data frame or matrix.
#' @param n The number of neighbors wanted as output. Defaults to \code{8}.
#'
#' @author D. Schmitz
#'
#' @examples
#'
#' ### a vector saved as object & a matrix of neighbors
#'
#' vector <- runif(50, 0, 10)
#' data("gdsm_mat")
#'
#' n_density(vec = vector, neighbors = gdsm_mat)
#'
#' n_density(vec = vector, neighbors = gdsm_mat, n = 4)
#'
#'
#' ### a vector specified by its name & its matrix of neighbors
#'
#' data("gdsm_mat")
#'
#' n_density(vec = "var12", neighbors = gdsm_mat)
#'
#' n_density(vec = "var12", neighbors = gdsm_mat, n = 4)
#'
#' @export

n_density <- function(vec, neighbors, n = 8){

  df <- data.frame()

  if(is.numeric(vec)){

    for(i in 1:nrow(neighbors)){

      df[i,1] <- cor(vec, neighbors[i,])

    }

  }

  else if (is.character(vec)){

    wvec <- neighbors[rownames(neighbors) == vec]

    wneighbors <- neighbors[!(row.names(neighbors) %in% vec), ]

    for(i in 1:nrow(wneighbors)){

      df[i,1] <- cor(wvec, wneighbors[i,])

    }

    df[i,1] <- cor(wvec, wneighbors[i,])

  }

  df[1] <- abs(df[1])

  df[1] <- df[order(-df[1]),]


  n_density <- mean(head(df[1:n,]))

  return(n_density)

}
