#' Find Nearest Neighbors
#'
#' @description \code{find_nn} finds the nearest neighbor for a given vector. Neighbors can be found via cosine similarity, Euclidean distance, or
#' Manhattan distance.
#'
#' @param vec The vector for which neighbors are to be found. This is either a vector saved as object or a row of the \code{neighbors} data frame or matrix. If a row of a data frame or matrix is specified, this row is excluded as competitor for the nearest neighbor (as a row will be closest to itself).
#' @param neighbors The potential neighbors, usually given as data frame or matrix.
#' @param n The number of neighbors wanted as output. Defaults to \code{5}.
#' @param method The measure used to determine the nearest neighbors. Either \code{"cosim"}, \code{"euclid"} (default), or \code{"manhat"}.
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
#' find_nn(vec = vector, neighbors = gdsm_mat, 3, "cosim")
#'
#' find_nn(vec = vector, neighbors = gdsm_mat, 3, "euclid")
#'
#' find_nn(vec = vector, neighbors = gdsm_mat, 3, "manhat")
#'
#'
#' ### a vector specified by its name & its matrix of neighbors
#'
#' data("gdsm_mat")
#'
#' find_nn(vec = "var12", neighbors = gdsm_mat, 3, "cosim")
#'
#' find_nn(vec = "var12", neighbors = gdsm_mat, 3, "euclid")
#'
#' find_nn(vec = "var12", neighbors = gdsm_mat, 3, "manhat")
#'
#' @export


find_nn <- function(vec, neighbors, n = 5, method = "euclid") {

  df <- data.frame()

  if(is.numeric(vec)){

    if(method == "euclid"){

      for(i in 1:nrow(neighbors)){

        df[i,1] <- gdsm::euclid(vec, neighbors[i,])

      }

    }else if(method == "cosim"){

      for(i in 1:nrow(neighbors)){

        df[i,1] <- gdsm::cosim(vec, neighbors[i,])

      }

    }else if(method == "manhat"){

      for(i in 1:nrow(neighbors)){

        df[i,1] <- gdsm::manhat(vec, neighbors[i,])

      }

    }

    df[2] <- rownames(neighbors)

  }else if (is.character(vec)){

    wvec <- neighbors[rownames(neighbors) == vec]

    wneighbors <- neighbors[!(row.names(neighbors) %in% vec), ]

    if(method == "euclid"){

      for(i in 1:nrow(wneighbors)){

        df[i,1] <- gdsm::euclid(wvec, wneighbors[i,])

      }

    }else if(method == "cosim"){

      for(i in 1:nrow(wneighbors)){

        df[i,1] <- gdsm::cosim(wvec, wneighbors[i,])

      }

    }else if(method == "manhat"){

      for(i in 1:nrow(wneighbors)){

        df[i,1] <- gdsm::manhat(wvec, wneighbors[i,])

      }

    }

    df[2] <- rownames(wneighbors)

  }


  if(method == "cosim"){

    df <- df[order(-df[1]),]

  }else{

    df <- df[order(df[1]),]

  }

  rownames(df) <- 1:nrow(df)

  colnames(df) <- c(method, "nearest_neighbor")

  res <- head(df, n)

return(res)

}
