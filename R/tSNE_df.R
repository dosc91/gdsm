#' Create a tSNE Data Frame for Visualization
#'
#' @description \code{tSNE_df} makes use of \code{Rtsne::Rtsne}, which is a wrapper for the C++ implementation of Barnes-Hut t-Distributed
#' Stochastic Neighbor Embedding. tSNE is a method for constructing a low dimensional embedding of high-dimensional data, distances, or
#' similarities. Exact t-SNE can be computed by setting \code{theta = 0.0}.
#'
#' @param data A data frame object or matrix.
#' @param index integer matrix; Each row contains the identity of the nearest neighbors for each observation
#' @param distance numeric matrix; Each row contains the distance to the nearest neighbors in \code{index} for each observation
#' @param dims integer; Output dimensionality (default: 2)
#' @param initial_dims integer; the number of dimensions that should be retained in the initial PCA step (default: 50)
#' @param perplexity numeric; Perplexity parameter (should not be bigger than 3 * perplexity < nrow(X) - 1, see details for interpretation)
#' @param theta numeric; Speed/accuracy trade-off (increase for less accuracy), set to 0.0 for exact TSNE (default: 0.5)
#' @param check_duplicates logical; Checks whether duplicates are present. It is best to make sure there are no duplicates present and set this option to FALSE, especially for large datasets (default: TRUE)
#' @param pca logical; Whether an initial PCA step should be performed (default: TRUE)
#' @param partial_pca logical; Whether truncated PCA should be used to calculate principal components (requires the irlba package). This is faster for large input matrices (default: FALSE)
#' @param max_iter integer; Number of iterations (default: 1000)
#' @param verbose logical; Whether progress updates should be printed (default: global "verbose" option, or FALSE if that is not set)
#' @param is_distance logical; Indicate whether X is a distance matrix (default: FALSE)
#' @param Y_init matrix; Initial locations of the objects. If NULL, random initialization will be used (default: NULL). Note that when using this, the initial stage with exaggerated perplexity values and a larger momentum term will be skipped.
#' @param pca_center logical; Should data be centered before pca is applied? (default: TRUE)
#' @param pca_scale logical; Should data be scaled before pca is applied? (default: FALSE)
#' @param normalize logical; Should data be normalized internally prior to distance calculations with \code{\link{normalize_input}}? (default: TRUE)
#' @param stop_lying_iter integer; Iteration after which the perplexities are no longer exaggerated (default: 250, except when Y_init is used, then 0)
#' @param mom_switch_iter integer; Iteration after which the final momentum is used (default: 250, except when Y_init is used, then 0)
#' @param momentum numeric; Momentum used in the first part of the optimization (default: 0.5)
#' @param final_momentum numeric; Momentum used in the final part of the optimization (default: 0.8)
#' @param eta numeric; Learning rate (default: 200.0)
#' @param exaggeration_factor numeric; Exaggeration factor used to multiply the P matrix in the first part of the optimization (default: 12.0)
#' @param num_threads integer; Number of threads to use when using OpenMP, default is 1. Setting to 0 corresponds to detecting and using all available cores
#'
#' @author D. Schmitz
#'
#' @references Krijthe, J. H. (2015). Rtsne: T-Distributed Stochastic Neighbor Embedding using a Barnes-Hut Implementation, URL: https://github.com/jkrijthe/Rtsne
#'
#' @examples
#'
#' tSNE_df(gdsm_df)
#'
#' @export

tSNE_df <- function(data, dims = 2, initial_dims = 50,
                      perplexity = 3, theta = 0.5, check_duplicates = TRUE,
                      pca = TRUE, partial_pca = FALSE, max_iter = 1000,
                      verbose = FALSE, is_distance = FALSE,
                      Y_init = NULL, pca_center = TRUE, pca_scale = FALSE,
                      normalize = TRUE, stop_lying_iter = ifelse(is.null(Y_init), 250L,
                      0L), mom_switch_iter = ifelse(is.null(Y_init), 250L, 0L),
                      momentum = 0.5, final_momentum = 0.8, eta = 200,
                      exaggeration_factor = 12, num_threads = 1){

  data <- scale(data)

  fit <- Rtsne::Rtsne(dims = dims, initial_dims = initial_dims, perplexity = perplexity, theta = theta, check_duplicates = check_duplicates, pca = pca,
               partial_pca = partial_pca, max_iter = max_iter, verbose = verbose, is_distance = is_distance, Y_init = Y_init, pca_cener = pca_cener,
               pca_scale = pca_scale, normalize = normalize, stop_lying_iter = stop_lying_iter, mom_switch_iter = mom_switch_iter, momentum = momentum,
               final_momentum = final_momentum, eta = eta, exaggeration_factor = exaggeration_factor, num_threads = num_threads, index = index,
               distance = distance,
               X = data)

  df <- data.frame(fit$Y)

  row.names(df) <- row.names(data)

  if(dims == 2){

    names(df)[1] <- "tSNE1"
    names(df)[2] <- "tSNE2"

  }

  if(dims == 3){

    names(df)[1] <- "tSNE1"
    names(df)[2] <- "tSNE2"
    names(df)[3] <- "tSNE3"

  }

  return(df)

}
