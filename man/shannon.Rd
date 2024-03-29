% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shannon.R
\name{shannon}
\alias{shannon}
\title{Calculate Shannon Entropy}
\usage{
shannon(vec, data = NULL, base = 2)
}
\arguments{
\item{vec}{The vector for which the Shannon Entropy is to be calculated. This is either a vector saved as object or a row of the \code{neighbors} data frame or matrix. If a row of a data frame or matrix is specified, this row is excluded as competitor for the nearest neighbor (as a row will be closest to itself).}

\item{data}{A data frame object or matrix.}

\item{base}{Base of the logarithm to be used, defaults to \code{2}.}
}
\description{
\code{shannon} calculates the Shannon Entropy of a vector. The entropy quantifies the expected value of the information contained in a
vector.
}
\examples{

### a vector saved as object

vector <- runif(50, 0, 10)

shannon(vec = vector)

shannon(vec = vector, base = 10)


### a vector specified by its name & its matrix

data("gdsm_mat")

shannon(vec = "var12", data = gdsm_mat)

shannon(vec = "var12", data = gdsm_mat, base = 10)

}
\author{
D. Schmitz
}
