#' @title Determinant of a rational matrix
#' @description Determinant of a square matrix with rational entries.
#'
#' @param M a square matrix such that \code{as.character(Mij)} is 
#'   a quoted integer or a quoted fraction for each entry \code{Mij}
#'
#' @return A quoted rational number representing the determinant. 
#' @export
#' @examples 
#' library(RationalMatrix)
#' M <- cbind(c("1/2", "3"), c("5/3", "-2/7"))
#' Qdet(M)
Qdet <- function(M) {
  stopifnot(nrow(M) == ncol(M))
  if(is.matrixZQ(M)) {
    M <- as.character(M)
  } else {
    storage.mode(M) <- "character"
    check <- all(vapply(M, isFraction, logical(1L)))
    if(!check) {
      stop("Invalid matrix `M`.")
    }
  }
  if(anyNA(M)) {
    stop("Found missing values in `M`.")
  }
  det_rcpp(M)
}
