#' @title Determinant of a rational matrix
#' @description Determinant of a square matrix with rational entries.
#'
#' @template squareMtemplate
#'
#' @return A string: quoted rational number representing the determinant. 
#' @export
#' @examples 
#' library(RationalMatrix)
#' M <- cbind(c("1/2", "3"), c("5/3", "-2/7"))
#' Qdet(M)
Qdet <- function(M) {
  stopifnot(nrow(M) == ncol(M))
  M <- checkM(M)
  det_rcpp(M)
}

#' @title Check injectivity
#' @description Checks whether a rational matrix represents an injective linear 
#'   map (i.e. has trivial kernel).
#' 
#' @template Mtemplate
#'
#' @return A Boolean value indicating whether the linear map corresponding to 
#'   \code{M} is injective.
#' @export
#' 
#' @examples 
#' library(RationalMatrix)
#' set.seed(666L)
#' M <- matrix(rpois(35L, 1), 5L, 7L)
#' QisInjective(M)
QisInjective <- function(M){
  M <- checkM(M)
  isInjective_rcpp(M)
}

#' @title Check surjectivity
#' @description Checks whether a rational matrix represents a surjective linear 
#'   map.
#' 
#' @template Mtemplate
#'
#' @return A Boolean value indicating whether the linear map corresponding to 
#'   \code{M} is surjective.
#' @export
#' 
#' @examples 
#' library(RationalMatrix)
#' set.seed(666L)
#' M <- matrix(rpois(35L, 1), 7L, 5L)
#' QisSurjective(M)
QisSurjective <- function(M){
  M <- checkM(M)
  isSurjective_rcpp(M)
}
