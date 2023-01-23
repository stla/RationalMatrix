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
QisInjective <- function(M) {
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
QisSurjective <- function(M) {
  M <- checkM(M)
  isSurjective_rcpp(M)
}

#' @title Check invertibility
#' @description Checks whether a square rational matrix is invertible.
#' 
#' @template squareMtemplate
#'
#' @return A Boolean value indicating whether \code{M} is invertible.
#' @export
#' 
#' @examples 
#' library(RationalMatrix)
#' set.seed(666L)
#' M <- matrix(rpois(25L, 1), 5L, 5L)
#' QisInvertible(M)
QisInvertible <- function(M) {
  stopifnot(nrow(M) == ncol(M))
  M <- checkM(M)
  isInvertible_rcpp(M)
}

#' @title Rank of a rational matrix
#' @description Returns the rank of a rational matrix.
#' 
#' @template Mtemplate
#'
#' @return An integer, the rank of \code{M}.
#' @export
#' 
#' @examples 
#' library(RationalMatrix)
#' M <- cbind(c("1/2", "3", "1"), c("5/3", "-2/7", "10/3"), c("1", "1", "2"))
#' Qrank(M)
Qrank <- function(M) {
  M <- checkM(M)
  rank_rcpp(M)
}

#' @title Inverse of a rational matrix
#' @description Inverse matrix of a square rational matrix.
#' 
#' @template squareMtemplate
#'
#' @return A character matrix representing the inverse of \code{M}.
#' @export
#' 
#' @examples 
#' library(RationalMatrix)
#' M <- cbind(c("1/2", "3", "1"), c("5/3", "-2/7", "10/3"), c("0", "1", "2"))
#' Qinverse(M)
Qinverse <- function(M) {
  M <- checkM(M, TRUE)
  inverse_rcpp(M)
}

#' @title Kernel of a rational matrix
#' @description Kernel (null-space) of a rational matrix.
#' 
#' @template Mtemplate
#'
#' @return A character matrix representing a basis of the kernel of \code{M}. 
#'   Note that this basis is not orthogonal.
#' @export
#' 
#' @examples 
#' library(RationalMatrix)
#' set.seed(666L)
#' M <- matrix(rpois(30L, 6), 10L, 3L)
#' M <- cbind(M, M[,1] + M[,2], M[,2] + 2L*M[,3])
#' Qkernel(M)
Qkernel <- function(M) {
  M <- checkM(M)
  kernel_rcpp(M)
}

