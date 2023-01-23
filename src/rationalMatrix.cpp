#include <RcppEigen.h>
#include <boost/multiprecision/gmp.hpp>
#include "RationalMatrix_types.h"
using namespace boost::multiprecision;
typedef Eigen::Matrix<mpq_rational, Eigen::Dynamic, Eigen::Dynamic> QMatrix;

// ------------------------------------------------------------------------- //
// ------------------------------------------------------------------------- //
std::string q2str(mpq_rational r) {
  mpz_int numer = numerator(r);
  mpz_int denom = denominator(r);
  mpz_t p;
  mpz_init(p);
  mpz_set(p, numer.backend().data());
  mpz_t q;
  mpz_init(q);
  mpz_set(q, denom.backend().data());
  size_t n = mpz_sizeinbase(p, 10) + 2;
  size_t d = mpz_sizeinbase(q, 10) + 2;
  char* cnumer = new char[n];
  char* cdenom = new char[d];
  cnumer = mpz_get_str(cnumer, 10, p);
  cdenom = mpz_get_str(cdenom, 10, q);
  std::string snumer = cnumer;
  std::string sdenom = cdenom;
  delete[] cnumer;
  delete[] cdenom;
  mpz_clear(p);
  mpz_clear(q);
  return snumer + "/" + sdenom;
}

// ------------------------------------------------------------------------- //
// ------------------------------------------------------------------------- //
QMatrix charMatrix2qMatrix(CharMatrix M) {
  const int m = M.nrow();
  const int n = M.ncol();
  QMatrix Mq(m, n);
  for(int i = 0; i < m; i++) {
    for(int j = 0; j < n; j++) {
      Mq(i, j) = mpq_rational(Rcpp::as<std::string>(M(i, j)));
    }
  }
  return Mq;
}

// ------------------------------------------------------------------------- //
// ------------------------------------------------------------------------- //
CharMatrix qMatrix2charMatrix(const QMatrix& Mq) {
  const int m = Mq.rows();
  const int n = Mq.cols();
  CharMatrix M(m, n);
  for(int i = 0; i < m; i++) {
    for(int j = 0; j < n; j++) {
      M(i, j) = q2str(Mq.coeff(i, j));
    }
  }
  return M;
}

// ------------------------------------------------------------------------- //
// ------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::String Qdet_rcpp(CharMatrix M) {
  QMatrix Mq = charMatrix2qMatrix(M);
  mpq_rational d = Mq.determinant();
  return q2str(d);
}
