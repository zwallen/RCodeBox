#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix pairwise_similarity_cpp(CharacterMatrix x) {

  int n = x.nrow();
  int p = x.ncol();

  NumericMatrix out(n, n);

  for (int i = 0; i < n; i++) {
    out(i, i) = NA_REAL;

    for (int j = i + 1; j < n; j++) {

      int num = 0;
      int den = 0;

      for (int k = 0; k < p; k++) {

        bool i_na = CharacterVector::is_na(x(i, k));
        bool j_na = CharacterVector::is_na(x(j, k));

        if (!i_na || !j_na) {
          den++;
          if (!i_na && !j_na && x(i, k) == x(j, k)) {
            num++;
          }
        }
      }

      double val = (den > 0) ? static_cast<double>(num) / den : NA_REAL;
      out(i, j) = val;
      out(j, i) = val;
    }
  }

  return out;
}