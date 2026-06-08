#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
LogicalVector is_even_C(IntegerVector x) {
  int n = x.size();
  LogicalVector out(n);
  for (int i = 0; i < n; ++i) {
    if (IntegerVector::is_na(x[i])) {
      out[i] = NA_LOGICAL;
    } else {
      out[i] = ((x[i] & 1) == 0);
    }
  }
  return out;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
is_even_C(c(42, NA))
*/
