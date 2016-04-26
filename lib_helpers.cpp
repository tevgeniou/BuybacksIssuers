#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(openmp)]]

// [[Rcpp::export]]
NumericVector row_weights(NumericMatrix x, NumericVector weight) {
  int nX = x.ncol();
  int nY = x.nrow();
  NumericVector v = no_init(nY);
  
  #pragma omp parallel for schedule(static)
  for (int i=0; i < nY; i++) {
    NumericMatrix::Row row = x(i, _);

    double w = 0;
    for (int j=0; j < nX; j++) {
      if(row[j]!=0 && !R_IsNA(row[j])) {
        w += weight[j];
      }
    }

    double o = 0;
    if (w!=0) {
      for(int j=0; j < nX; j++) {
        if(row[j]!=0 && !R_IsNA(row[j])) {
          o += row[j]*weight[j] / w;
        }
      }
    }
    
    v[i] = o;
  }
  
  v.attr("names") = rownames(x);
  return v;
}


/*** R
row_weights.R <- function(x, weight) {
  apply(x, 1, function(row) {
    n = sum((row!=0)*weight, na.rm=TRUE)
    if (n!=0) sum(row*weight / n, na.rm=TRUE) else 0
  })
}
*/
