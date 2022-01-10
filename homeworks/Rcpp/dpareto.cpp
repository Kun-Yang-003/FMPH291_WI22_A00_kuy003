#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double paretodens(double x, double a, double b, bool logflag = false) {
  double ld = 0;
  if(a <= 0 || b <= 0) {
    ld = 0;
  }
  else if (x < a) {
    ld = 0;
  }
  else {
    ld = log(b) + (b * log(a)) - ((b + 1) * log(x));
    if (logflag) {
      ld = ld;
    } else {
      ld = exp(ld);
    }
  }
  return ld;
}
