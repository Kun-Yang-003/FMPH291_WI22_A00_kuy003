#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double paretodens(double x, double a, double b, bool logflag = false) {
  double ld = 0;
  if(a <= 0 || b <= 0) {
    stop ("a and b should be restrictly positive");
  }
  else if (x < a) {
    if (logflag) {
      ld = -std::numeric_limits<double>::infinity();
    } else {
      ld = 0;
    }
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
