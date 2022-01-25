#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double paretodens(double x, double a, double b, bool logd = false) {
  double ld = 0;
  if(a <= 0 || b <= 0) {
    stop ("a and b should be restrictly positive");
  }
  else if (x < a) {
    if (logd) {
      ld = -std::numeric_limits<double>::infinity();
    } else {
      ld = 0;
    }
  }
  else {
    ld = log(b) + (b * log(a)) - ((b + 1) * log(x));
    if (logd) {
      ld = ld;
    } else {
      ld = exp(ld);
    }
  }
  return ld;
}

// [[Rcpp::export]]
NumericVector dpareto(
    NumericVector x, NumericVector a, NumericVector b, bool logd = false) {
  int xn = x.size();
  int an = a.size();
  int bn = b.size();
  // if (xn == 0 || an == 0 || bn == 0) special handling is needed such as
  // returning a zero length vector
  if (xn==0L || an == 0L || bn == 0L)
    Rprintf(" Zero Length Detected: xn == %d, an ==%d\n, bn ==%d\n", xn, an, bn);
  int n = xn > an ? xn : an;
  n = n > bn ? n : bn;
  int ix=0, ia=0, ib=0;
  NumericVector ld(n);
  for (int i = 0; i<n; ++i) {
    // Do something here:
    ld[i] = paretodens( x[ix], a[ia], b[ib], logd);
    // Update the indexes for the next pass:
    if (++ix == xn ) ix = 0;
    if (++ia == an ) ia = 0;
    if (++ib == bn ) ib = 0;
  }
  return ld;
}