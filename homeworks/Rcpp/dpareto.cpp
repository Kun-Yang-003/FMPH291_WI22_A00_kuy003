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
    NumericVector x, NumericVector y, NumericVector z, bool logd = false) {
  int xn = x.size();
  int yn = y.size();
  int zn = z.size();
  // if (xn == 0 || yn == 0 || zn == 0) special handling is needed such as
  // returning a zero length vector
  if (xn==0L || yn == 0L || zn == 0L)
    Rprintf(" Zero Length Detected: xn == %d, yn ==%d\n, zn ==%d\n", xn, yn, zn);
  int n = xn > yn ? xn : yn;
  n = n > zn ? n : zn;
  int ix=0, iy=0, iz=0;
  NumericVector ld(n);
  for (int i = 0; i<n; ++i) {
    // Do something here:
    ld[i] = paretodens( x[ix], y[iy], z[iz], logd);
    // Update the indexes for the next pass:
    if (++ix == xn ) ix = 0;
    if (++iy == yn ) iy = 0;
    if (++iz == zn ) iz = 0;
  }
  return ld;
}