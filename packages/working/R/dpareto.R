# dpareto

dpareto <- function(x, alpha, beta, log = FALSE)
{
  Rcpp::sourceCpp("homeworks/Rcpp/dpareto.cpp")
  .dpareto(x, alpha, beta, log)
}
