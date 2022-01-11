dparetoR <- function(x, a, b, logflag = FALSE){
  nx <- length(x)
  na <- length(a)
  nb <- length(b)
  n <- max(nx, na, nb)
  if (nx < n) x <- rep(x, length.out = n)
  if (na < n) a <- rep(a, length.out = n)
  if (nb < n) b <- rep(b, length.out = n)

  if(a <= 0 | b <= 0) ld <- NaN
  if(x < a){
    ld <- -Inf
  }else{
    ld <- log(b) + b * log(a) - (b + 1) * log(x)
  }
  if(logflag){
    return(ld)
  }else{
    return(exp(ld))
  }
}
