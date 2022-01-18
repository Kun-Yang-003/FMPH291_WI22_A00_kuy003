dparetoR <- function(x, a, b, logd = FALSE){
  nx <- length(x)
  na <- length(a)
  nb <- length(b)
  n <- max(nx, na, nb)
  if (nx < n) x <- rep(x, length.out = n)
  if (na < n) a <- rep(a, length.out = n)
  if (nb < n) b <- rep(b, length.out = n)
  
  ld <- ifelse(a <= 0 | b <= 0, NaN, ifelse(x < a, -Inf, log(b) + b * log(a) - (b + 1) * log(x)))
  
  if(logd){
    return(ld)
  }else{
    return(exp(ld))
  }
}
