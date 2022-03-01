rlnp <- function(n, y, scale = 1){
  # x <- seq(-5,5,0.001)
  # imax <- which.max(dlnp(x,y))
  # x0 <- x[imax]
  # M <- dlnp(x0,y)/max(dcauchy(x, x0, scale))
  x0 <- optimize(function(x){dlnp(x, y)}, c(-5, 5), maximum = T)$maximum
  M <- dlnp(x0,y)/optimize(function(x){dcauchy(x, x0, scale)}, c(-5, 5), maximum = T)$objective
  x <- c()
  yunif <- c()
  dratio <- c()
  keep <- c()
  n_ <- 10*n
  while(sum(keep) < n){
    x_ <- rcauchy(n_)
    yunif_ <- runif(n_)
    dratio_ <- dlnp(x_, y) / (M*dcauchy(x_, x0, scale))
    keep_ <- yunif_ <= dratio_
    x <- c(x, x_)
    yunif <- c(yunif, yunif_)
    dratio <- c(dratio, dratio_)
    keep <- c(keep, keep_)
  }
  i.end <- max(which(keep)[1:n])
  return(list(x = x[1:i.end], yunif = yunif[1:i.end], dratio = dratio[1:i.end], keep = keep[1:i.end], x0 = x0, M = M, y = y, scale = scale))
}

rlnpPlot <- function(obj){
  with(obj, {
    curve( M * dcauchy( x , x0, scale), min(x[ keep ]), max(x[ keep ], lty = 4), ylab="", xlab = "")
    curve( dlnp( x , y), min(x[ keep ]), max(x[ keep ]), col = 2, add = TRUE, ylab="", xlab = "")
    points( x[ keep ], yunif[ keep ] * (dcauchy( x[ keep ] , x0, scale) * M), pch=".", col=2, ylab="", xlab = "")
    points( x[ ! keep ], yunif[ ! keep ] * (dcauchy( x[ ! keep ] , x0, scale) * M), pch=".", ylab="", xlab = "")
    legend( "topright", col = 1:2, lty = rep( 1, 2 ), legend = c( "denvelope/rejected", "dlnp/accepted"))
    title(main = paste0("dlnp(x, ", y, ")"), sub = paste0("denvelope(x) = ", round(M,2), " * dcauchy(x,", round(x0,2), ",", round(scale,2), ")", "\n", "fraction of samples rejected = ", round(1-sum(keep)/length(keep),2)))
  })
}