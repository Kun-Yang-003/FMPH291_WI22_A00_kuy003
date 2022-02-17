tabll <- function(p.vec, eta.mat, X){
  N <- nrow(X)
  C <- ncol(X)
  M <- length(p.vec)
  if(ncol(eta.mat) != C) warning("")
  if(nrow(eta.mat) != M) warning("")
  m.vec <- rowSums(X)
  ll <- 1
  for(i in 1:N){
    lli <- 0
    for(j in 1:M){
      lli <- lli + p.vec[j] * factorial(m.vec[i]) * prod(eta.mat[j,] ^ X[i,] / factorial(X[i,]))
    }
    ll <- ll * lli
  }
  ll
}

prop.by.row <- function(X) sweep(X, 1, rowSums(X), "/")

ExZ.X <- function(p.vec, eta.mat, X){
  N <- nrow(X)
  C <- ncol(X)
  M <- length(p.vec)
  Ez <- matrix(nrow = N, ncol = M)
  for(i in 1:N){
    for(j in 1:M){
      Ez[i, j] <- p.vec[j] * dmultinom(X[i,], prob = eta.mat[j,])
    }
  }
  prop.by.row(Ez)
}

Mparms <- function(expect.Z, X){
  eta.mat <- prop.by.row(t(expect.Z) %*% X)
  p.vec <- colMeans(expect.Z)
  list(eta = eta.mat, p = p.vec)
}

updatell <- function(p.vec, eta.mat, X, n.iter = 10){
  for(it in 1:n.iter){
    expect.Z <- ExZ.X(p.vec, eta.mat, X)
    expect.M <- Mparms(expect.Z, X)
    p.vec <- expect.M$p
    eta.mat <- expect.M$eta
  }
  return(list(eta = eta.mat, p = p.vec, ll = tabll(p.vec, eta.mat, X)))
}
