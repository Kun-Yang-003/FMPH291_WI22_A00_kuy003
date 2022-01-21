## From Luke Tierney's notes on Computer Arithmetic
## from  his STAT7400 class

Finv0 <- function(u, a) {
  p <- pnorm(a)
  qnorm(p + u * (1 - p))
}

## Some plots:
u <- (1:100) / 101
plot(u, Finv0(u, 0), type = "l")
plot(u, Finv0(u, 2), type = "l")
plot(u, Finv0(u, 4), type = "l")
plot(u, Finv0(u, 8), type = "l")


## An improved version:
Finv1 <- function(u, a) {
  q <- pnorm(a, lower.tail = FALSE)
  qnorm(q * (1 - u), lower.tail = FALSE)
}

lines(u, Finv1(u, 8), col = "red") 

Finv2 <- function(u, a) {
  q <- pnorm(a, lower.tail = FALSE, log.p = T)
  qnorm(q + log(1 - u), lower.tail = FALSE, log.p = T)
}