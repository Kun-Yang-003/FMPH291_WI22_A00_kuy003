fpSum <- function(x, y, digits = 2){
  z <- x+y
  n_digits <- 10^floor(log10(z))
  round(z/n_digits, digits = (digits-1))*n_digits
}

fpSum <- function(x, y = 0, digits = 2){
  stopifnot("digits must be an integer greater than 0" = digits > 0 & is.integer(digits))
  z <- x+y
  n_digits <- 10^floor(log10(z))
  print(z/n_digits)
  z <- round(z/n_digits, digits = digits-1)*n_digits
  # z <- formatC(z, digits = digits-1, format = "e")
  return(z)
}

fpSum <- function(x, y = 0, digits = 2) signif(x + y, digits = digits)

# fpSum(z)
# z <- 3.94e9
# z <- 3.94e5
# z <- 3.94e-1
# z <- 3.94e-5
# z <- 3.94e-9
# z <- 3.94
# z <- 39
# z <- 4
# z <- 0.0394
# len <- nchr(as.character(z))
