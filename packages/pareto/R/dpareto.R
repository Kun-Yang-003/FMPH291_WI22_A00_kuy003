# The Pareto Distribution

## dparetoR is created to calculate the density for a Pareto distribution
## parameters: x: vector of data
##             alpha: required to be positive
##             beta: required to be positive
##             log: use of log form, default as false

## This dparetoR function with recycle is copied from homework1-review and is used for HW2
dpareto <- function(x, alpha, beta, log = FALSE) {
  
  result <- .dpareto (x, alpha, beta, log)
  
  if(anyNA(result)) {warning("NaNs")}
  
  return (result)
}
