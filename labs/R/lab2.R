## File: lab2.R
## Author:
## Date: 01/13/2022
## Purpose: FMPH 291 Statistical Computing using R - lab 2 exercises 


#### Hypotenuse length calculation

hypot1 <- function(x,y) sqrt(x ^ 2 + y ^ 2)

hypot1(0, 0)
# [1] 0
hypot1(6, 8)
# [1] 10
hypot1(1:10, 20)
# [1] 20.02498 20.09975 20.22375 20.39608 20.61553 20.88061 21.18962 21.54066 21.93171 22.36068

hypot1(3e300, 4e300)
# [1] Inf

hypot1(3e-161, 4e-161)
# [1] 4.999972e-161

# it comes with zero when using smaller values for x and y
hypot1(1e-162, 1e-162)
# [1] 0

## Do you have a rough idea why these differ?
'
The double type has a limit of maximum digit, 
so when the number comes with a extremely large 
or small absolute value, it returns Inf or 0.
'

## Roughly, how small can x and y be and still return a value that is non-zero?
'
A rough estimation of that numbers on my machine with current setting is, 
some values greater than (1e-162) for both x and y.
'

# adding 1e-100 to make it workable when x+y=0
# It is a little bit tricky that if one number is big but the other 
# number is not, the fraction corresponding to the latter would 
# possibly be treated as 0 again. 
# But it is fine to ignore the latter in that case, I think.
hypot2 <- function(x, y) exp(1/2*(log( ((x/sqrt(1e-100+abs(x+y)))^2 + (y/sqrt(1e-100+abs(x+y)))^2) ) + log(1e-100+abs(x+y))))


# hypot1(0, 0)
# hypot2(6, 8)
# hypot2(3e300, 4e300)
# hypot2(3e-171, 4e-171)
# hypot2(3e-201, 4e-201)


all.equal(hypot2( 3 * (0:10), -4 * (0:10)), hypot1( 3 * (0:10), -4 * (0:10)))
# [1] TRUE
# [1]  0  5 10 15 20 25 30 35 40 45 50



#### Logspace computations

logSumExp1 <- function(x,y) log( exp( x ) + exp( y ))

logSumExp1(1, 1)

logSumExp1(100, 100)

bigButFinite <- log(.Machine$double.xmax)

logSumExp1(bigButFinite-1, bigButFinite-1)

logSumExp1(bigButFinite, bigButFinite) 

## Why is Inf the result for the last expression?
'
Same answer as above.
'

# logSumExp2 <- function(x, y) log( exp(x-sqrt(bigButFinite)) + exp(y-sqrt(bigButFinite))) + sqrt(bigButFinite)
logSumExp2 <- function(x, y){
  m <- pmax(x, y)
  log( exp(x-m) + exp(y-m)) + m
}

# logSumExp2(1, 1)
# logSumExp2(100, 100)
# logSumExp2(bigButFinite-1, bigButFinite-1)
# logSumExp2(bigButFinite, bigButFinite)

# vec_x <- c(1, 1, 1000, 10, 100, 2e50)
# vec_y <- c(1, 1000, 1, 20, 200, 3e50)
df <- read.table(text = "
                 x y
                 1 1
                 1 1000
                 1000 1
                 10 20
                 100 200
                 2e50 3e50", header= T)

logSumExp2(df$x, df$y)
# [1] 1.693147e+00 1.000000e+03 1.000000e+03 2.000005e+01 2.000000e+02 3.000000e+50


#### additional task
#####################

all.equal(hypot2( 3 * (0:10), -4 * (0:10)), hypot1( 3 * (0:10), -4 * (0:10)))
#Should print out TRUE

#####################

df <- read.table(text = "
x y
-1 -1
1 1000
1000 1
-30 40
300 -400
3e300 4e300
1 4e300
3e300 1
3e-161 4e-161
1e-161 4e-161
3e-161 1", header= T)
do.call("hypot2", df)
#Should print out 1.414214e+00 1.000000e+03 1.000000e+03 5.000000e+01
#5.000000e+02 5.000000e+300 4.000000e+300 3.000000e+300
#5.000000e-161 4.123106e-161 1.000000e+00

#####################

df <- read.table(text = "
x y
1 1
1 1000
1000 1
10 20
100 200
2e50 3e50", header= T)
do.call("logSumExp2", df)
#Should print out 1.693147e+00 1.000000e+03 1.000000e+03 2.000005e+01 2.000000e+02 3.000000e+50

#####################

df <- read.table(text = "
x y
1 3
2 4
3 5
-1 -1
2 -1
1 -2", header= T)

all.equal(logSumExp1(df$x, df$y), logSumExp2(df$x, df$y))
#Should print out TRUE