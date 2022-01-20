\name{plus}
\alias{plus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
  Add x and y
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
   Add all combinations of x and y and make sure that the result is an integer.
}
\usage{
plus(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
    integer vector for row values
}
  \item{y}{
%%     ~~Describe \code{y} here~~
    integer vector for column values
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
  This forms a table of all possible combinations of values and applies
  the addition operator to them.  It checks to be sure that the
 \code{x} and \code{y} are actually integers.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
  matrix of integer values with suitable row and column names
}
\references{
%% ~put references to the literature/web site here ~
\href{https://en.wikipedia.org/wiki/Arithmetic}{Wikipedia Arithmetic Page}
}
\author{
%%  ~~who you are~~
Armin Schwartzman <armins@ucsd.edu>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y) 
{
    res <- outer(as.integer(x), as.integer(y), function(xx, yy) as.integer(xx + 
        yy))
    dimnames(res) <- list(formatC(x, digits = 0, width = max(nchar(x))), 
        formatC(y, digits = 0, width = max(nchar(y))))
    res
  }

## a simple addition table

plus( 1:9, 1:9 )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
