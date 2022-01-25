\name{dpareto}
\alias{dpareto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
  density function of pareto distribution
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
  dpareto computes the density of the Pareto distribution.
}
\usage{
dpareto(x, alpha, beta, log = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
  numeric vector.
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
  numeric vector. Should be greater than 0.
}
  \item{beta}{
%%     ~~Describe \code{beta} here~~
  numeric vector. Should be greater than 0.
}
  \item{log}{
%%     ~~Describe \code{log} here~~
  logical. Applying logtransfomation if TRUE.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
dpareto gives the density distribution function. The Paretp distribution density is calculated using the following formula: $$ f(x|\alpha,\beta) = \frac{\beta{\alpha}^\beta}{x^{\beta+1}} $$
for $0<\alpha\leq x$ and $\beta>0$. Note that densities are defined on the entire real line; they are zero outside of the support of the distribution.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
  numeric vector of probabilities.
}
\references{
%% ~put references to the literature/web site here ~
Pareto, Vilfredo, Cours d'Économie Politique: Nouvelle édition par G.-H. Bousquet et G. Busino, Librairie Droz, Geneva, 1964, pp. 299–345.

}
\author{
%%  ~~who you are~~
Kun Yang <kuy003@ucsd.edu>
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
function (x, alpha, beta, log = FALSE) 
{
    Rcpp::sourceCpp("homeworks/Rcpp/dpareto.cpp")
    .dpareto(x, alpha, beta, log)
  }
}

## a simple times table

dpareto(2:4, 1, 2)

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
