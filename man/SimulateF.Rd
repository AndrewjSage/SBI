\name{SimulateF}
\alias{SimulateF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Performs simulation-based hypothesis tests for F test statistic.
}
\description{
Simulates null distribution and returns simulation-based p-value.
}
\usage{
SimulateRegression(data, x, y, reps)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Dataset in the form of a dataframe.
}
  \item{x}{
name of categorical explanatory variable, as a character string.
}
  \item{y}{
name of numeric response variable, as a character string.
}
  \item{reps}{
Number of simulations to perform}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a histogram of the simulated null distribution. The red line indicates the observed statistic. The simulation-based p-value is also returned.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Andrew Sage
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
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

