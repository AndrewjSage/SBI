\name{SimulateProportion}
\alias{SimulateProportion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Performs simulation-based hypothesis tests for one-sample and two-sample proportions.
}
\description{
Simulates null distribution and returns simulation-based p-value.
}
\usage{
SimulateProportion(n, x, p, alternative, reps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
sample size. If comparing two proportions, enter each sample size as a vector. (e.g. c(10,15))
}
  \item{x}{
number of "successes". If comparing two proportions, enter each sample size as a vector. (e.g. c(5,10))
}
  \item{p}{
Hypothesized value for 1-proportion. Leave blank for two proportions(assumed to be 0).
}
  \item{alternative}{
Specify alternative hypothesis as either "less", "greater", or "two.sided"
}
  \item{reps}{
Number of simulations to perform
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
