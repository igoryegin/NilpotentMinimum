\name{neg.dombi}
\title{
Dombi-parametrised strong fuzzy negation
}
\description{
A simple, one-line function to negate fuzzy scores (truth values)
}
\usage{
neg.dombi(x, fixpoint = 0.5)
}

\arguments{
  \item{x}{
  a vector of real numbers on an interval from 0 to 1
  }
  \item{fixpoint}{
  a unique real number \eqn{\nu} invariant under negation, i.e. the one for which \eqn{N_D(x) = x} }
  }
}

\value{
A vector of negated scores
}

\details{
The formula for Dombi negation is

\deqn{N_D(x) = \left( 1 + \left( 1-\frac{1}{\nu} \right)^2 \frac{x}{1-x} \right)^{-1}}

Standard fuzzy negation \eqn{N(x) = 1-x}
is a special case of Dombi negation with \eqn{\nu = 0.5} (hence the default value)
}

\references{
Dombi, J., & Jónás, T. (2020). On a strong negation-based representation of modalities.
Fuzzy Sets and Systems, 407, 142–160.
https://doi.org/10.1016/j.fss.2020.10.005
}
\author{
Igor Yegin
}
\examples{
require(NilpotentMinimum)
x <- seq(0, 1, 0.01)
neg.dombi(x)
neg.dombi(x, fixpoint = 0.65)
}
