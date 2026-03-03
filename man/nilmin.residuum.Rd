\name{nilmin.residuum}
\title{
Residuum (R-implication) of the Nilpotent Minimum (NM) T-norm
}
\usage{
nilmin.residuum(x, y, fixpoint = 0.5)
}

\arguments{
  \item{x}{
  a vector of real numbers from 0 to 1; the \emph{left-hand} side of implication
  }
  \item{y}{
  a vector of real numbers from 0 to 1; the \emph{right-hand} side of implication
  }
  \item{fixpoint}{
  negation fixpoint to be used inside the function
  }
}

\value{
a vector of fuzzy truth values for the expression \eqn{x \rightarrow_{NM} y}
}

\details{
When \eqn{x > y}, the residuation operator corresponding to the NM t-norm is

\deqn{x \rightarrow_{NM} y = \max \left( N(x), y \right)}

where \eqn{N(x)} is an arbitrary strong negation.
This function uses Dombi negation,
with \code{fixpoint} parameter passed internally to \code{\link{neg.dombi}}.

}
\references{
Cintula, P., Hájek, P., & Noguera, C. (2011). Handbook of mathematical fuzzy logic. Studies in Logic.
}
\author{
Igor Yegin
}
\seealso{
\code{\link{neg.dombi}}
}
\examples{
require(NilpotentMinimum)
x <- seq(0, 1, 0.1)
y <- seq(1, 0, -0.1)
nilmin.residuum(x, y)
}
