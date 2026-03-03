\name{pnilmin.tnorm}
\alias{pnilmin.tnorm}
\title{
Parallel Nilpotent Minimum (NM) t-norm
}
\description{
NM t-norm applied element-wise to a pair of fuzzy score vectors
}
\usage{
pnilmin.tnorm(x, y, fixpoint = 0.5)
}
\arguments{
  \item{x, y}{
  real-valued vectors on an interval from 0 to 1
  }
  \item{fixpoint}{
  negation fixpoint to be used inside the function
  }
}
\value{
  a vector of truth values
}
\details{
See \code{\link{nilmin.tnorm}}
}

\references{
Cintula, P., Hájek, P., & Noguera, C. (2011). Handbook of mathematical fuzzy logic. Studies in Logic.
}
\author{
  Igor Yegin
}
\seealso{
\code{\link{nilmin.tnorm}}
}
\examples{
require(NilpotentMinimum)
x <- c(0, 0.2, 0.21, 0.5, 1)
y <- c(0.8, 0.8, 0.8, 0.5, 0.5)
nilmin.residuum(x, y)
}
