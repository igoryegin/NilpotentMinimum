\name{NM.tnorm}
\alias{pnilmin.tnorm}
\alias{nilmin.tnorm}
\title{
(Weak) Nilpotent Minimum t-norm
}
\description{
(W)NM t-norm (logical 'and') applied to a fuzzy score vector, element-wise to a pair of vectors,
or rowwise/columnwise to a matrix of fuzzy scores.
}
\usage{
nilmin.tnorm(x, type = c("strong", "weak"), fixpoint = 0.5, byrow = FALSE)
pnilmin.tnorm(x, y, type = c("strong", "weak"), fixpoint = 0.5)
}
\arguments{
  \item{x}{
  a numeric vector or a matrix of real numbers from 0 to 1
  }
  \item{y}{
  a vector of real numbers on [0, 1]
  }
  \item{fixpoint}{
  negation fixpoint to be used inside the function
  }
  \item{byrow}{
  a boolean indicating whether the t-norm operation should be applied to matrix rows;
  ignored if \code{x} is not a matrix
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
x <- c(0.3, 0.6, 0.7, 0.9, 1)
nilmin.tnorm(x)
nilmin.tnorm(x, type = "weak")
nilmin.tnorm(x, type = "weak", fixpoint = 0.75)

y <- c(0.4, 0.4, 0, 0.5, 0.5)
pnilmin.tnorm(x, y)
}
