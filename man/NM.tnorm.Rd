\name{NM.tnorm}
\alias{NM.tnorm}
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
The formula for (Weak) Nilpotent Minimum t-norm is

\deqn{x ⊛ y = \left\{ \begin{array}{ll}
\min(x, y), & x > n(y) \\
0, & x \le n(y)
\end{array}
\right.}

Nilpotent Minimum is the \emph{pointwise largest} t-norm out of all t-norms with
strong (i.e., order-reversing and involutive) negation.
}

\references{
Esteva, F., & Godo, L. (2001). Monoidal t-norm based logic: towards a logic for left-continuous t-norms. 
	\emph{Fuzzy Sets and Systems, 124(3)}, 271–288. https://doi.org/10.1016/s0165-0114(01)00098-7

Wang, S. (2005). A fuzzy logic for the revised drastic product t-norm. 
	\emph{Soft Computing, 11(6)}, 585–590. https://doi.org/10.1007/s00500-005-0024-8

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
