\name{NM.tconorm}
\alias{NM.tconorm}
\alias{pnilmin.tconorm}
\alias{nilmin.tconorm}
\title{
(Weak) Nilpotent Minimum t-conorm
}
\description{
(W)NM t-conorm (logical 'or') applied to a vector of truth values,
element-wise to a vector pair, or row/columnwise to a truth value matrix
}
\usage{
pnilmin.tconorm(x, type = c("strong", "weak"), fixpoint = 0.5)
nilmin.tconorm(x, y, type = c("strong", "weak"), fixpoint = 0.5,
                         byrow = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  see \code{\link{NM.tnorm}}
}
\details{
The formula for (Weak) Nilpotent Minimum t-conorm is given by

\deqn{x ⨁ y = \left\{ \begin{array}{ll}
1, & x > n(y) \\
\max(x, y), & x \le n(y)
\end{array}
\right.
}

}
\value{
a vector (or a single number) of fuzzy truth values
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
\code{\link{NM.tnorm}}
}
\examples{
require(NilpotentMinimum)
x <- c(0, 0.1, 0.1, 0.3, 0.4, 0.45, 0.49)
nilmin.tconorm(x)
nilmin.tconorm(x, type = "weak")

y <- c(0, 0.7, 0., 0.7, 0, 0.7, 0)
pnilmin.tconorm(x, y)
}
