\name{NM.tconorm}
\alias{NM.tconorm}
\alias{pnilmin.tconorm}
\alias{nilmin.tconorm}
%- Also NEED an '\alias' for EACH other topic documented here.
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
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
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
x <- c(0.3, 0.6, 0.7, 0.9, 1)
nilmin.tconorm(x)
nilmin.tconorm(x, type = "weak")
nilmin.tconorm(x, type = "weak", fixpoint = 0.75)

y <- c(0.4, 0.4, 0, 0.5, 0.5)
pnilmin.tconorm(x, y)
}
