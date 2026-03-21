\name{nilmin.residuum}
\alias{nilmin.residuum}
\title{
  (Weak) Nilpotent Minimum residuum
}
\description{
  Computes the fuzzy implication (also known as R-implication or residuum)
  defined within the (W)NM fuzzy logic
}
\usage{
nilmin.residuum(x, y, type = c("strong", "weak"), fixpoint = 0.5)
}

\arguments{
  \item{x}{
  a vector of real numbers from 0 to 1; the \emph{left-hand} side of implication
  }
  \item{y}{
  a vector of real numbers from 0 to 1; the \emph{right-hand} side of implication
  }
  \item{type}{
  a string; determines whether strong or weak negation is used (see \link{FuzzyNegation})
  }
  \item{fixpoint}{
  a real number; negation fixpoint to be used inside the function
  }
}

\value{
a vector of fuzzy truth values for the expression \eqn{x \rightarrow y}
}

\details{
When \eqn{x > y}, the residuation operator corresponding to the (W)NM t-norm is

\deqn{x \rightarrow y = \max \left( \sim{x}, y \right)}

where \eqn{\sim x} is an arbitrary strong or w negation operator.
Otherwise, \eqn{x \rightarrow y} evaluates to 1.

This function uses Dombi negation when \code{type = "strong"}, and RDP negation
when \code{type = "weak"}.

}
\references{
Cintula, P., Hájek, P., & Noguera, C. (2011). Handbook of mathematical fuzzy logic. Studies in Logic.
}
\author{
Igor Yegin
}
\seealso{
\code{\link{FuzzyNegation}}
}
\examples{
require(NilpotentMinimum)
x <- seq(0, 1, 0.1)
y <- seq(1, 0, -0.1)
nilmin.residuum(x, y)
nilmin.residuum(x, y, type = "weak")
}
