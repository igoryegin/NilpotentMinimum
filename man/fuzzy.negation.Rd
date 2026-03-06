\name{fuzzy.negation}
\alias{neg.refdp}
\alias{neg.dombi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Strong and Weak Fuzzy Negations
}
\description{
Simple functions to compute Dombi-parametrised (strong) negation or Refined Drastic Product
  (weak) negation
}
\usage{
neg.dombi(x, fixpoint = 0.5)
neg.refdp(x, fixpoint = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{x}{
  a vector of real numbers on an interval from 0 to 1
  }
  \item{fixpoint}{
  a single number \eqn{\nu} invariant under negation, i.e. the one for which
  \eqn{N(x) = x} or \eqn{n(x) = x}
  }
}
\details{

By definition, a fuzzy negation operator is \eqn{N(x)} is a \emph{strong negation} if
it is order-reversing and the law of involution (double negation) holds for it,
i.e., \eqn{N(N(x)) = x}.

For a \emph{weak negation} \eqn{n(x)} it is sufficient that \eqn{n(n(x)) \ge x} is satisfied.


The strong negation operator implemented in this package is the \bold{Dombi} negation,
described by the following formula

\deqn{N(x) = \left( 1 + \left( 1-\frac{1}{\nu} \right)^2 \frac{x}{1-x} \right)^{-1}}

Standard fuzzy negation \eqn{N(x) = 1-x}
is a special case of Dombi negation with \eqn{\nu = 0.5}.

Weak negation is represented by the \bold{Refined Drastic Product (RDP)} negation.
It is defined as a piecewise function

\deqn{n(x) = \begin{cases}
              1, & x = 0 \\
              \nu, & 0 < x \le \nu \\
              0, &  x > \nu
              \end{cases}
              }
It corresponds to induced negation \eqn{x \rightarrow_{RDP} 0} in RDP fuzzy logic, hence the name.
For \eqn{\nu = 0}, \eqn{(n(x))} reduces to \emph{strict negation}, which converts
all nonzero fuzzy scores to 0.
}
\value{
A vector of negated scores
}
\references{
Bianchi, M. (2011) On Some Axiomatic Extensions of the Monoidal T-Norm Based Logic.
\emph{Ledizioni}

Dombi, J., & Jónás, T. (2020). On a strong negation-based representation of modalities.
\emph{Fuzzy Sets and Systems, 407}, 142–160.
https://doi.org/10.1016/j.fss.2020.10.005

Wang, S. (2005). A fuzzy logic for the revised drastic product t-norm.
\emph{Soft Computing, 11(6)}, 585–590.
https://doi.org/10.1007/s00500-005-0024-8
}
\author{
Igor Yegin
}

\examples{
require(NilpotentMinimum)
x <- seq(0, 1, 0.01)
neg.dombi(x)
neg.dombi(x, fixpoint = 0.65)
neg.refdp(x)
neg.refdp(x, fixpoint = 0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
