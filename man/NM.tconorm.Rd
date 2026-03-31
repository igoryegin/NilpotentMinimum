\name{NM.tconorm}
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
  see \code{\link{nilmin.tnorm}}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
