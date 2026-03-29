#' @export

pnilmin.tnorm <- function(x, y, type = c("strong", "weak"), fixpoint = 0.5) {
  type <- match.arg(type)
  negswitch <- function(x, type, fixpoint) switch(type,
                                                  strong = neg.dombi(x, fixpoint = fixpoint),
                                                  weak = neg.revdp(x, fixpoint = fixpoint))
  if(!is.numeric(x) | !is.numeric(y))
    stop('x and/or y is not numeric')
  if(any(c(x, y) < 0) | any(c(x, y) > 1))
    stop('All fuzzy truth values must be in the [0, 1] interval')
  ifelse(x <= negswitch(y, type, fixpoint),
         0,
         pmin(x, y, na.rm = TRUE))
}
