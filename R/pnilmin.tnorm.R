pnilmin.tnorm <- function(x, y, fixpoint = 0.5) {
  if(!is.numeric(x) | !is.numeric(y))
    stop('x and/or y is not numeric')
  if(any(c(x, y) < 0) | any(c(x, y) > 1))
    stop('All fuzzy truth values must be in the [0, 1] interval')
  ifelse(x <= neg.dombi(y, fixpoint = fixpoint),
         0,
         pmin(x, y, na.rm = TRUE))
}
