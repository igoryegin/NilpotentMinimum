pnilmin.tnorm <- function(x, y, fixpoint = 0.5) {
  ifelse(x <= neg.dombi(y, fixpoint = fixpoint),
         0,
         pmin(x, y, na.rm = TRUE))
}
