nilmin.residuum <- function(x, y, fixpoint = 0.5) {
  if(any(c(x, y) < 0 | c(x, y) > 1))
    stop("All fuzzy truth values must be in the [0, 1] interval")
  ifelse(x <= y,
         1,
         pmax(neg.dombi(x, fixpoint = fixpoint), y)
         )
}
