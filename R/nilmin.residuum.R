nilmin.residuum <- function(x, y, fixpoint = 0.5) {
  if(!is.numeric(x) | !is.numeric(y))
    stop('x and/or y is not numeric')
  if(any(c(x, y) < 0) | any(c(x, y) > 1))
    stop('All fuzzy truth values must be in the [0, 1] interval')
  ifelse(x <= y,
         1,
         pmax(neg.dombi(x, fixpoint = fixpoint), y, na.rm = TRUE)
         )
}
