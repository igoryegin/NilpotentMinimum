neg.dombi <- function(x, fixpoint = 0.5) {
  if(any(x < 0 | x > 1))
    stop("All fuzzy truth values must be in the [0, 1] interval")
  (1 + (1-(1/fixpoint))^2 * x/(1-x)) ^ (-1)
}
