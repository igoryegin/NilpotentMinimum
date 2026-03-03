neg.dombi <- function(x, fixpoint = 0.5) {
  if(!is.numeric(x))
    stop('x is not numeric')
  if(any(x < 0 | x > 1))
    stop('All fuzzy truth values must be in the [0, 1] interval')
  if(fixpoint <= 0 | fixpoint >= 1)
    stop('negation fixpoint must be strictly between 0 and 1')
  (1 + (1-(1/fixpoint))^2 * x/(1-x)) ^ (-1)
}
