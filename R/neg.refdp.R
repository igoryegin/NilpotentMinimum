neg.refdp <- function(x, fixpoint = 0.5) {
  if(!is.numeric(x))
    stop('x is not numeric')
  if(any(x < 0 | x > 1))
    stop('All fuzzy truth values must be in the [0, 1] interval')
  if(fixpoint < 0 | fixpoint >= 1)
    stop('negation fixpoint must be in the [0, 1) interval')
  ifelse(x == 0, 1,
         ifelse(x <= fixpoint, fixpoint, 0)
         )
}
