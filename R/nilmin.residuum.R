#' @export

nilmin.residuum <- function(x, y, type = c("strong", "weak", "ordsum"), fixpoint = 0.5, alpha = 1/3) {
  type <- match.arg(type)
  if(!is.numeric(x) | !is.numeric(y))
    stop('x and/or y is not numeric')
  if(any(c(x, y) < 0) | any(c(x, y) > 1))
    stop('All fuzzy truth values must be in the [0, 1] interval')
  if(type == "strong")
    ifelse(x <= y,
           1,
           pmax(neg.dombi(x, fixpoint = fixpoint), y, na.rm = TRUE)
    )
  else if(type == "weak")
    ifelse(x <= y,
           1,
           pmax(neg.revdp(x, fixpoint = fixpoint), y, na.rm = TRUE)
    )
  else if(type == "ordsum") {
	if(alpha < 0 | alpha > 0.5)
		stop('alpha must be in [0, 1/2]')
	ifelse(x <= y,
		   1,
		   ifelse(x > y & alpha <= y & x <= 1 - alpha, 
				  1 - x + y - alpha,
				  pmax(1 - x, y))) 
	}
  else
    stop('t-norm type must be one of c("strong", "weak", "ordsum")')
}
