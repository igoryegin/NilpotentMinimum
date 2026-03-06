nilmin.residuum <- function(x, y, neg = c("dombi", "rdp"), fixpoint = 0.5) {
  neg <- match.arg(neg)
  if(!is.numeric(x) | !is.numeric(y))
    stop('x and/or y is not numeric')
  if(any(c(x, y) < 0) | any(c(x, y) > 1))
    stop('All fuzzy truth values must be in the [0, 1] interval')
  if(neg == "dombi")
    ifelse(x <= y,
           1,
           pmax(neg.dombi(x, fixpoint = fixpoint), y, na.rm = TRUE)
    )
  else if(neg == "rdp")
    ifelse(x <= y,
           1,
           pmax(neg.rdp(x, fixpoint = fixpoint), y, na.rm = TRUE)
    )
  else
    stop('negation must be one of c("dombi", "rdp")')
}
