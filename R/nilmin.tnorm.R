nilmin.tnorm <- function(x, type = c("strong", "weak"), fixpoint = 0.5,
                         byrow = FALSE) {
  require(matrixStats)
  if(!is.matrix(x) | !is.numeric(x))
    stop("x must be a numeric vector or a matrix")
  if (length(x) == 0L) stop("x must be nonempty")
  if (all(is.na(x)))
    NA_real_
  type <- match.arg(type)
  negswitch <- function(x, type, fixpoint) switch(type,
                                        strong = neg.dombi(x, fixpoint = fixpoint),
                                        weak = neg.revdp(x, fixpoint = fixpoint))
  if(is.matrix(x) & byrow) {
    if(ncol(x) <= 1L)
      x
    else {
      rowMins(x) * (rowMins(x) > negswitch(rowOrderStats(x, which = 2), type, fixpoint))
    }
  }
  else if(is.matrix(x) & !byrow) {
    if(is.matrix(x) & byrow) {
      if(nrow(x) <= 1L)
        x
      else {
        colMins(x) * (colMins(x) > negswitch(colOrderStats(x, which = 2), type, fixpoint))
      }
    }
  }
  else {
    tmp <- sort(as.numeric(x))
    min(tmp, na.rm = TRUE) * (tmp[1] > negswitch(tmp[2], type, fixpoint))
  }
}
