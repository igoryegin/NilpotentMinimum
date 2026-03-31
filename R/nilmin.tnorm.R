#' @export
#' @import matrixStats

nilmin.tnorm <- function(x, type = c("strong", "weak"), fixpoint = 0.5,
                         byrow = FALSE) {
  require(matrixStats)
  if(is.data.frame(x)) {
    x <- unname(as.matrix(x))
    warning("data.frame passed to x; converted to matrix")
  }
  if(!is.matrix(x) & !is.numeric(x))
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
      tmp <- rowMins(x) *
        (rowMins(x) > negswitch(rowOrderStats(x, which = 2), type, fixpoint))
      tmp
    }
  }
  else if(is.matrix(x) & !byrow) {
    if(nrow(x) <= 1L)
      x
    else {
      tmp <- colMins(x) *
        (colMins(x) > negswitch(colOrderStats(x, which = 2), type, fixpoint))
      tmp
    }
  }
  else {
    x <- sort(as.numeric(x))
    tmp <- min(x, na.rm = TRUE) * (x[1] > negswitch(x[2], type, fixpoint))
    tmp
  }
}
