#' @export
#' @import matrixStats

nilmin.tconorm <- function(x, type = c("strong", "weak"), fixpoint = 0.5,
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
    k <- ncol(x)
    if(ncol(x) <= 1L)
      x
    else {
      tmp <-
        1 - (1 - rowMaxs(x)) *
               (rowMaxs(x) < negswitch(rowOrderStats(x, which = k - 1), type, fixpoint))
      tmp
    }
  }
  else if(is.matrix(x) & !byrow) {
    k <- nrow(x)
    if(nrow(x) <= 1L)
      x
    else {
      tmp <-
        1 - (1 - colMaxs(x)) *
        (colMaxs(x) < negswitch(colOrderStats(x, which = k - 1), type, fixpoint))
      tmp
    }
  }
  else {
    x <- sort(as.numeric(x), decreasing = TRUE)
    tmp <- ifelse((x[1] < negswitch(x[2], type, fixpoint)),
                  max(x, na.rm = TRUE),
                  1)
    tmp
  }
}
