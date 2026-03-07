nilmin.tnorm <- function(x, type = c("strong", "weak"), fixpoint = 0.5,
                         byrow = TRUE) {
  if(!is.matrix(x) | !is.numeric(x))
    stop("x must be a numeric vector or a matrix")
  if (length(x) == 0L) stop("x must be nonempty")
  if (all(is.na(x)))
    NA_real_
  if(is.matrix(x) & byrow) {
    tmp <- rowSort(x[, colAny(!is.na(x)), drop = FALSE])
    if(ncol(tmp) <= 1L)
      tmp
    else {
      ifelse(rowSums(tmp[, 1:2], na.rm = TRUE) <= 1,
             0,
             pmin(tmp, na.rm = TRUE))
    }
  }
  else if(is.matrix(x) & !byrow) {
    if(is.matrix(x) & byrow) {
      tmp <- colSort(x[, rowAny(!is.na(x)), drop = FALSE])
      if(nrow(tmp) <= 1L)
        tmp
      else {
        ifelse(colSums(tmp[, 1:2], na.rm = TRUE) <= 1,
               0,
               pmin(tmp, na.rm = TRUE))
      }
    }
  }
  else {
    tmp <- sort(as.numeric(x))
    if(sum(tmp[1:2], na.rm = TRUE) <= min(1, length(tmp) - 1))
      0
    else
      min(tmp, na.rm = TRUE)
  }
}
