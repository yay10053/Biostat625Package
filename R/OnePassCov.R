#' One Pass Covariance between two numeric vectors
#' @name OnePassCov
#' @param x - A size n numeric vector
#' @param y - A size n numeric vector
#' @return A numeric value of covariance between x and y with n-1 as denominator
#' @examples
#' x <- rnorm(1:100)
#' y <- rnorm(1:100)
#' OnePassCov(x,y)
#' @export

OnePassCov <- function(x, y) {
  ##Check input validity
  stopifnot(length(x) == length(y))

  n.x = 0
  avg.x = 0
  n.y = 0
  avg.y = 0
  cov.xy = 0

  for(i in 1:length(x)) {
    xi = x[i]
    yi = y[i]
    n.x = n.x + 1
    n.y = n.y + 1
    avg.y = avg.y + (1/i)*(yi - avg.y)
    x.diff = (xi - avg.x)
    y.diff = (yi - avg.y)

    part = x.diff*y.diff
    cov.xy = part + cov.xy
    avg.x = avg.x + (1/i)*(xi - avg.x)
  }

  return( cov.xy/(length(x)-1))

}
