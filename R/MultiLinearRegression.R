#' Multiple Linear regression function with Cholesky Decomposition
#' @param x A size n x p Design matrix for regression (without intercept)
#' @param y A size n vector
#' @param int Does linear regression has intercept? TRUE for has intercept, FALSE for no intercept
#' @returns
#' A list with the following elements:
#'
#' beta.est :A vector of estimated beta for y ~ x *beta + error
#'
#' Std.Error :A vector of standard error for each estimate
#'
#' t.val :A vector of t-statistic for each estimate
#'
#' p.value :A vector of p-values of each estimate
#'
#' fitted :A vector of fitted values
#'
#' SSE :A numeric value for the Sum of squared error of the model
#'
#' R.square :A numeric value for the R square of the model
#'
#' @examples
#' x <- matrix(rnorm(1:300), 100, 3)
#' y <- rnorm(1:100)
#' fit = MultiLinearRegression(x,y,int = TRUE)
#' @export
MultiLinearRegression <- function(x,y, int = TRUE){
  n <- length(y)
  p <- ncol(x)
  ##Check input validity
  stopifnot(int == TRUE || int == FALSE)
  stopifnot(nrow(x) == n)

  ##change design matrix if intercept attr = TRUE
  if(int == TRUE){
    x <- cbind(rep(1,n),x)
  }else if (int == FALSE){
    x <- x
  }else{stop()}

  ##chol decomposition
  tXX <- crossprod(x)
  tXY <- crossprod(x,y)
  U <- chol(tXX)
  z <- forwardsolve(U,tXY,upper.tri=TRUE,transpose=TRUE)
  ans <- as.vector(backsolve(U,z))

  ##Y fitted value
  yhat = x%*%ans
  df = n - (p + 1)
  SSE = sum((y - yhat)^2)
  SSR = sum((mean(y) - yhat)^2)
  SST = sum((y - mean(y))^2)
  sigma2hat = SSE/(n - (p + 1))
  se = sqrt(sigma2hat*diag(solve(tXX)))
  Rsq = 1 - SSE/SST
  tstat = ans/se
  p.val = 2 * pt(abs(tstat), df = 96, lower.tail = FALSE)
  return(list(beta.est = ans, Std.Error = se, t.val = tstat, p.value = p.val, fitted = yhat, SSE = SSE, R.square = Rsq))
}


