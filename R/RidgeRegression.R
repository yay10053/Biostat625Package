#' The fast method to find beta estimates from Ridge Regression
#' @name RidgeRegression
#' @param X A size n x p Design matrix for regression (without intercept)
#' @param Y A size n vector
#' @param lambda A numeric number, tuning parameter of the ridge regression model
#' @param int Does the regression has intercept? TRUE for has intercept, FALSE for no intercept
#' @return A vector of estimated beta (to 4 precision) of the ridge regression model
#' @examples
#' x <- matrix(rnorm(1:300), 100, 3)
#' y <- rnorm(1:100)
#' fit = RidgeRegression(x, y, lambda = 0.1, int = TRUE)
#' @export
RidgeRegression <- function(X, Y, lambda, int = T){
  n = length(Y)

  ##Check input validity
  stopifnot(int == TRUE || int == FALSE)
  stopifnot(nrow(x) == n)

  ##change design matrix if intercept attr = TRUE
  if(int == TRUE){
    X <- cbind(rep(1,n),X)
  }else if (int == FALSE){
    X <- X
  }else{stop()}

  p = dim(X)[2]
  if (n < p){

    tXX <- crossprod(t(X))
    I <- lambda * diag(n)
    U = chol((tXX + I))
    z <- forwardsolve(U,Y,upper.tri=TRUE,transpose=TRUE) # solve t(U) %*% z = tXY for z
    z = backsolve(U,z)# solve U %*% beta = z for beta
    b = t(X) %*% z
  }else{
    tXX <- crossprod(X)     # t(x) %*% x
    tXY <- crossprod(X,Y)   # t(x) %*% y
    I <- lambda * diag(p)
    U = chol((tXX + I))
    z <- forwardsolve(U,tXY,upper.tri=TRUE,transpose=TRUE) # solve t(U) %*% z = tXY for z
    b = backsolve(U,z)# solve U %*% beta = z for beta
  }
  beta = round(b, 4)


  return(beta)
}
