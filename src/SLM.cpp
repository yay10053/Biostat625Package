#include <Rcpp.h>
//' Simple Linear Regression function with Rcpp
//' @name SLM
//' @param x A size n predictor vector
//' @param y A size n outcome vector
//' @returns A list containing: intercept, beta estimate, std.Error, t.value, p.value from the simple linear regression
//' @export

using namespace Rcpp;

// [[Rcpp::export]]
List SLM(NumericVector x, NumericVector y){
  y = y - mean(y);
  x = x - mean(x);
  int  n = y.size();
  double ysq = sum( y * y );
  ysq /= n - 1.0;
  double xsq = sum( x * x );
  xsq /= n - 1.0;
  double xy = sum( x * y );
  xy /= n - 1.0;
  double rxy = xy;
  rxy /= sqrt( ysq * xsq );
  double b = rxy * sqrt( ysq / xsq );
  double z = mean(y) - b * mean(x);
  double se_b = sqrt(ysq * ( 1.0 - rxy * rxy ) / (n-2.0) / xsq);
  NumericVector tstat;
  tstat.push_back(rxy * sqrt( ( n - 2 ) / ( 1 - rxy * rxy ) ));
  NumericVector p = pt(abs(tstat), n - 2 ,0, 0)*2;
  return Rcpp::List::create( Rcpp::Named("intercept") = z,
                             Rcpp::Named("beta.est") = b,
                             Rcpp::Named("Std.Error") = se_b,
                             Rcpp::Named("t.value") = tstat[0], Rcpp::Named("p.value") = p[0]);
}

