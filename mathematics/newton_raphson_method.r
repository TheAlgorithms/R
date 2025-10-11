newton_raphson <- function(f, fprime, x0, tol = 1e-6, max_iter = 100) {
  x <- x0
  for (i in 1:max_iter) {
    deriv <- fprime(x)
    if (abs(deriv) < .Machine$double.eps) {
      warning("Derivative is zero. Newton-Raphson method fails.")
      return(NA)
    }
    x_new <- x - f(x) / deriv
   if (abs(x_new - x) < tol) {
     return(x_new)
   }
   x <- x_new
 }
  return(x)
}
