
# question 2c
log_max_likelihood <- function(par, datum) {
  res <- length(datum) * log(par) - (par + 1) * sum(log(datum))
  
  return (-res)
}

input <- c(21.42, 14.65, 50.42, 28.78, 11.23)

max_likelihood_est <- optim(par = 0.926, fn = log_max_likelihood, method = "L-BFGS-B", hessian = TRUE, lower = 0.01, dat = input)

# question 2d
hessian_val <- max_likelihood_est$hessian
standard_error <- (1/hessian_val)^0.5

#confidence interval
par_of_max_likelihood_est <- max_likelihood_est$par 
ci <- par_of_max_likelihood_est + c(-1, 1) * as.vector(standard_error) * qnorm(0.975)