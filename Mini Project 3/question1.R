
# Question 1b
mse <- function(n, theta) {
  # Create uniform distribution between 0 to theta
  generate_uniform_dist <- runif(n, min=0, max=theta)
  
  # Calculate maximum likelihood estimator
  maximum_likelihood_est <- max(generate_uniform_dist) 
  method_of_moments <- 2 * mean(generate_uniform_dist)
  
  return(c(method_of_moments, maximum_likelihood_est))
}

mse_est <- function(n, theta) {
  est <- replicate(1000, mse(n, theta))
  est <- (est - theta)^2
  est.methodOfMoments <- est[c(FALSE, TRUE)]
  est.maxLikelihoodEst <- est[c(TRUE, FALSE)]
  
  return(c(mean(est.methodOfMoments), mean(est.maxLikelihoodEst)))
}

mean_sq_err_1_1 <- mse_est(1, 1)

# Question 1c

