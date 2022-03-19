
# Question 1b
generate_mom_mle <- function(n, theta) {
  # Create uniform distribution between 0 to theta
  generate_uniform_dist <- runif(n, min=0, max=theta)
  
  # Calculate maximum likelihood estimator
  maximum_likelihood_est <- max(generate_uniform_dist) 
  method_of_moments <- 2 * mean(generate_uniform_dist)
  
  return(c(method_of_moments, maximum_likelihood_est))
}

mse_est <- function(n, theta) {
  est <- replicate(1000, mse(n, theta))
  
  # calculate mean squared err
  est <- (est - theta)^2
  
  methodOfMoments <- est[c(FALSE, TRUE)]
  maxLikelihoodEst <- est[c(TRUE, FALSE)]
  
  # store mean of mom and mle
  mean_of_mom <- mean(methodOfMoments)
  mean_of_mle <- mean(maxLikelihoodEst)
  
  return(c(mean_of_mom, mean_of_mle))
}

mean_sq_err <- function(mse_est, n, theta) {
  return(mse_est(n, theta))
}

# Question 1c

# create variables to store results for different values to theta
one_one <- mean_sq_err(mse_est, 1, 1)
one_five <- mean_sq_err(mse_est, 1, 5)
one_fifty <- mean_sq_err(mse_est, 1, 50)
one_hundr <- mean_sq_err(mse_est, 1, 100)

two_one <- mean_sq_err(mse_est, 2, 1)
two_five <- mean_sq_err(mse_est, 2, 5)
two_fifty <- mean_sq_err(mse_est, 2, 50)
two_hundr <- mean_sq_err(mse_est, 2, 100)

three_one <- mean_sq_err(mse_est, 3, 1)
three_five <- mean_sq_err(mse_est, 3, 5)
three_fifty <- mean_sq_err(mse_est, 3, 50)
three_hundr <- mean_sq_err(mse_est, 3, 100)

five_one <- mean_sq_err(mse_est, 5, 1)
five_five <- mean_sq_err(mse_est, 5, 5)
five_fifty <- mean_sq_err(mse_est, 5, 50)
five_hundr <- mean_sq_err(mse_est, 5, 100)

ten_one <- mean_sq_err(mse_est, 10, 1)
ten_five <- mean_sq_err(mse_est, 10, 5)
ten_fifty <- mean_sq_err(mse_est, 10, 50)
ten_hundr <- mean_sq_err(mse_est, 10, 100)

thirty_one <- mean_sq_err(mse_est, 30, 1)
thirty_five <- mean_sq_err(mse_est, 30, 5)
thirty_fifty <- mean_sq_err(mse_est, 30, 50)
thirty_hundr <- mean_sq_err(mse_est, 30, 100)

# create vectors from inputs, mle and mse 
input_vals_for_theta <- c(1, 5, 50, 100)
input_vals_for_N <- c(1, 2, 3, 5, 10, 30)

mom_vals <- c(one_one[1], two_one[1], three_one[1], five_one[1], ten_one[1], thirty_one[1])
mle_vals <- c(one_one[2], two_one[2], three_one[2], five_one[2], ten_one[2], thirty_one[2])

textColor <- c("blue", "red")

# plot graph mse when n = 1, theta = {}
plot(input_vals_for_N, mom_vals, main = "Graph when Theta = 1", type="b", xlab="theta", ylab="Mean Squared Error", col="blue")
lines(input_vals_for_N, mle_vals, type="b", col="red")
legend("topright", legend = c("MOM", "MLE"), col = textColor, text.col = textColor, ncol = 1, lty = 1)
