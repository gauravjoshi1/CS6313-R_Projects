# z_confidence function checks 
# if the inverse of lambda lies between 
# upper and lower bound 
z_confidence <- function(n, lambda) {
  U <- rexp(n, lambda)
  lower_bound <- mean(U) - qnorm(0.975) * sd(U)/sqrt(n)
  upper_bound <- mean(U) + qnorm(0.975) * sd(U)/sqrt(n)
  
  # return true if lies within the interval
  if (upper_bound > 1/lambda & lower_bound < 1/lambda) return(1)
  
  return(0)
  
}

z_proportion <- function(n, lambda) {
  val <- replicate(5000, z_confidence(n, lambda))
  
  return(length(val[which(val == 1)])/5000)
}


m_star <- function(n, lambda) {
  U <- rexp(n, lambda)
  return (mean(U))
}


b_confidence <- function(n, lambda) {
  U <- rexp(n, lambda)
  inverse_lambda <- 1/lambda
  inverse_mean <- 1/mean(U)
  
  V <- replicate(1000, m_star(n, inverse_mean))
  bound <- sort(V)[c(25, 975)]
  
  if (bound[2] > inverse_lambda & bound[1] < inverse_lambda) return(1)
  
  return (0)
}

b_proportion <- function(n, lambda) {
  val <- replicate(5000, b_confidence(n, lambda))
  one <- val[which(val == 1)]
  return (length(one)/5000)
}


# Create a matrix of z_prop values with sorted by col of L values
zci <- matrix(c(z_proportion(5, 0.01), z_proportion(10, 0.01),  z_proportion(30, 0.01), z_proportion(100, 0.01), 
                z_proportion(5, 0.1),  z_proportion(10, 0.1), z_proportion(30, 0.1), z_proportion(100, 0.1), 
                z_proportion(5, 1), z_proportion(10, 1), z_proportion(30, 1), z_proportion(100, 1),
                z_proportion(5, 10), z_proportion(10, 10), z_proportion(30, 10), z_proportion(100, 10)), nrow = 4, ncol = 4)

bci <- matrix(c(b_proportion(5, 0.01), b_proportion(10, 0.01),  b_proportion(30, 0.01), b_proportion(100, 0.01), 
                       b_proportion(5, 0.1),  b_proportion(10, 0.1), b_proportion(30, 0.1), b_proportion(100, 0.1), 
                       b_proportion(5, 1), b_proportion(10, 1), b_proportion(30, 1), b_proportion(100, 1),
                       b_proportion(5, 10), b_proportion(10, 10), b_proportion(30, 10), b_proportion(100, 10)), nrow = 4, ncol = 4)

par(mfrow=c(2,2))
plot(c(5, 10, 30, 100), zci[, 1], xlab = 'n', ylab = 'proportion', 
     main = "L = 0.01", xlim = c(1, 100), ylim = c(0, 1), 
     type = 'b', col = 'red')
lines(c(5, 10, 30, 100), bci[, 1], type = 'b', col = 'blue')

plot(c(5, 10, 30, 100), zci[, 2], xlab = 'n', ylab = 'proportion', main = "L = 0.1", xlim = c(1, 100), ylim = c(0, 1), type = 'b', col = 'red')
lines(c(5, 10, 30, 100), bci[, 2], type = 'b', col = 'blue')

plot(c(5, 10, 30, 100), zci[, 3], xlab = 'n', ylab = 'proportion', main = "L = 1", xlim = c(1, 100), ylim = c(0, 1), type = 'b', col = 'red')
lines(c(5, 10, 30, 100), bci[, 3], type = 'b', col = 'blue')

plot(c(5, 10, 30, 100), zci[, 4], xlab = 'n', ylab = 'proportion', main = "L = 10", xlim = c(1, 100), ylim = c(0, 1), type = 'b', col = 'red')
lines(c(5, 10, 30, 100), bci[, 4], type = 'b', col = 'blue')

plot(c(0.01, 0.1, 1, 10), zci[1,], xlab = 'lambda', ylab = 'proportion', 
     main = "N = 5", xlim = c(0.01, 10), ylim = c(0, 1), 
     type = 'b', col = 'red')
lines(c(0.01, 0.1, 1, 10), bci[1,], type = 'b', col = 'blue')

plot(c(0.01, 0.1, 1, 10), zci[2,], xlab = 'lambda', ylab = 'proportion', main = "N = 10", xlim = c(0.01, 10), ylim = c(0, 1), type = 'b', col = 'red')
lines(c(0.01, 0.1, 1, 10), bci[2,], type = 'b', col = 'blue')

plot(c(0.01, 0.1, 1, 10), zci[3,], xlab = 'lambda', ylab = 'proportion', main = "N = 30", xlim = c(0.01, 10), ylim = c(0, 1), type = 'b', col = 'red')
lines(c(0.01, 0.1, 1, 10), bci[3,], type = 'b', col = 'blue')

plot(c(0.01, 0.1, 1, 10), zci[4,], xlab = 'lambda', ylab = 'proportion', main = "N = 100", xlim = c(0.01, 10), ylim = c(0, 1), type = 'b', col = 'red')
lines(c(0.01, 0.1, 1, 10), bci[4,], type = 'b', col = 'blue')






