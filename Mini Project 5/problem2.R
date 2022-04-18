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


