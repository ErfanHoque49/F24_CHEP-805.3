########################################################
# CHEP 805.3 - Biostatistics I
########################################################

# Simulation example: 
############################################################################
#Simulate some count data. Fit a Poisson regression model using `glm()`. 
#View the model summary to see the MLE estimates. Using the simulated data, 
#calculate the MLE for Custom Poisson Likelihood using `optim()` function.
############################################################################

# Simulate some count data:

set.seed(123)  # For reproducibility
n <- 100  # Sample size
x <- rnorm(n, mean = 5, sd = 2)  # Predictor variable
lambda <- exp(1 + 0.5 * x)  # Log-linear model for the rate parameter
y <- rpois(n, lambda)  # Generate count data following a Poisson distribution

# Fit a Poisson regression model using glm():

poisson_model <- glm(y ~ x, family = poisson(link = "log"))

# View the model summary to see the MLE estimates:

summary(poisson_model)

## MLE for Custom Poisson Likelihood using optim()

# Define the Poisson log-likelihood function:

poisson_log_likelihood <- function(params, x, y) {
  beta0 <- params[1]
  beta1 <- params[2]
  lambda <- exp(beta0 + beta1 * x)
  log_lik <- sum(dpois(y, lambda, log = TRUE))
  return(-log_lik)  # Return negative log-likelihood for minimization
}


# Use optim() to find the MLEs:

initial_params <- c(0, 0)  # Initial guesses for beta0 and beta1
mle_result <- optim(initial_params, poisson_log_likelihood, x = x, y = y)
mle_result$par  # MLE estimates for beta0 and beta1

# Print the MLE estimates:

cat("MLE for beta0:", mle_result$par[1], "\n")
cat("MLE for beta1:", mle_result$par[2], "\n")



