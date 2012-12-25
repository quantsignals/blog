returns <- c(-0.3,0.0,0.2)
probabilities <- c(0.2,0.5,0.3)

probabilities <- probabilities/sum(probabilities)
cw <- cumsum(probabilities)      # cumulative distribution

Nsimulations <- 100000
simulated_returns <- sapply(runif(Nsimulations,0,1), function(x) (returns[min(which(cw-x>0))]))

hist(simulated_returns)
theoretical_mean <- sum(returns*probabilities)
theoretical_var <- sum(returns*returns*probabilities) - theoretical_mean^2

simulated_mean <- mean(simulated_returns)
simulated_var <- var(simulated_returns)

print(sprintf("simulated vs. theoretical"))
print(sprintf("mean: %2.5f %2.5f",simulated_mean, theoretical_mean))
print(sprintf("var: %2.5f %2.5f",simulated_var, theoretical_var))

