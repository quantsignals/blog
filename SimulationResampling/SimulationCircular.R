Nsimulations <- 100000
returns <- c(-0.3,0.0,0.2)
probabilities <- c(0.2,0.5,0.3)
probabilities <- probabilities/sum(probabilities)
simulated_returns <- c()
max_probability <- max(probabilities)
b <- 0.
L = length(probabilities)
i = floor(runif(1,1,L+1))
for( j in 1:Nsimulations)
{
b =  runif(1,0,1+2*max_probability)
while(probabilities[i]<b)
{
b = b - probabilities[i]
i = (i ) %% L+1
}
simulated_returns <-c(simulated_returns,returns[i])
}
hist(simulated_returns)

hist(simulated_returns)
theoretical_mean <- sum(returns*probabilities)
theoretical_var <- sum(returns*returns*probabilities) - theoretical_mean^2

simulated_mean <- mean(simulated_returns)
simulated_var <- var(simulated_returns)

print(sprintf("simulated vs. theoretical"))
print(sprintf("mean: %2.5f %2.5f",simulated_mean, theoretical_mean))
print(sprintf("var: %2.5f %2.5f",simulated_var, theoretical_var))

