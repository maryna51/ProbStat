# Author: Filip Ekström Kelvinius, Statistics and Machine Learning, Linköping university, Sweden
# e-mail: filip.ekstrom@liu.se


geometric_likelihood= function(x, theta)
{
  n = length(x)
  return(theta^n *(1-theta)^(sum(x) - n))
}

beta_prior = function(theta, alpha, beta)
{
  return(dbeta(theta, alpha, beta))
}

beta_posterior = function(theta, alpha_prior, beta_prior, x)
{
  n = length(x)
  return(dbeta(theta, n+alpha_prior, sum(x) - n + beta_prior))
}

gen_data = function(n, theta, alpha, beta)
{
  # function to generate data
  data = rgeom(n, theta) + 1 # +1 to have same interpretation of Geometric distribution as in course
  plot_all(data, alpha, beta, main="Manipulating plot")
}

plot_all = function(x, alpha_prior, beta_prior, main="")
{
  # empty plot
  plot(1, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 5), main=main)
  
  
  theta = seq(0, 1, 0.001)
  
  # plot prior
  prior = beta_prior(theta, alpha_prior, beta_prior)
  lines(theta, prior, col='green')
  
  # plot scaled likelihood (scaling to enable)
  likelihood = geometric_likelihood(x, theta)
  likelihood_rescaled = likelihood/max(likelihood)
  lines(theta, likelihood_rescaled, col='red')
  
  # plot posterior
  posterior = beta_posterior(theta, alpha_prior, beta_prior, x)
  lines(theta, posterior, col='blue')
  
  # legend
  legend(0.7, 5, legend=c(paste("Prior, alpha =", alpha_prior, ", beta =", beta_prior),
                            "Likelihood (scaled)",
                            "Posterior"),
         col=c("green", "red", "blue"),
         lty=1,
         cex=0.8
  )
}


# data from exercise 10.32
data = c(2,3,5,8,2)

# Theta is probability that an event occur
# For example, could be probability that we get a 6 when rolling a dice
# Or data is the number of times it takes to get a 6, and it therefore follows a geometric distribution
# Prior should encode what you think about theta before observing any data (your prior belief)
# Let's compare some different choices

# Beta(1,1) = Uniform distribution - we don't know anything about theta
plot_all(data, 1,1, "Uniform prior")

# Maybe we talk with someone with experience of these particular dice, and they give us some advice

# They think it is more likely to be around 0.5:
plot_all(data, 3,3, "Prior around 0.5")

# They think it is more likely to be a lower value (around 0.2):
plot_all(data, 2,5, "Prior around 0.2")

# They think it is more likely to be a higher value (around 0.8)
plot_all(data, 5, 2, "Prior around 0.8")

# What happens with the prior, likelihood and posterior in the different cases? 
# If you would compare using the posterior to estimate theta and using a maximum likelihood estimation,
# what would be different? 

# install.packages("manipulate") # uncomment to install package manipulate
library(manipulate)



# experiment yourself with different priors, and using different amount of data
# what happens to the posterior when you have a small or large amount of data? 
# You might have to run twice to get the possibility to manilpulate the values
manipulate(
  gen_data(n, theta, alpha, beta),
  alpha = slider(0, 10, step=0.1, initial=1, label="alpha"),
  beta = slider(0, 10, step=0.1, initial=1, label="beta"),
  n = slider(1,300, step=10, initial=10, label="Number of datapoints"),
  theta = slider(0, 1, step=0.05, initial=0.2, label="theta")
)


