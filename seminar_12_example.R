# observe some data
observe_data = function(n)
{
  x = runif(n, 0, 10)
  y = 5*x + 3.5 + rnorm(n, 0, 3)
  return(list('x'=x, 'y'=y))
}

# simple linear regression
set.seed(12345)
n = 40
data = observe_data(n)
x = data$x
y = data$y

plot(x,y)

Sxy = sum(x*y) - sum(x)*sum(y)/n
Sxx = sum(x^2) - sum(x)^2/n

b1 = Sxy/Sxx
b1

xbar = mean(x)
ybar = mean(y)
b0 = ybar - b1*xbar
b0


# Plot E[Y | X = x] 
xstar = seq(0, 10, 0.01)
ystar = b1*xstar + b0

lines(xstar, ystar, col='red')


# Confidence and prediction intervals
yhat = b1*x + b0

SSerr = sum((y-yhat)^2)
s = sqrt(SSerr / (n-2))


alpha = 0.05
conf_moe = qt(1-alpha/2, n-2)*s*sqrt(1/n+(xstar-xbar)^2/Sxx)

lines(xstar, ystar + conf_moe, col="green")
lines(xstar, ystar - conf_moe, col="green")


pred_moe = qt(1-alpha/2, n-2)*s*sqrt(1+1/n+(xstar-xbar)^2/Sxx)

lines(xstar, ystar + pred_moe, col="blue")
lines(xstar, ystar - pred_moe, col="blue")

# why is prediciton interval wider than confidence interval (intuitively, without considering formula)?
