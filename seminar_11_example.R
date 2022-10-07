# observe some data
observe_data = function(n)
{
  x = runif(n, 0, 10)
  y = 5*x + 3.5 + rnorm(n, 0, 3)
  return(list('x'=x, 'y'=y))
}


observe_data_v2 = function(n)
{
  x = runif(n, 0, 10)
  y = 5*x^2 - 4*x + 3.5 + rnorm(n, 0, 3)
  return(list('x'=x, 'y'=y))
}

# simple linear regression
set.seed(12345)
n = 40
data = observe_data(n)
x = data$x
y = data$y

plot(x,y)

SSxy = sum(x*y) - sum(x)*sum(y)/n
SSxx = sum(x^2) - sum(x)^2/n

b1 = SSxy/SSxx
b1

x_bar = 1/n*sum(x)
y_bar = 1/n*sum(y)
b0 = y_bar - b1*x_bar
b0

x_pred = seq(0, 10, 0.01)
y_hat = b1*x_pred + b0
lines(x_pred, y_hat, col='red')


# quadratic regression
set.seed(12345)
n = 40
data = observe_data_v2(n)
x = data$x
y = data$y

plot(x,y)

X = cbind(rep(1, n), x, x^2)
print(X)

b = solve(t(X)%*%X)%*%t(X)%*%y
b

x_pred = seq(0, 10, 0.01)
y_hat = b[3]*x_pred^2 + b[2]*x_pred + b[1] 
lines(x_pred, y_hat, col='red')
