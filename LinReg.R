# The data give the speed (in mph) of cars and the distances taken to stop (in ft), recorded in 1920s
data(cars) # Loading data set 'cars' (R's internal data set)
attach(cars) # Making variables available (outside of 'namespace')
cars
plot(cars)
lmFit<-lm(dist~speed) # Linear regression
lmFit
abline(lmFit) # Plot regression line