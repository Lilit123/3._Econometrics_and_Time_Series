#Subject - Times Series :: Homework1
#Author - Hovsepyan Lilit


#Part a.
#load libraries
library(astsa)
library(plotly)
library(stats)

set.seed(1234) #This is for reproducibility
# Now we will generate the data with Normat distribution and sd=1
w = rnorm(150,0,1) #generate extra 50 for burnin time to generate the autoreggression
# Here we get the autoregressions and get rid of the first 50 of them
x = stats::filter(w, filter = c(0,-.9), method = "recursive")[-(1:50)]
# Filter x
v = stats::filter(x, rep(1/4,4), sides = 1)
# Generic plotting
plot.ts(x, main = "AR(2)")
par(new = TRUE)
lines(v, type = "l", col = "red")


#Part b.
x.b =  cos(2*pi*(1:150)/4)
x.b = x.b[-(1:50)]
v.b = stats::filter(x.b,rep(1/4, 4), sides = 1)
# Generic plotting
plot.ts(x.b, main = "AR(2)")
par(new = TRUE)
lines(v.b, col="red")


#Part c.

x.b =  cos(2*pi*(1:150)/4) + w
x.b = x.b[-(1:50)]
v.b = stats::filter(x.b,rep(1/4, 4), sides = 1)
# Generic plotting
plot.ts(x.b, main = "AR(2)")
par(new = TRUE)
lines(v.b, col="red")


#Part d.

#In part b. we clearly see that there is a seasonality component, since we see that 
#there are cycles that repeat regularly over time. 
#Seasonal adjustment is a statistical method for removing the seasonal component
#of a time series that exibits a seasonal pattern. In C by adding some
#N(0,1) noice we see that we did seasonal adjustments in the data

























