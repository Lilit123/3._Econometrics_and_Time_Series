# Problem 2.

install.packages("wooldridge")
library(wooldridge)
?wage2
View(wage2)

# (1).

model1 <- lm(log(wage2$wage) ~ wage2$educ)
summary(model1)


logwage <- log(wage2$wage)

neweduc <- wage2$educ - mean(wage2$educ)
newwage <- logwage - mean(logwage)
neweduc_sqr <- neweduc^2
betta1 <- sum(neweduc*newwage)/sum(neweduc_sqr)

# (2).

yloghat <- 5.973062 + 0.059839*wage2$educ
n <- length(wage2$wage)
s <- sqrt((sum((logwage-yloghat)^2))/(n-2))

se1 <- s*sqrt(n/((n*sum(wage2$educ^2) - sum(wage2$educ)^2)))
se1

# (3).

model2 <- lm(log(wage2$wage) ~ wage2$IQ + wage2$educ + wage2$exper + wage2$tenure + wage2$age)
summary(model2)




