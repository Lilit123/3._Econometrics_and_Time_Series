View(bankloan)
bankloan$branch<-as.factor(bankloan$branch)
bankloan$ed<-as.factor(bankloan$ed)
bankloan$default<-as.factor(bankloan$default)

# 1.
smp_size <- floor(0.80 * nrow(bankloan))
smp_size
set.seed(123)
train_ind <- sample(seq_len(nrow(bankloan)), size = smp_size)
train_ind

train <- bankloan[train_ind, ]
head(train)

test <- bankloan[-train_ind, ]
head(test)

sapply(test, function(x) sum(is.na(x)))
sapply(train, function(x) sum(is.na(x)))

#2.
model <- glm(train$default ~ .,family=binomial(link='logit'),data=train)
summary(model)


#3.
# I will comment on significant results only
# (This is not significant but since it's a numeric data I decided to interprete it for differe)Branch: On average everything else held constant, for branch15 the log odds of getting loan default will increase 2.045e+01 times compared with branch3, at 95% conf. level.
# Employee: Each one-unit change in employee (years of employment increases by 1 year) will decrease the log odds of getting loan default by -2.289e-01, and its p-value indicates that it is somewhat significant in determining the admit.
# everything else held constant, at 95% Conf level.
# debtinc: Each one-unit change in debtinc (years of employment increases by 1 year) will increase the log odds of getting loan default by 1.141e-01, and its p-value indicates that it is somewhat significant in determining the admit.
# everything else held constant, at 95% Conf level.
# The same is for creddebt: as debtinc....Each one-unit change in creddebt (years of employment increases by 1 year) will increase the log odds of getting loan default by 5.035e-01, and its p-value indicates that it is somewhat significant in determining the admit.
# everything else held constant, at 95% Conf level.

#4.
#Statistically significant results are employ, debtinc, creddebt since their probs are less than 0.05 (alpha = 0.05, for 95% conf level)..It's a bit strange that for example ed, age, income (education level) don't have an impact on loan default. Maybe I would rather drop insignificant variables one by one in order to improve my model, but it would take too much time))

#5.
prediction <- predict(model, test, type = "response")


