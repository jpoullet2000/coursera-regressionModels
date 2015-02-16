## Q1
library(car)
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
#  two sided hypothesis test of whether β1 from a linear regression model is 0 or not
linearHypothesis(fit, "x=0")
# or more simply
summary(fit)

## Q2
#  residual standard deviation
summary(fit)

## Q3

data(mtcars)
newdata = data.frame(wt = mean(mtcars$wt))
predict(fit,newdata, interval="confidence")

## Note that the following is not correct 
# -------------------
# with intercept
fit <- lm(mpg ~ wt, data=mtcars)
(ci <- confint(fit))
c(ci[1,1] + mean(mtcars$wt)*ci[2,1],ci[1,2] + mean(mtcars$wt)*ci[2,2])

# without intercept
fit <- lm(mpg ~ wt-1, data=mtcars)
(ci <- confint(fit))
c(mean(mtcars$wt)*ci[1,1],mean(mtcars$wt)*ci[1,2])
# -------------------

## Q4
# What is the weight coefficient interpreted as
# The estimated expected change in mpg per 1,000 lb increase in weight.

## Q5
fit <- lm(mpg ~ wt, data=mtcars)
newdata <- data.frame(wt = 3)
predict(fit, newdata, interval = "confidence")

## Q6
fit <- lm(mpg ~ wt, data=mtcars)
(ci <- confint(fit))
ci[1,1]+2*ci[2,1]-ci[1,1] 

## some more for plotting purposes
## the confidence interval for the prediction depends on the point where we make the prediction
## so the width of this interval varies with the wt variable
fit <- lm(mpg ~ wt, data=mtcars)
pred <- predict(fit,interval="confidence")
plot(mtcars$wt,mtcars$mpg)
points(mtcars$wt, pred[,2], col = 'red', pch = 22,lty=2)
points(mtcars$wt, pred[,3], col = 'blue', pch = 22, lty=2) 

## Q7
# If my X from a linear regression is measured in centimeters and I convert it to meters what would happen to the slope coefficient?
# It would get multiplied by 100.

## Q8
## I have an outcome, Y, and a predictor, X and fit a linear regression model with Y=β0+β1X+ϵ to obtain β^0 and β^1. What would be the consequence to the subsequent slope and intercept if I were to refit the model with a new regressor, X+c for some constant, c?

# The new intercept would be β^0−cβ^1
# Proof:
# # Y=β0+β1X+ϵ = β0w+β1w (X+c) => β1w = β1; β0w + β1w*c = β0, so the new intercept β0w = β0-β1*c 
## Q9
fit <- lm(mpg ~ wt,data=mtcars)
fit0 <- lm(mpg ~ 1,data=mtcars)
summary(fit)
summary(fit0)

sum(fit$residuals^2)/sum(fit0$residuals^2)

## Q10
# Do the residuals always have to sum to 0 in linear regression?
# If an intercept is included, then they will sum to 0.
