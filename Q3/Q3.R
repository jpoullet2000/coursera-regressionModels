## Q1
data(mtcars)
df <- mtcars
df$cyl <- as.factor(df$cyl)
fit <- lm(mpg ~ cyl + wt, data = df)
summary(fit)
# expected change in mpg if increase of 4 for cyl
fit$coefficients[3]

## Q2
fit <- lm(mpg ~ cyl + wt, data = df)
fitUW <- lm(mpg ~ cyl, data = df)
summary(fit)
summary(fitUW)
# Holding weight constant, cylinder appears to have less of an impact on mpg than if weight is disregarded.

## Q3
library("lmtest")
fit <- lm(mpg ~ cyl + wt, data = df)
fitU <- lm(mpg ~ cyl*wt, data = df)
lrtest(fit, fitU)

## The likelihood-ratio test is appropriate only if the two models you are comparing are nested, i. e., if one can be retrieved from the other, e. g., by fixing parameters (e. g., to zero). Models with more parameters will always fit better, the question the LR test answers is whether the increase in fit is defensible given the amount of added parameters.  To compare non-nested models, you may use information criteria such as AIC or BIC (the smaller the better).

## Q4
fit <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(fit)
# How is the wt coefficient interpretted?
# The estimated expected change in MPG per one ton increase in weight for a specific number of cylinders (4, 6, 8).

## Q5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
lm.influence(fit)
max(lm.influence(fit)$hat)

## Q6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
(imf <- influence.measures(fit))
df <- as.data.frame(imf$infmat)
df[which(df$hat == max(df$hat)),'dfb.x']

## Q7
# Consider a regression relationship between Y and X with and without adjustment for a third variable Z. Which of the following is true about comparing the regression coefficient between Y and X with and without adjustment for Z.
# It is possible for the coefficient to reverse sign after adjustment. For example, it can be strongly significant and positive before adjustment and strongly significant and negative after adjustment.





