## Q1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

f <- function(x,w,u){
    sum(w*(x-u)^2)
}

umin <- optimize(f,interval = c(-1,1),w=w,x=x)
## the minimum value is obtained for u equals to
umin$minimum
## the minimum value of the equation is 
umin$objective

## Q2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

fit <- lm(y ~ x-1)
# the slope is given by 
fit$coefficients

## Q3
data(mtcars)
attach(mtcars)
fit <- lm(mpg ~ wt)
detach(mtcars)

## Q4
corrXY = 0.5
ratiosYsX = 2 # sY = sX/2
beta1 = corrXY* ratiosYsX
beta1

## Q5
corrXY <- 0.4
X <- 1.5
sY = sX = 1
beta1 = corrXY*sY/sX 
Y = X*beta1
Y

## Q6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
(x-mean(x))/sd(x)

## Q7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
fit <- lm(y ~ x)
fit$coefficients

## Q8
# You know that both the predictor and response have mean 0. What can be said about the intercept when you fit a linear regression?
# It must be identically 0. Linear regression always pass by mean of X and Y

## Q9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
f <- function(x,u){
    sum((x-u)^2)
}
umin <- optimize(f,interval = c(-1,1),x=x)
## the minimum value is obtained for u equals to
umin$minimum
## the minimum value of the equation is 
umin$objective

## Q10
## beta1 = corrXY*sY/sX
## gamma1 = corrXY*sX/sY
## beta1/gamma1 = sY²/sX² = varY/varX
