
library(kernlab)

# generate random points on circles
random_circle <- function(n, r = 1, center = c(0,0), sigma = 0.5, theta = NULL) {
  if ( is.null(theta) ) theta = runif(n, 0, 2*pi)
  x = r*cos(theta) + rnorm(n, center[1], sigma)
  y = r*sin(theta) + rnorm(n, center[2], sigma)
  return( cbind(x, y) )
}

# plot
n <- 50
x <- rbind(random_circle(n, sigma = 0.05),
           random_circle(n, r = 0.5, sigma = 0.05))

colors <- c("forestgreen", "purple3")
class <- rep(1:2, each = n)
plot(x, col = colors[class], pch = 19)


# kernel spaces -----------------------------------------------------

# linear kernel --------
kern <- vanilladot()
kx <- kernelMatrix(x, kernel = kern)
dim(kx)

pairs(kx[,c(1,2,4,7,50,70,80,90,100)], gap = 0, 
      pch = 19, col = colors[class] )


# polynomial kernel ----
kern <- polydot(degree = 3)
kx <- kernelMatrix(x, kernel = kern)

pairs(kx[,c(1,2,4,7,50,70,80,90,100)], gap = 0, 
      pch = 19, col = colors[class] )


# Gaussian radial basis -
kern <- rbfdot(sigma = 0.5)
kx <- kernelMatrix(x, kernel = kern)
pairs(kx[,c(1,2,4,7,50,70,80,90,100)], gap = 0, 
      pch = 19, col = colors[class] )

# smaller sigma
kern <- rbfdot(sigma = 0.01)
kx <- kernelMatrix(x, kernel = kern)
pairs(kx[,c(1,2,4,7,50,70,80,90,100)], gap = 0, 
      pch = 19, col = colors[class] )


# can we separate the classes? --------------------------------------

# original data
y <- factor(class)
fit <- glm(y ~ x, family = binomial)
p <- predict(fit, type = "response")
yhat <- ifelse(p > 0.5, 1, 0)
table(y, yhat)

# maybe with interaction terms?
fit <- glm(y ~ x + x^2, family = binomial)
p <- predict(fit, type = "response")
yhat <- ifelse(p > 0.5, 1, 0)
table(y, yhat)


# GRBF kernel data
kern <- rbfdot(sigma = 0.01)
kx <- kernelMatrix(x, kernel = kern)
#
# NOTE: cannot fit a logistic regression with as many features as sample size!
# we take 50 features
fit <- glm(y ~ kx[,seq(1, 100, by = 2)], family = binomial)
p <- predict(fit, type = "response")
yhat <- ifelse(p > 0.5, 1, 0)
table(y, yhat)                     # WOW!

