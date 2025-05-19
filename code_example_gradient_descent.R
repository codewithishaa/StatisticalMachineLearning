
data <- read.csv("data_toy_binary.csv")
dim(data)
y <- ifelse(data$y == "b", 1, 0)
x <- as.matrix(data[,-1])
n <- nrow(x)


## gradient descent ----------------------------------------------------------

# initialization
w_prev <- runif(ncol(x), -0.1, 0.1)   # try other initialization
obj_prev <- log(.Machine$double.xmax)
obj_vec <- c()
tol <- 1e-05
maxit <- 1000
crit <- TRUE
iter <- 0

# learning rate
eta <- 0.1

# functions
sigma <- function(x, w) {
  lin <- x%*%w
  e <- exp(lin)
  out <- e/(1 + e)
  return(out)
}
#
loss <- function(y, x, w) {
  s <- sigma(x, w)
  y*log(s) + (1 - y)*log(1 - s)
}

# gradient descent algorithm
while ( crit ) {
  w <- w_prev - eta/n * crossprod(x, (sigma(x, w_prev) - y) )
  obj <- -mean( loss(y, x, w) )
  w_prev <- w
  err <- (obj_prev - obj)
  obj_prev <- obj
  iter <- iter + 1
  crit <- (err > tol) & (iter < maxit)
  obj_vec <- c(obj_vec, obj)
}


# comparison -----------------------------------------------------------------
# plot loss function
plot(obj_vec, type = "l")

# fit model using glm
data$y <- factor(data$y)
fit <- glm(y ~ ., data = data[,-2], family = "binomial")
glm_w <- fit$coefficients

# compare coefficient estimates
plot(w, glm_w, pch = 19, col = adjustcolor("black", 0.5))
abline(0, 1, col = "darkorange", lty = 2)

# can also compare predicted probabilities
plot(sigma(x, w) - fitted(fit), pch = 19, col = adjustcolor("black", 0.1))
abline(h = 0, col = "darkorange", lty = 2)

