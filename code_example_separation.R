
# no separation -----------------------------------------------------
x <- runif(200, -3,3)
y <- ifelse(x + rnorm(100, sd = 0.5) > 0, 1, 0) 
cols <- c("purple", "forestgreen")[y+1]

plot(x, y, pch = 19, col = adjustcolor(cols, 0.7), yaxt = "n")
axis(2, at = 0:1)
abline(v = 0, lty = 2)

fit <- glm(y ~ x, family = "binomial")
summary(fit)

lines(sort(x), sort(predict(fit, type = "response")),
      col = "black", lwd = 3)
text(1, 0.5, paste0("w = ", round(fit$coefficients[2], 2)))



# complete separation -----------------------------------------------
x <- runif(200, -3,3)
y <- ifelse(x > 0, 1, 0)
cols <- c("purple", "forestgreen")[y+1]

plot(x, y, pch = 19, col = adjustcolor(cols, 0.7), yaxt = "n")
axis(2, at = 0:1)
abline(v = 0, lty = 2)

fit <- glm(y ~ x, family = "binomial")
summary(fit)

lines(sort(x), sort(predict(fit, type = "response")),
      col = "black", lwd = 3)
text(1, 0.5, paste0("w = ", round(fit$coefficients[2], 2)))



# complete separation with multiple variables -----------------------
set.seed(1247)    # to reproduce all examples

### complete separation
n <- 200
x1 <- rbeta(n, 2, 4)
x2 <- rbeta(n, 2, 4)
x3 <- rbeta(n, 2, 2)
y <- ifelse((x1 + x2) > 0.7, 1, 0)
cols <- c("purple", "forestgreen")[y+1]

# the full linear combination seems ok
plot(x1 + x2 + x3, y, pch = 19, col = adjustcolor(cols, 0.5), yaxt = "n")
axis(2, at = 0:1)

# however...
plot(x1 + x2, y, pch = 19, col = adjustcolor(cols, 0.5), yaxt = "n")
axis(2, at = 0:1)
abline(v = 0.7, lty = 2)
# the linear combination x1 + x2 perfectly describes y 
# since for x1 + x2 > 0.7 we have y = 1 and for
# x1 + x2 < 0.7 we have y = 0

fit <- glm(y ~ x1 + x2 + x3, family = "binomial")
summary(fit)
# the test says that actually x1 and x2 are not
# useful at describing y, but they should be since
# y ~ x1 + x2!


# check fitted line in x1,x2 space
lines(sort(x1 + x2), sort(predict(fit, type = "response")),
      col = "black", lwd = 3)
text( 1, 0.5, paste0( "w_1 + w_2 = ", round(fit$coefficients[2] + fit$coefficients[3], 2) ) )


# check fitted line in x1,x2,x3 space
plot(x1 + x2 + x3, y, pch = 19, col = adjustcolor(cols, 0.5), yaxt = "n")
axis(2, at = 0:1)
lines(sort(x1 + x2 + x3), sort(predict(fit, type = "response")),
      col = "black", lwd = 3)
text( 1.6, 0.5, paste0( "w_1 + w_2 = ", round(fit$coefficients[2] + fit$coefficients[3], 2) ) )
text( 1.45, 0.3, paste0( "w_3 = ", round(fit$coefficients[4], 2) ) )



# extreme values ----------------------------------------------------
set.seed(1247)    # to reproduce all examples

### here there is no separation
#
n <- 200
x1 <- rbeta(n, 2, 4)
x2 <- rbeta(n, 2, 4)
x3 <- rbeta(n, 2, 2)
y <- ifelse((x1 + x2 + 0.2*x3) + rnorm(n, sd = 0.1) > 0.7, 1, 0)
cols <- c("purple", "forestgreen")[y+1]

# no separation
plot(x1 + x2 + x3, y, pch = 19, col = adjustcolor(cols, 0.5), yaxt = "n")
axis(2, at = 0:1)
plot(x1 + x2, y, pch = 19, col = adjustcolor(cols, 0.5), yaxt = "n")
axis(2, at = 0:1)

fit <- glm(y ~ x1 + x2 + x3, family = "binomial")
summary(fit)      # all good here!

# add some extreme values
set <- which(x1 > 0.6)
x1[set] <- runif(length(set), 1, 3)
plot(x1, y, pch = 19, col = adjustcolor(cols, 0.5), yaxt = "n")
axis(2, at = 0:1)
# still no separation but some large values of x

fit <- glm(y ~ x1 + x2 + x3, family = "binomial")
summary(fit)
predict(fit, type = "response")[set]
# this causes the warning, as the estimated
# probabilities for some of the extreme values are
# numerically 1, but there is no separation
# and inference can be performed on the coefficients

