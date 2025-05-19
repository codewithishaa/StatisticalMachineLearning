

set.seed(1977)
n <- 100
x <- sort(runif(n, -20,20))
y <- 0.5*x^3 + 2.5*x^2 + rnorm(n, 0, 200 + 100)
y <- y/100
plot(x, y, pch = 19)

par(mfrow = c(1,4))

# linear model ------
fit1 <- lm(y ~ x)
plot(x, y, pch = 19)
abline(fit1, lwd = 2, col = "deepskyblue2")

# quadatric model ---
fit2 <- lm(y ~ poly(x, 2, raw = TRUE))
plot(x, y, pch = 19)
lines(x, fit2$fitted.values, col = "magenta3", lwd = 3)

# cubic model -------
fit3 <- lm(y ~ poly(x, 3, raw = TRUE))
plot(x, y, pch = 19)
lines(x, fit3$fitted.values, col = "orange2", lwd = 3)

# d-degree model ----
fit4 <- lm(y ~ poly(x, 150, raw = TRUE))
plot(x, y, pch = 19)
lines(x, fit4$fitted.values, col = "forestgreen", lwd = 3)


# metrics -----------
mods <- list(fit1 = fit1, fit2 = fit2, fit3 = fit3, fit4 = fit4)
res <- sapply(mods, function(l)
{
  r2 <- cor(y, fitted(l))^2
  rmse <- sqrt( var(residuals(l)) )
  var <- var(fitted(l))
  return( c(r2, rmse, var) )
})
colnames(res) <- names(mods)
rownames(res) <- c("r2", "rmse", "var")
res


# multiple samples ------------------------------------------------------------
R <- 100
res <- array(NA, c(R, 3, 4))

for (r in 1:100) {
  x <- sort(runif(n, -20,20))
  y <- 0.5*x^3 + 2.5*x^2 + rnorm(n, 0, 200 + 100)
  y <- y/100
  # plot(x, y, pch = 19)
  
  # linear model
  fit1 <- lm(y ~ x)
  
  # quadatric model
  fit2 <- lm(y ~ poly(x, 2, raw = TRUE))
  
  # cubic model
  fit3 <- lm(y ~ poly(x, 3, raw = TRUE))
  
  # d-degree model
  fit4 <- lm(y ~ poly(x, 150, raw = TRUE))
  
  mods <- list(fit1 = fit1, fit2 = fit2, fit3 = fit3, fit4 = fit4)
  res[r,,] <- sapply(mods, function(l)
  {
    r2 <- cor(y, fitted(l))^2
    rmse <- sqrt( var(residuals(l)) )
    var <- var(fitted(l))
    return( c(r2, rmse, var) )
  })
}
par(mfrow = c(1, 3))
cols <- c("deepskyblue2", "magenta3", "orange2", "forestgreen")
boxplot(res[,1,], main = "r2", col = cols)
legend("bottomright", fill = cols, 
       legend = c("linear", "quad", "cubic", "poly"),
       bty = "n")
boxplot(res[,2,], main = "rmse", col = cols)
boxplot(res[,3,], main = "var", col = cols)

