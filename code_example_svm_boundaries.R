library(kernlab)

# generate random points on circles
random_circle <- function(n, r = 1, center = c(0,0), sigma = 0.5, theta = NULL) {
  if ( is.null(theta) ) theta = runif(n, 0, 2*pi)
  x = r*cos(theta) + rnorm(n, center[1], sigma)
  y = r*sin(theta) + rnorm(n, center[2], sigma)
  return( cbind(x, y) )
}

# plot
n <- 100
x <- rbind(random_circle(n, sigma = 0.05),
           random_circle(n, r = 0.5, sigma = 0.05))
colnames(x) <- c("x1", "x2")

colors <- c("forestgreen", "purple3")
class <- rep(1:2, each = n)
plot(x, col = colors[class], pch = 19)



# train different svms ----------------------------------------------
y <- class
x <- scale(x)

# polynomial - degree 2
svm_poly_1 <- ksvm(x, y, type = "C-svc",
                   kernel = "polydot", kpar = list(degree = 2))

# polynomial - degree 3
svm_poly_2 <- ksvm(x, y, type = "C-svc",
                   kernel = "polydot", kpar = list(degree = 3))

# GRBF - sigma 0.2
svm_grbf_1 <- ksvm(x, y, type = "C-svc",
                   kernel = "rbfdot", kpar = list(sigma = 0.2))

# GRBF - sigma 1
svm_grbf_2 <- ksvm(x, y, type = "C-svc",
                   kernel = "rbfdot", kpar = list(sigma = 1))


# plot decision boundaries ------------------------------------------
plot_svm <- function(svm, x, grid_lenght = 100, 
                     pal = function(n) hcl.colors(n, palette = "BrBg"),
                     pch = c(19, 17),   # class points
                     level = c(-1, 0, 1), 
                     show_support_vectors = TRUE, ...) 
{
  x <- as.matrix(x)
  y <- svm@ymatrix
  
  # set ranges for contour plot and crate grid of values
  r1 <- range(x[,1]) + c(-0.1, 0.1)
  r2 <- range(x[,2]) + c(-0.1, 0.1)
  xx <- seq(r1[1], r1[2], length = grid_lenght)
  yy <- seq(r2[1], r2[2], length = grid_lenght)
  grid <- expand.grid(xx, yy)
  
  # obtain values of the SVM classifier over grid values
  pred <- predict(svm, newdata = grid, type = "decision")
  
  # produce contour plot
  z <- matrix(pred, nrow = grid_lenght)
  filled.contour(xx, yy, z, color.palette = pal,
                 plot.axes = {
                   points(x, col = adjustcolor("black", 0.7), pch = pch[y])
                   if ( show_support_vectors ) points(svm@xmatrix[[1]][,1], svm@xmatrix[[1]][,2], 
                                                      col = "#ff4d5c", pch = 0, cex = 1)
                   contour(xx, yy, z = z, 
                           levels = level, add = TRUE, lty = 2, col = "black")
                 }, ...)
}


plot_svm(svm_poly_1, x, main = "Poly 2")
plot_svm(svm_poly_2, x, main = "Poly 3")
plot_svm(svm_grbf_1, x, main = "GRBF 0.2")
plot_svm(svm_grbf_2, x, main = "GRBF 1")

