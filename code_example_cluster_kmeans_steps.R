
# colors for plots (I know, they look terrible)
colors <- c("darkorchid3", "olivedrab", "darkorange2", "deepskyblue2", "brown3", 
            "magenta1", "palegreen3", "goldenrod3", "dodgerblue3", "firebrick1")


# load and prepare data ---------------------------------------------
library(palmerpenguins)
?palmerpenguins
?penguins

data_full <- as.data.frame( na.omit(penguins) )
data <- scale(data_full[,3:6])

pairs(data, pch = 19, gap = 0)
#
# plot coloring by penguin species
pairs(data, col = colors[data_full$species], pch = 19, gap = 0)


# set algorithm -----------------------------------------------------
K <- 2   # number of clusters - change this number to experiment

V <- ncol(data)
N <- nrow(data)
rg <- apply(data, 2, range)

# convergence and iterations
tol <- 1e-05
converged <- TRUE
iter <- 0
w0 <- sqrt( .Machine$double.xmax )
w_vec <- c()

# to show subsequent iterations in the same panel
par(mfrow = c(2,2))

# initialization ----------------------------------------------------
mu <- t( replicate( K, runif(V, rg[1,], rg[2,]) ) )
plot(data[,1:2], pch = 19, col = adjustcolor("black", 0.5),
     main = "Initialization")
points(mu[,1:2], col = "black", pch = 22, cex = 2, lwd = 2, bg = colors)


# k-means algorithm -------------------------------------------------

while ( converged ) {
  iter <- iter + 1
  
  # allocation step -----------------------
  d <- matrix(NA, N, K)
  for ( k in 1:K ) {
    d[,k] <- rowSums( sweep(data, 2, mu[k,], "-")^2 )
  }
  z <- apply(d, 1, which.min)
  #
  plot( data[,1:2], pch = 19, col = adjustcolor(colors[z], 0.7), 
        main = paste("Iteration", iter, "- Allocation") )
  points(mu[,1:2], col = "black", pch = 22, cex = 2, lwd = 2, bg = colors)
  
  # update step --------------------------
  m0 <- mu
  for ( k in 1:K ) {
    mu[k,] <- if ( !is.null(nrow(data[z == k,])) ) {
       colMeans( data[z == k,] )
    } else data[z == k,]
  }
  #
  plot(data[,1:2], pch = 19, col = adjustcolor(colors[z], 0.7), 
       main = paste("Iteration", iter, "- Update") )
  points(m0[,1:2], col = adjustcolor("black", 0.3), 
         pch = 22, cex = 2, lwd = 2, bg = adjustcolor(colors, 0.3))
  points(mu[,1:2], col = "black", pch = 24, cex = 2, lwd = 2, bg = colors)
  
  # check convergence --------------------
  w <- rep(NA, K)
  for ( k in 1:K ) {
    # within group sum of squares -- ojective function
    w[k] <- if ( !is.null(nrow(data[z == k,])) ) {
       sum( sweep(data[z == k,], 2, mu[k,], "-")^2 )
    } else sum(data[z == k,] - mu[k,])^2  # equal to 0
  }
  w <- sum(w)
  w_vec <- c(w_vec, w)  # growing a vector is really bad coding practice...
  ratio <- (w0 - w)/w
  w0 <- w
  converged <- !( ratio < tol )
}

# results
mu
z
w
plot(w_vec, type = "b", pch = 19)


# plot and color points according to clusters
pairs(data, col = colors[z], pch = 19, gap = 0)


# compare clustering with species
table(z, data_full$species)

