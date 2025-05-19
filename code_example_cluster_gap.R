
# set data ----------------------------------------------------------
# colors for plots
colors <- c("darkorchid3", "olivedrab", "darkorange2", "deepskyblue2", "brown3", 
            "magenta1", "palegreen3", "goldenrod3", "dodgerblue3", "firebrick1")

library(palmerpenguins)
data_full <- as.data.frame( na.omit(penguins) )
data <- scale(data_full[,3:6])

pairs(data, pch = 19, gap = 0)
# plot coloring by penguin species
pairs(data, col = colors[data_full$species], pch = 19, gap = 0)


# intuition ---------------------------------------------------------
par(mfrow = c(1, 2))
plot(data[,1:2], pch = 19, col = adjustcolor("black", 0.8),
     main = "Original data")

N <- nrow(data)
V <- ncol(data)
rg <- apply(data, 2, range)
rg

data_sim <- matrix(NA, N, V)
for ( j in 1:V ) data_sim[,j] <- runif(N, rg[1,j], rg[2,j])
plot(data_sim[,1:2], pch = 19, col = adjustcolor("black", 0.8),
     main = "Simulated data")

# number of clusters
K <- 3

# fit kmeans and log wss
km <- kmeans(data, K, nstart = 20)
w <- round( log(km$tot.withinss), 2 )
pairs(data, pch = 19, col = adjustcolor(colors[km$cluster], 0.8),
      main = paste0("Original data -- log W = ", w) )
#
km_sim <- kmeans(data_sim, K, nstart = 20)
w_sim <- round( log(km_sim$tot.withinss), 2 )
pairs(data_sim, pch = 19, col = adjustcolor(colors[km_sim$cluster], 0.8),
      main = paste0("Simulated data -- log W = ", w_sim) )



# simulate and compute gap ------------------------------------------
K <- 3
km <- kmeans(data, K, nstart = 20)
w <- round( log(km$tot.withinss), 2 )

B <- 100
w_sim <- rep(NA, B)
for ( b in 1:B ) {
  data_sim <- matrix(NA, N, V)
  for ( j in 1:V ) data_sim[,j] <- runif(N, rg[1,j], rg[2,j])
  km_sim <- kmeans(data_sim, K, nstart = 20)
  w_sim[b] <- round( log(km_sim$tot.withinss), 2 )
}

mean(w_sim)
sd(w_sim)

mean(w_sim) - w



# for different values of K -----------------------------------------
K_vec <- 1:10

B <- 100
w <- rep(NA, length(K_vec))
w_sim <- matrix(NA, B, length(K_vec))

# implement k-means on original data
for ( k in K_vec ) {
  km <- kmeans(data, K_vec[k], nstart = 20)
  w[k] <- round( log(km$tot.withinss * 0.5), 2 )
  # NOTE: in the code implementation of 'clusGap', the W is divided by 2
}

# implement assessment on uniform data
for ( b in 1:B ) {
  
  # generate uniform synthetic data over the range of the original data
  data_sim <- matrix(NA, N, V)
  for ( j in 1:V ) data_sim[,j] <- runif(N, rg[1,j], rg[2,j])
  
  # implement k-means on synthetic data
  for ( k in K_vec ) {
    km_sim <- kmeans(data_sim, K_vec[k], nstart = 20)
    w_sim[b,k] <- round( log(km_sim$tot.withinss * 0.5), 2 )
  }
}

plot(exp(w))   # do you see any elbow there?

w
w_sim

w
colMeans(w_sim)

# gap
gap <- colMeans(w_sim) - w
gap 

# standard error
se <- sqrt(1 + 1/B)*apply(w_sim, 2, sd)


# R implementation
library(cluster)
gs <- clusGap(data, 
              FUNcluster = kmeans, 
              K.max = 10, 
              d.power = 1,    # change this to 2
              spaceH0 = "original")

gs
