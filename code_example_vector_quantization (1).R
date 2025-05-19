
# set data ----------------------------------------------------------
library(palmerpenguins)
data_full <- as.data.frame( na.omit(penguins) )
x <- scale(data_full[,3:6])

# use same ranges for visualization purposes
r <- apply(x, 2, range)
rg <- c(min(r), max(r))

pairs(x, pch = 19, gap = 0,
      xlim = rg, ylim = rg)


# vector quantization by k-means ------------------------------------

# k-means
k <- 20
km <- kmeans(x, k, nstart = 50)
M <- km$centers
Z <- mclust::unmap(km$cluster)
x_hat <- Z %*% M

# x_hat is a discretized version of x
head(x)
head(x_hat)

# identical (up to permutation of rows)
unique(x_hat)
M

# plot quants
pairs(unique(x_hat), gap = 0, pch = 19, col = "darkorchid3", cex = 1.5,
      xlim = rg, ylim = rg)

# plot data and quants
n <- nrow(x)
temp <- rbind(x, unique(x_hat))
cols <- c( rep(adjustcolor("gray40", 0.2), n), rep("darkorchid3", k) )
cx <- c( rep(1, n), rep(1.5, k) )
#
pairs(temp, gap = 0, pch = 19, col = cols, cex = cx,
      xlim = rg, ylim = rg)


# compression -- is it worth it?
( nrow(x)*ncol(x) ) / ( nrow(x) + ncol(x) )
#
prod( dim(Z) ) + prod( dim(M) )
prod( dim(x) )
#
# Z is sparse!
z_class <- max.col(Z)
length( z_class ) + prod( dim(M) )
prod( dim(x) )

# storage
object.size(x)
object.size(z_class) + object.size(M)  # much smaller
