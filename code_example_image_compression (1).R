
# load image data ---------------------------------------------------
library(jpeg)
# check also package 'png' to load png images

# try other images!
img <- readJPEG("Lectures/section_8/fig_flowers.jpg")
# img <- readJPEG("image.jpg")
dim(img)
img[1,1,1]
img[1,1,]

# plot
par(mar = rep(0, 4))
plot( as.raster(img) )

# convert array to matrix
# rows = total number of pixels
# columns = RGB channels
x <- apply(img, 3, c)
dim(x)


# image compression and region detection ----------------------------
k <- 10
km <- kmeans(x, k, nstart = 10, iter.max = 20)
M <- km$centers
Z <- mclust::unmap(km$cluster)
x_hat = Z %*% M
img_hat <- array( x_hat, dim(img) )

# constituent 'quant' colors
barplot( rep(1,k), col = rgb(M[,1], M[,2], M[,3]),
         yaxt = "n", xaxt = "n")

# plot compressed image
par(mar = c(0,0,1.5,0))
plot( as.raster(img_hat) )
mtext(paste0("K = ", k), 3, line = 0, cex = 1.7)


# compression level
prod( dim(Z) ) + prod( dim(M) )
prod( dim(x) )
#
# Z is sparse!
z_class <- max.col(Z)
length( z_class ) + prod( dim(M) )
prod( dim(x) )

# ~actual size
format(object.size(img), "Mb")
format(object.size(M), "Mb") 
format(object.size(z_class), "Mb")

