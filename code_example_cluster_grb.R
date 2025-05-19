
# gamma ray burst data
# see here for more information: https://arxiv.org/abs/1703.07338
load("Lectures/section_7/data_grb.RData")
#
# gamma
str(data_grb)
# type          : type of grb (A, B, C, D, E) defined according to a classification model and validated by astrophysicists
# t50           : the time by which 50% of the flux arrive
# t90           : the time by which 90% of the flux arrive
# flux_256      : peak flux measured in 256 ms bins
# fluence_tot   : total fluence
# H32           : measure of spectral hardness
# H321          : another measure of spectral hardness

# colors for plotting
colors = c("darkorchid3", "olivedrab", "darkorange2", "deepskyblue2", "brown3", 
            "magenta1", "palegreen3", "goldenrod3", "dodgerblue3", "firebrick1")

data = data_grb[,-1]
type = data_grb[,1]

# plot 
pairs(data, gap = 0, col = adjustcolor("black", 0.5), pch = 19)
#
# color by type
pairs(data, gap = 0, col = adjustcolor(colors[type], 0.5), pch = 19)


# kmeans ------------------------------------------------------------
km = kmeans(data, centers = 3, nstart = 200)
pairs(data, gap = 0, pch = 19, 
      col = adjustcolor(colors[km$cluster], 0.5))

# relation between cluster and type of grb
table(type, km$cluster)


# elbow and CH index ------------------------------------------------
K = 10                
wss = bss = rep(NA, K)

for ( k in 1:K ) {
  # run kmeans for each value of k
  fit = kmeans(data, centers = k, nstart = 100) 
  wss[k] = fit$tot.withinss     # store total within sum of squares
  bss[k] = fit$betweenss 
}

# compute calinski-harabasz index
N = nrow(data)
ch = ( bss/(1:K - 1) ) / ( wss/(N - 1:K) )
ch[1] = NA        # the value of CH index for K = 1 is not defined!

par( mfrow = c(1,2) )   
plot(1:K, wss, type = "b", ylab = "WSS", xlab = "K", main = "WSS")
plot(1:K, ch, type = "b", ylab = "CH", xlab = "K", main = "CH")
par( mfrow = c(1,1) )

sel_K = which.max(ch)
km = kmeans(data, centers = sel_K, nstart = 200)
pairs(data, gap = 0, pch = 19, 
      col = adjustcolor(colors[km$cluster], 0.5))

# relation between cluster and type of grb
table(type, km$cluster)


# elbow and CH index - second round! --------------------------------

# this time we standardize the data
data = scale(data)

K = 10                
wss = bss = rep(NA, K)

for ( k in 1:K ) {
  # run kmeans for each value of k
  fit = kmeans(data, centers = k, nstart = 100) 
  wss[k] = fit$tot.withinss     # store total within sum of squares
  bss[k] = fit$betweenss 
}

# compute calinski-harabasz index
N = nrow(data)
ch = ( bss/(1:K - 1) ) / ( wss/(N - 1:K) )
ch[1] = NA        # the value of CH index for K = 1 is not defined!

par( mfrow = c(1,2) )   
plot(1:K, wss, type = "b", ylab = "WSS", xlab = "K", main = "WSS")
plot(1:K, ch, type = "b", ylab = "CH", xlab = "K", main = "CH")
par( mfrow = c(1,1) )

# fit km for selected K
sel_K = which.max(ch)
km = kmeans(data, centers = sel_K, nstart = 200)
pairs(data, gap = 0, pch = 19, 
      col = adjustcolor(colors[km$cluster], 0.5))

# relation between cluster and type of grb
table(type, km$cluster)


# silhouette --------------------------------------------------------
# NOTE: now 'data' are standardized!

library(cluster)
?silhouette

d = dist(data, method = "euclidean")^2

K = 10
ave_sil = rep(NA, K)
sil = vector("list", K)
for ( k in 2:K ) {
  km = kmeans(data, k, nstart = 100)
  sil[[k]] = silhouette(km$cluster, d)
  ave_sil[k] = mean(sil[[k]][,"sil_width"])
}
ave_sil[1] = NA

plot(1:K, ave_sil, type = "b", ylab = "CH", xlab = "K", main = "Avg silhouette")

# plot silhouette
plot(sil[[2]])
plot(sil[[3]])
plot(sil[[4]])


# fit km for selected K
sel_K = which.max(ave_sil)
km = kmeans(data, centers = sel_K, nstart = 200)
pairs(data, gap = 0, pch = 19, 
      col = adjustcolor(colors[km$cluster], 0.5))

# relation between cluster and type of grb
table(type, km$cluster)


# gap statistic -----------------------------------------------------
?clusGap

# original space ...mmmmh
gap_stat = clusGap(data, kmeans, K.max = 10, nstart = 20, spaceH0 = "original")
gap_stat
plot(gap_stat)

# what about different space?
gap_stat = clusGap(data, kmeans, K.max = 10, nstart = 20, spaceH0 = "scaledPCA")
gap_stat
plot(gap_stat)

# alternative (and better looking) plot
library(plotrix)
gs = gap_stat$Tab[,3]
loc_max = min( which(diff(gs) < 0) )
plotCI(1:10, gs, li = gs - gap_stat$Tab[,4], ui = gs + gap_stat$Tab[,4], pch = 19,
       xlab = "", ylab = "", xaxt = "n")
lines(gs)
abline(h = gs[loc_max] - gap_stat$Tab[,4][loc_max], col = "purple2", lty = 2, lwd = 2)
points(loc_max, gs[loc_max], col = "green4", pch = 19, cex = 1.2)
mtext("Number of clusters K", side = 1, line = 2.2)
mtext("Gap", side = 2, line = 2.2)
axis(1, 1:10)
grid()


# fit km for selected K
sel_K = 4
km = kmeans(data, centers = sel_K, nstart = 200)
pairs(data, gap = 0, pch = 19, 
      col = adjustcolor(colors[km$cluster], 0.5))

# relation between cluster and type of grb
table(type, km$cluster)


# compare clustering ------------------------------------------------

library(e1071)
?classAgreement

# how many clusters then?
km3 = kmeans(data, centers = 3, nstart = 200)
km4 = kmeans(data, centers = 4, nstart = 200)
km5 = kmeans(data, centers = 5, nstart = 200)

tab3 = table(type, km3$cluster)
tab3
classAgreement(tab3)$crand


tab4 = table(type, km4$cluster)
tab4
classAgreement(tab4)$crand


tab5 = table(type, km5$cluster)
tab5
classAgreement(tab5)$crand




