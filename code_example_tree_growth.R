
library(MASS)
library(rpart)
library(partykit)
library(rpart.plot)


# generate some toy data --------------------------------------------
set.seed(12024)
n <- 50
sigma <- matrix(c(1,0.2,0.2,1), 2,2)
x <- rbind( mvrnorm(n/2, c(-3,-3), sigma),
            mvrnorm(n/2, c(0,3), sigma + 0.5),
            mvrnorm(n, c(0.5,-0.5), sigma + diag(c(0.5,0.1))) )
cl <- rep(1:2, each = n)
y <- factor(cl, labels = c("purple", "orange"))

data <- data.frame(y = y, x1 = x[,1], x2 = x[,2])


# for plotting
#
# this is terrible coding practice and should be avoided at any cost (!)
# but it will make easier plotting at each iteration of the example
plot_data <- function(...) {
  par(mar = c(3.5,3.5,0.1,0.1))
  cols <- c("purple3", "darkorange3")
  plot(x, col = adjustcolor(cols[y], 0.6), pch = 19,
       xlab = "", ylab = "", pty = "s")
  mtext(expression(X[1]), side = 1, line = 2.2)
  mtext(expression(X[2]), side = 2, line = 2.2)
}
plot_data()


# how does a tree grow? ---------------------------------------------

# use entropy loss
entropy <- function(p) -sum( p*ifelse(p > 0, log(p), 0) ) 


# select split on the x1 dimension
thres <- -3
plot_data()
abline(v = thres, lwd = 2, lty = 3, col = "grey30")
left <- x[,1] < thres
right <- x[,1] >= thres
p_left <- prop.table( table(y[left]) )
p_right <- prop.table( table(y[right]) )
rbind(p_left, p_right)
entropy( p_left ) + entropy( p_right )


thres <- 0
plot_data()
abline(v = thres, lwd = 2, lty = 3, col = "grey30")
left <- x[,1] < thres
right <- x[,1] >= thres
p_left <- prop.table( table(y[left]) )
p_right <- prop.table( table(y[right]) )
rbind(p_left, p_right)
entropy( p_left ) + entropy( p_right )


thres <- -1.4
plot_data()
abline(v = thres, lwd = 2, lty = 3, col = "grey30")
left <- x[,1] < thres
right <- x[,1] >= thres
p_left <- prop.table( table(y[left]) )
p_right <- prop.table( table(y[right]) )
rbind(p_left, p_right)
entropy( p_left ) + entropy( p_right )


# select split on the x2 dimension
thres1 <- -1.4  
# for this split, the entropy on the left is near zero,
# thres1 identifies a terminal (classification) node,
# the algorithm will focus on the right side of thres1

thres <- 0
plot_data()
abline(v = thres1, lwd = 2, lty = 3, col = "grey30")
segments(thres1, thres, x1 = 5, y1 = thres, lwd = 2, lty = 3, col = "grey30")
up <- (x[,1] >= thres1) & (x[,2] >= thres)
down <- (x[,1] >= thres1) & (x[,2] < thres)
p_up <- prop.table( table(y[up]) )
p_down <- prop.table( table(y[down]) )
entropy( p_up ) + entropy( p_down )


thres <- 2
plot_data()
abline(v = thres1, lwd = 2, lty = 3, col = "grey30")
segments(thres1, thres, x1 = 5, y1 = thres, lwd = 2, lty = 3, col = "grey30")
up <- (x[,1] >= thres1) & (x[,2] >= thres)
down <- (x[,1] >= thres1) & (x[,2] < thres)
p_up <- prop.table( table(y[up]) )
p_down <- prop.table( table(y[down]) )
entropy( p_up ) + entropy( p_down )


thres <- -2
plot_data()
abline(v = thres1, lwd = 2, lty = 3, col = "grey30")
segments(thres1, thres, x1 = 5, y1 = thres, lwd = 2, lty = 3, col = "grey30")
up <- (x[,1] >= thres1) & (x[,2] >= thres)
down <- (x[,1] >= thres1) & (x[,2] < thres)
p_up <- prop.table( table(y[up]) )
p_down <- prop.table( table(y[down]) )
entropy( p_up ) + entropy( p_down )


thres <- 1.22
plot_data()
abline(v = thres1, lwd = 2, lty = 3, col = "grey30")
segments(thres1, thres, x1 = 5, y1 = thres, lwd = 2, lty = 3, col = "grey30")
up <- (x[,1] >= thres1) & (x[,2] >= thres)
down <- (x[,1] >= thres1) & (x[,2] < thres)
p_up <- prop.table( table(y[up]) )
p_down <- prop.table( table(y[down]) )
entropy( p_up ) + entropy( p_down )


# test splits over range of x1 --------------------------------------
rg <- range(data$x1)
n_splits <- 100
grid <- seq(rg[1], rg[2], length = n_splits + 2)[-c(1,n_splits + 2)]

ent <- rep(NA, n_splits)
for ( j in 1:n_splits ) {
  
  thres <- grid[j]
  # plot_data()
  # abline(v = thres, lwd = 2, lty = 3, col = "grey30")
  left <- x[,1] < thres
  right <- x[,1] >= thres
  
  p_left <- prop.table( table(y[left]) )
  p_right <- prop.table( table(y[right]) )
  ent[j] <- entropy( p_left ) + entropy( p_right )
}

plot(grid, ent, type = "l")
grid[ which.min(ent) ]

plot_data()
abline(v = grid[ which.min(ent) ], lwd = 2, lty = 3, col = "grey30")
# is this a reasonable split??


# allow only splits that fulfill some constraint
# for example, minimum number of observations in a region is minsplit
minsplit <- 20
ent <- rep(NA, n_splits)
for ( j in 1:n_splits ) {
  
  thres <- grid[j]
  left <- x[,1] < thres
  right <- x[,1] >= thres
  
  # skip iteration if number of observations in a region is insufficient
  if ( sum(left) < minsplit | sum(right) < minsplit ) next
  
  p_left <- prop.table( table(y[left]) )
  p_right <- prop.table( table(y[right]) )
  ent[j] <- entropy( p_left ) + entropy( p_right )
}

# now entropy is computed only on valid splits
ent
plot(grid, ent, type = "l")
grid[ which.min(ent) ]

plot_data()
abline(v = grid[ which.min(ent) ], lwd = 2, lty = 3, col = "grey30")


# fit and plot a classification tree --------------------------------
library(rpart)
?rpart
?rpart.control

fit <- rpart(y ~ ., data = data,
             method = "class",
             parms = list(split = "information"))
fit

# note that the leaf probabilities are given
(28-1)/28
(1)/28
#
(25-3)/25
3/25
#
1/47
(47-1)/47

# plotting
plot(as.party(fit))

# nicer visualization
rpart.plot(fit)


# classification probabilities 
fit
predict(fit)
unique( predict(fit) )
#
# entropy of the tree
probs <- unique( predict(fit) )
ents <- apply(probs, 1, entropy)
ents
sum(ents)

