# 
# ====================== Comparing classifiers using training, validation and test sets
#


# deepsolar project -------------------------------------------------
# https://web.stanford.edu/group/deepsolar/ds
load("data_deepsolar_small.RData")
str(deepsolar)


# load and check some aspects of the data ---------------------------

# balance
table(deepsolar$coverage)/nrow(deepsolar)

# better standardize
plot( apply(deepsolar[,-1], 2, sd), pch = 19 )
apply(deepsolar[,-1], 2, range)
#
# deepsolar[,-1] <- scale(deepsolar[,-1])    # this will introduce some information leakage
# see lab for a discussion


# hold-out cross validation -----------------------------------------
#
# compare:
# lr1 - only daily_solar_radiation as input
# lr2 - all variables in input
# lr3 - 2nd order interactions

n <- nrow(deepsolar)

# function to compute accuracy
class_acc <- function(y, yhat) {
  tab <- table(y, yhat)
  return( sum(diag(tab))/sum(tab) )
}

# function to return predictions
class_estimate <- function(model, newdata, tau = 0.5) {
  yhat <- ifelse( predict(model, newdata = newdata, type = "response") > tau, 1, 0 )
  return(yhat)
}


R <- 20     # number of replications
out <- matrix(NA, R, 3*3)
colnames(out) <- c(paste0("lr", 1:3, "_train"),
                   paste0("lr", 1:3, "_val"),
                   paste0("lr", 1:3, "_test"))

for ( r in 1:R ) {
  
  # split the data
  train <- sample(1:n, size = 0.60*n)                     # 60% of data points are used as training data 
  val <- sample( setdiff(1:n, train), size = 0.20*n )     # 20% of data points are used as validation data
  test <- setdiff(1:n, union(train, val))                 # 20% of data points are used as test data
  
  # prevent information leakage --- see lab material
  deepsolar_train_val_sc <- scale(deepsolar[c(train, val), -1])
  deepsolar[c(train, val), -1] <- deepsolar_train_val_sc
  deepsolar[test,-1] <-  scale(deepsolar[test,-1], center = attr(deepsolar_train_val_sc, "scaled:center"), 
                               scale = attr(deepsolar_train_val_sc, "scaled:scale"))
  # ALTERNATIVE (suboptimal)
  # deepsolar[c(train, val), -1] <- scale(deepsolar[c(train, val), -1])
  # deepsolar[test,-1] <- scale(deepsolar[test,-1])
  
  
  # training phase --------------------------------
  lr1 <- glm(coverage ~ daily_solar_radiation, 
             data = deepsolar[train,], 
             family = binomial)
  #
  lr2 <- glm(coverage ~ ., 
             data = deepsolar[train,], 
             family = binomial)
  #
  lr3 <- glm(coverage ~ .^2, 
             data = deepsolar[train,], 
             family = binomial)

  
  # classify the training data observations
  y_train_1 <- class_estimate(lr1, newdata = deepsolar[train,])
  out[r,1] <- class_acc(deepsolar$coverage[train], y_train_1)
  #
  y_train_2 <- class_estimate(lr2, newdata = deepsolar[train,])
  out[r,2] <- class_acc(deepsolar$coverage[train], y_train_2)
  #
  y_train_3 <- class_estimate(lr3, newdata = deepsolar[train,])
  out[r,3] <- class_acc(deepsolar$coverage[train], y_train_3)
  
  
  # validation ------------------------------------
  y_val_1 <- class_estimate(lr1, newdata = deepsolar[val,])
  out[r,4] <- class_acc(deepsolar$coverage[val], y_val_1)
  #
  y_val_2 <- class_estimate(lr2, newdata = deepsolar[val,])
  out[r,5] <- class_acc(deepsolar$coverage[val], y_val_2)
  #
  y_val_3 <- class_estimate(lr3, newdata = deepsolar[val,])
  out[r,6] <- class_acc(deepsolar$coverage[val], y_val_3)
  
  
  # test ------------------------------------------
  y_test_1 <- class_estimate(lr1, newdata = deepsolar[test,])
  out[r,7] <- class_acc(deepsolar$coverage[test], y_test_1)
  #
  y_test_2 <- class_estimate(lr2, newdata = deepsolar[test,])
  out[r,8] <- class_acc(deepsolar$coverage[test], y_test_2)
  #
  y_test_3 <- class_estimate(lr3, newdata = deepsolar[test,])
  out[r,9] <- class_acc(deepsolar$coverage[test], y_test_3)

  print(r)
}


boxplot(out)

# nicer visualization
# 
# tidy up results in a single datframe
acc <- c( out )
cl <- rep(factor(rep(1:3, each = R), labels = c("lr1", "lr2", "lr3")), 3)
set <- factor(rep(1:3, each = R*3), labels = c("train", "val", "test"))
acc <- data.frame(acc = acc, cl = cl, set = set)
#
# colors denote classifiers
cols <- c("purple", "forestgreen", "darkorange2")
#
# 'fancy' boxplot to show all results in a single panel
boxplot(acc ~ cl + set, data = acc,
        border = cols, col = adjustcolor(cols, 0.1), at = c(1:3, 5:7, 9:11),
        xaxt = "n")
mtext(side = 1, line = 1, at = c(2, 6, 10), text = levels(set))
legend("bottomright", legend = levels(cl), bty = "n", fill = cols)

#performance in ornage train is best 
#model can be to complex and to simplify 
