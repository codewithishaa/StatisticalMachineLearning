# 
# ====================== Evaluate single classifier using K-fold cross validation
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


# k-fold cross validation -------------------------------------------

K <- 5   # number of folds
# NOTE: setting K = N gives LOO cross validation -- it can be very slow!

n <- nrow(deepsolar)
folds <- rep( 1:K, ceiling(n/K) )
folds <- sample(folds)             # random permute 
folds <- folds[1:n]                # ensure we got n data points


out <- matrix(NA, K, 3)
colnames(out) <- c("fold", "in-sample", "out-sample")

for ( k in 1:K ) {
  
  out[k, "fold"] <- k
  
  # split into folds
	train <- which(folds != k)
	test <- setdiff(1:n, train)
	
	# prevent information leakage --- see lab material
	deepsolar_train_sc <- scale(deepsolar[train,-1])
	deepsolar[train,-1] <- deepsolar_train_sc
	deepsolar[test,-1] <-  scale(deepsolar[test,-1], center = attr(deepsolar_train_sc, "scaled:center"), 
	                             scale = attr(deepsolar_train_sc, "scaled:scale"))
	
	# fit classifier on the training data (the folds except the k-th)
	lr <- glm(coverage ~ ., 
	          data = deepsolar[train,], 
	          family = binomial)
	
	# classify the training data observations
	probs_train <- predict(lr, type = "response")
	y_train <- ifelse(probs_train > 0.5, 1, 0)
	tab_train <- table(truth = deepsolar$coverage[train], y_train)
	out[k, "in-sample"] <- sum(diag(tab_train))/sum(tab_train)
	
	# classify the test data observations, i.e. the k-th fold
	probs_test <- predict(lr, newdata = deepsolar[test,], type = "response")
	y_test <- ifelse(probs_test > 0.5, 1, 0)
	tab_test <- table(truth = deepsolar$coverage[test], y_test)
	out[k, "out-sample"] <- sum(diag(tab_test))/sum(tab_test)
	
	print(k)
}

out
colMeans(out[,-1])
apply(out[,-1], 2, sd)



# multiple replications -------------------------------------------

R <- 10      # number of replications
K <- 5       # number of folds
n <- nrow(deepsolar)

out <- vector("list", R)

for ( r in 1:R ) {
  
  out[[r]] <- matrix(NA, K, 3)
  colnames(out[[r]]) <- c("fold", "in-sample", "out-sample")
  
  folds <- rep( 1:K, ceiling(n/K) )
  folds <- sample(folds)             # random permute 
  folds <- folds[1:n]                # ensure we got n data points
  
  for ( k in 1:K ) {
    
    out[[r]][k, "fold"] <- k
    
    # split into folds
    train <- which(folds != k)
    test <- setdiff(1:n, train)
    
    # prevent information leakage --- see lab material
    deepsolar_train_sc <- scale(deepsolar[train,-1])
    deepsolar[train,-1] <- deepsolar_train_sc
    deepsolar[test,-1] <-  scale(deepsolar[test,-1], center = attr(deepsolar_train_sc, "scaled:center"), 
                                 scale = attr(deepsolar_train_sc, "scaled:scale"))
    
    # fit classifier on the training data (the folds except the k-th)
    lr <- glm(coverage ~ ., 
              data = deepsolar[train,], 
              family = binomial)
    
    # classify the training data observations
    probs_train <- predict(lr, type = "response")
    y_train <- ifelse(probs_train > 0.5, 1, 0)
    tab_train <- table(truth = deepsolar$coverage[train], y_train)
    out[[r]][k, "in-sample"] <- sum(diag(tab_train))/sum(tab_train)
    
    # classify the test data observations, i.e. the k-th fold
    probs_test <- predict(lr, newdata = deepsolar[test,], type = "response")
    y_test <- ifelse(probs_test > 0.5, 1, 0)
    tab_test <- table(truth = deepsolar$coverage[test], y_test)
    out[[r]][k, "out-sample"] <- sum(diag(tab_test))/sum(tab_test)
    
    print( c(r, k) )
  }

}

avg <- t( sapply(out, colMeans)[-1,] )
avg
boxplot(avg)

