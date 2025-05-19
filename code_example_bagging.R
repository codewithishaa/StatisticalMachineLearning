#
# ==== Implements bagging from scratch
#


data("spam", package = "kernlab")
library(rpart)
library(e1071)

# split into training and test
set.seed(8523)
N <- nrow(spam)
train <- sample(1:nrow(spam), N*0.7)
test <- setdiff(1:N, train)


# single classification tree --------------------------------------------------
ct <- rpart(type ~ ., data = spam[train,])

# predictions and estimate labels
y_hat <- predict(ct, newdata = spam[test,], type = "class")
tab <- table(y_hat, spam$type[test])
tab
classAgreement(tab)$diag


# ensemble via bagging -------------------------------------------------------
B <- 100  # number of bootstrap samples (aka size of the ensemble)

# For each replicate, we fit the model on the training bootstrapped sample and 
# evaluate the predictive accuracy on the out of bag samples and the test data.
# Even though the test data is fixed throughout the entire procedure, 
# the training data changes, consequently the
# estimated model varies and predictions on the test data may differ.

# these will store predicted class labels and oob error
out <- oob <- matrix(NA, N, B)
oob_error <- rep(NA, B)

for ( b in 1:B ) {
  
  # set bootstrap training sample
  set <- sample(train, replace = TRUE)
  
  # out of bag observations
  out_of_bag <- setdiff(train, unique(set))
  
  # fit model on bootstrap training sample
  ct <- rpart(type ~ ., data = spam[set,], method = "class",
              parms = list(split = "information"),
              control = rpart.control(minsplit = 1, cp = -1, maxdepth = 30)    # very complex tree with high variance
  )
  # NOTE: change the hyperparameters of the tree to create simpler topology and check impact on performance
  
  # extract predictions for training (in bag) sample
  out[set,b] <- as.character( predict(ct, type = "class") )
  
  # compute predictions for the out of bag sample
  oob[out_of_bag,b] <- as.character( predict(ct, newdata = spam[out_of_bag,], type = "class") )
  votes_oob <- t( apply(oob[,1:b, drop = FALSE], 1, function(x) {
    table(factor(x, levels = c("nonspam", "spam"))) }
  ) )
  probs_oob <- votes_oob/rowSums(votes_oob)
  y_hat_oob <- colnames(probs_oob)[ max.col(probs_oob[out_of_bag,]) ]
  #
  # out of bag error
  tab <- table(y_hat_oob, spam$type[out_of_bag])
  oob_error[b] <- 1 - classAgreement(tab)$diag
  
  # compute predictions for the test data
  out[test,b] <- as.character( predict(ct, newdata = spam[test,], type = "class") )
  
  print(b)
}


# aggregate predictions ------------------------------------------------------

# show estimated labels for 2 training samples over
# the first 10 boostrap replicates
out[train[c(3,477)], 1:10]

# show estimated labels for 2 test samples over
# the first 10 boostrap replicates
out[test[c(3,477)], 1:10]


# We can aggregate the predicted labels by computing the "votes" (i.e. frequencies) 
# which each class received across the procedure for each sample. 
# The counts are then used to estimate the probability of belonging to each class. 
# The class labels of the test data observations are consequently 
# estimated using the maximum a posteriori rule, 
# assigning the class corresponding to the maximum probability.
votes <- t( apply(out, 1, function(x) {
  table(factor(x, levels = c("nonspam", "spam"))) }  
) )

# show first 6 observations
head(votes)

# compute estimated probabilities
probs <- votes/rowSums(votes)
head( probs[train,], 20 )

# out of bag error estimate
plot(oob_error, pch = 19, type = "b")
mean(oob_error)
#
# mean error of last 10 trees
mean(oob_error[B:(B-10)])


# what about predictions on the test data?
#
# assign class label according to the maximum probability (MAP)
y_hat_bag <- colnames(probs)[ max.col(probs[test,]) ]

# check accuracy
tab <- table(y_hat_bag, spam$type[test])
tab
classAgreement(tab)$diag
1 - classAgreement(tab)$diag

