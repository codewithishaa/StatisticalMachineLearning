# 
# ====================== Using training set for fitting and test set for testing
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


# hold-out sample ---------------------------------------------------

# split the data:
# 70% of data points are used as training data
# 30% of data points are used as test data
set.seed(20250)
n <- nrow(deepsolar)
train <- sample(1:n, size = 0.70*n)       # get indices of units in training and test data
test <- setdiff(1:n, train)

# prevent information leakage --- see lab material
deepsolar_train_sc <- scale(deepsolar[train,-1])
deepsolar[train,-1] <- deepsolar_train_sc
deepsolar[test,-1] <-  scale(deepsolar[test,-1], center = attr(deepsolar_train_sc, "scaled:center"), 
                             scale = attr(deepsolar_train_sc, "scaled:scale"))
#
# ALTERNATIVE APPROACH:
# it minimizes data leakage but not optimal
# deepsolar[train,-1] <- scale(deepsolar[train,-1])
# deepsolar[test,-1] <- scale(deepsolar[test,-1])

# different means!
colMeans(deepsolar[train,-1])
colMeans(deepsolar[test,-1])


# fit a classifier on the training data
lr <- glm(coverage ~ ., 
           data = deepsolar[train,], 
           family = binomial)

# classify the training data observations
probs_train <- predict(lr, type = "response")
y_train <- ifelse(probs_train > 0.5, 1, 0)
#
tab_train <- table(truth = deepsolar$coverage[train], y_train)
tab_train
sum(diag(tab_train))/sum(tab_train)


# classify the test data observations
probs_test <- predict(lr, newdata = deepsolar[test,], type = "response")
y_test <- ifelse(probs_test > 0.5, 1, 0)
#
tab_test <- table(truth = deepsolar$coverage[test], y_test)
tab_test
sum(diag(tab_test))/sum(tab_test)



# multiple replications ---------------------------------------------

R <- 100
out <- matrix(NA, R, 2)
colnames(out) <- c("train", "test")

for ( r in 1:R ) {
  
  # split the data
  train <- sample(1:n, size = 0.70*n)   # 70% of data points are used as training data
  test <- setdiff(1:n, train)           # 20% of data points are used as test data
  
  # prevent information leakage --- see lab material
  deepsolar_train_sc <- scale(deepsolar[train,-1])
  deepsolar[train,-1] <- deepsolar_train_sc
  deepsolar[test,-1] <-  scale(deepsolar[test,-1], center = attr(deepsolar_train_sc, "scaled:center"), 
                               scale = attr(deepsolar_train_sc, "scaled:scale"))
  
  # fit classifier on the training data
  lr <- glm(coverage ~ ., 
            data = deepsolar[train,], 
            family = binomial)
  
  # classify the training data observations
  probs_train <- predict(lr, type = "response")
  y_train <- ifelse(probs_train > 0.5, 1, 0)
  tab_train <- table(truth = deepsolar$coverage[train], y_train)
  out[r, "train"] <- sum(diag(tab_train))/sum(tab_train)
  
  # classify the test data observations
  probs_test <- predict(lr, newdata = deepsolar[test,], type = "response")
  y_test <- ifelse(probs_test > 0.5, 1, 0)
  tab_test <- table(truth = deepsolar$coverage[test], y_test)
  out[r, "test"] <- sum(diag(tab_test))/sum(tab_test)
  
}

apply(out, 2, summary)
boxplot(out)
apply(out, 2, sd)

