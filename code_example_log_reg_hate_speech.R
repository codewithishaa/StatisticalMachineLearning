
load("data_hate_speech_2.RData")
summary(data)
head(data)

data = data[,-1]
data$class = factor(data$class, levels = c("no_hate", "hate"))
table(data$class)

# logistic regression -----------------------------------------------
fit <- glm(class ~ ., data = data, family = binomial)
summary(fit)

head( predict(fit) )                       # linear predictor
head( predict(fit, type = "response") )    # probabilities


# (naive) model assessment ------------------------------------------
tau <- 0.5
p <- fitted(fit)
hist(p, breaks = 100) # is 0.5 a good threshold?
abline(v = tau, col = "purple", lwd = 2)

pred <- ifelse(p > tau, 1, 0)

# cross tabulation between observed and predicted
tab <- table(data$class, pred)
tab
sum(diag(tab))/sum(tab)
#
# is really accuracy appropriate here?
tab[2,2]/sum(tab[2,])   # true positives
tab[1,1]/sum(tab[1,])   # true negatives


# what if the model predict all 0 cases?
tau <- 1
pred <- ifelse(p > tau, 1, 0)
tab <- table(data$class, pred)
tab
sum(diag(tab))/sum(tab)  # still large accuracy!



# different thresholds ----------------------------------------------
tau <- seq(0, 1, by = 0.1)
out <- matrix(NA, 10, 5)
colnames(out) <- c("tau", "acc", "sens", "spec", "prec")
for ( j in 1:10 ) {
  pred <- factor(ifelse(p > tau[j], 1, 0), levels = 0:1)
  
  tab <- table(data$class, pred)
  
  out[j,1] <- tau[j]
  out[j,2] <- sum(diag(tab))/sum(tab)
  out[j,3] <- tab[2,2]/sum(tab[2,])
  out[j,4] <- tab[1,1]/sum(tab[1,])  
  out[j,5] <- tab[2,2]/sum(tab[,2])
}
round(out, 4)



# roc curve ---------------------------------------------------------
library(ROCR)
pred_obj <- prediction(fitted(fit), data$class, label.ordering = c("no_hate", "hate"))
perf <- performance(pred_obj, "tpr", "fpr")
plot(perf)
abline(0,1, col = "purple", lty = 2, lwd = 2)   # add bisect line

# compute the area under the ROC curve
auc <- performance(pred_obj, "auc")
auc@y.values
text( 0.8, 0.2, paste0("AUC-ROC = ", round(auc@y.values[[1]], 4)) )

# optimal threshold
sens <- performance(pred_obj, "sens")
spec <- performance(pred_obj, "spec")
#
tau_roc <- sens@x.values[[1]]
sens_spec <- sens@y.values[[1]] + spec@y.values[[1]]
best_roc <- which.max(sens_spec)
plot(tau_roc, sens_spec, type = "l")
points(tau_roc[best_roc], sens_spec[best_roc], pch = 19, col = adjustcolor("darkorange2", 0.5))
#
tau_roc[best_roc]   # optimal tau

# classification for optimal tau
pred <- ifelse(fitted(fit) > tau_roc[best_roc], 1, 0)
table(data$class, pred)
tab <- table(data$class, pred)
tab

# metrics for optimal tau
sum(diag(tab))/sum(tab)        # accuracy
sens@y.values[[1]][best_roc]   # sensitivity
spec@y.values[[1]][best_roc]   # specificity



# precision/recall  -------------------------------------------------

# produce precision/recall curve
pr <- performance(pred_obj, "prec", "rec")
plot(pr)
abline(h = table(data$class)[2]/nrow(data), col = "purple", lty = 2)

# compute area under the PR curve
aucpr <- performance(pred_obj, "aucpr")
aucpr@y.values
text( 0.2, 0.2, paste0("AUC-PR = ", round(aucpr@y.values[[1]], 4)) )

# f1 score and optimal threshold
prec <- performance(pred_obj, "prec")
perf_f <- performance(pred_obj, "f")
tau_f <- perf_f@x.values[[1]]
f <- perf_f@y.values[[1]]
best_f <- which.max(f)
plot(tau_f, f, type = "l")
points(tau_f[best_f], f[best_f], pch = 19, col = adjustcolor("darkorange2", 0.5))

tau_f[best_f]   # optimal tau according to the F1 score / Pr curve

# classification for optimal tau
pred <- ifelse(fitted(fit) > tau_f[best_f], 1, 0)
table(data$class, pred)

# accuracy and F1 score for optimal tau
acc <- performance(pred_obj, "acc")
acc@y.values[[1]][best_f]
f[best_f]
prec@y.values[[1]][best_f]

# summary
res <- cbind( c(tau_roc[best_roc], acc@y.values[[1]][best_roc], sens@y.values[[1]][best_roc], 
                spec@y.values[[1]][best_roc], prec@y.values[[1]][best_roc], 
                f[best_roc], auc@y.values[[1]]),
              c(tau_f[best_f], acc@y.values[[1]][best_f], sens@y.values[[1]][best_f], 
                spec@y.values[[1]][best_f], prec@y.values[[1]][best_f], 
                f[best_f], aucpr@y.values[[1]]) )
colnames(res) <- c("ROC", "PR")
rownames(res) <- c("tau", "acc", "sens", "spec", "prec", "F1", "AUC")
res


