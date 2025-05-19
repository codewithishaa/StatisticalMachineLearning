# 
# ====================== Using same data for fitting and testing the model
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
deepsolar[,-1] <- scale(deepsolar[,-1])



# fit ---------------------------------------------------------------
lr1 <- glm(coverage ~ daily_solar_radiation, 
           data = deepsolar, 
           family = binomial)

# classify the observations
probs <- predict(lr1, type = "response")
y_hat <- ifelse(probs > 0.5, 1, 0)

# Look at table (rows = 'truth', cols = 'prediction')
tab <- table(truth = deepsolar$coverage, prediction = y_hat)
tab

# compute classification accuracy
acc1 <- sum( diag(tab) ) / sum(tab)
acc1


# a more complex model ----------------------------------------------

# is a more complex model better? 
lr2 <- glm(coverage ~ daily_solar_radiation + average_household_income + 
             population_density + heating_fuel_electricity_rate, 
           data = deepsolar, 
           family = binomial)

# classify the observations
probs <- predict(lr2, type = "response")
y_hat <- ifelse(probs > 0.5, 1, 0)

# Look at table (rows = 'truth', cols = 'prediction')
tab <- table(truth = deepsolar$coverage, prediction = y_hat)
tab

# compute classification accuracy
acc2 <- sum( diag(tab) ) / sum(tab)
acc2


# more complex! -----------------------------------------------------

# even better? 
lr3 <- glm(coverage ~ ., 
           data = deepsolar, 
           family = binomial)

# classify the observations
probs <- predict(lr3, type = "response")
y_hat <- ifelse(probs > 0.5, 1, 0)

# Look at table (rows = 'truth', cols = 'prediction')
tab <- table(truth = deepsolar$coverage, prediction = y_hat)
tab

# compute classification accuracy
acc3 <- sum( diag(tab) ) / sum(tab)
acc3


# more! -------------------------------------------------------------

# now talking
lr4 <- glm(coverage ~ .^2, 
           data = deepsolar, 
           family = binomial)

# classify the observations
probs <- predict(lr4, type = "response")
y_hat <- ifelse(probs > 0.5, 1, 0)

# Look at table (rows = 'truth', cols = 'prediction')
tab <- table(truth = deepsolar$coverage, prediction = y_hat)
tab

# compute classification accuracy
acc4 <- sum( diag(tab) ) / sum(tab)
acc4


# -------------------------------------------------------------------
# RUN THIS AT YOUR OWN RISK - THIS WILL COULD TAKE A LOOOOONG TIME
lr5 <- glm(coverage ~ (daily_solar_radiation + earth_temperature + air_temperature +
                         average_household_income + employ_rate + population_density + housing_unit_count +
                         heating_fuel_gas_rate + heating_fuel_electricity_rate + heating_fuel_oil_rate +
                         land_area)^3, 
           data = deepsolar, 
           family = binomial)
#
probs <- predict(lr5, type = "response")
y_hat <- ifelse(probs > 0.5, 1, 0)
#
tab <- table(truth = deepsolar$coverage, prediction = y_hat)
tab
#
acc5 <- sum( diag(tab) ) / sum(tab)
acc5


# overview
mods <- list(lr1 = lr1, lr2 = lr2, lr3 = lr3, lr4 = lr4, lr5 = lr5)
data.frame( model = names(mods), accuracy = c(acc1, acc2, acc3, acc4, acc5),
            complexity = sapply(mods, function(x) length(coefficients(x))) )

