library(leaps)
library(caret)
library(car)
library(olsrr)

SI <- function(men){
  # Function to convert the non-SI units to SI 
  men$weight <- c(1/2.2*men$weight)
  men$height <- c(2.54*men$height)
  return(men)
} 

############ Setting up our data ############ 
############################################# 
fatmen = read.csv("bodyfatmen.csv")

# Converting non-SI units to SI units
fatmen = SI(fatmen)

# Splitting data in one training and one test set
## 80% of the sample size
smp_size <- floor(0.8 * nrow(fatmen))

set.seed(37) # set the seed to make the partition reproducible
train_ind <- sample(seq_len(nrow(fatmen)), size = smp_size)
men_train_full = fatmen[train_ind,]
men_test_full = fatmen[-train_ind,]


############ Computing and analyzing full model ############ 
############################################################ 
model.full = lm(density ~ .-density, data = men_train_full) 
summary(model.full)
plot(model.full)
vif(model.full)

### Influential points and outliers ### 
influ <- influence(model.full)

hii = influ$hat
p = 13
n = 198
large.hat = which(hii > 2*p/n) # Shows whichs points are influential 

#Cook's distance
ols_plot_cooksd_chart(model.full)

#DFFITS
ols_plot_dffits(model.full)

# Remove outliers 
men_train <- men_train_full[-c(42, 21),]

# Computing full model with outliers removed 
no.outlier.model.full = lm(density ~ .-density, data = men_train) 
summary(no.outlier.model.full)
plot(no.outlier.model.full)
vif(no.outlier.model.full)

#########  k-fold cross validation ######### 
############################################
predict.regsubsets <- function(object, newdata, id,...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}


k = 10 # number of folds
repeats = 20
set.seed(37)

repeat.cv.errors = matrix(NA, 13, repeats)


#TODO: for each iteration r, store mean.cv.error in a matrix

for(r in 1:repeats){
  
  folds = sample(1:k, nrow (men_train), replace = TRUE )
  cv.errors = matrix(NA, k, 13, dimnames = list(NULL, paste(1:13)))
  for(j in 1:k){
    best.fit = regsubsets(density~., data = men_train[folds != j,], nvmax = 13)
    for(i in 1:13){
      pred <- predict(best.fit, men_train[folds == j,] , id = i)
      cv.errors[j,i] = mean((men_train$density[folds == j]-pred)^2)
    }
  }
  mean.cv.errors = apply(cv.errors, 2, mean)
  repeat.cv.errors[,r] = mean.cv.errors
}

mean.repeat.cv.errors = apply(repeat.cv.errors, 1, mean)

par(mfrow = c(1 ,1))
plot(mean.repeat.cv.errors, type = 'b', ylab = 'Mean Cross-Validation Error')

### Find that best are 3, 4 and 8 (for our purposes)

reg.best = regsubsets(density~., data = men_train, nvmax = 13)
coef(reg.best, 3)
coef(reg.best, 4)
coef(reg.best, 8)

#########  Computing reduced models ######### 
#############################################

model.3 = lm(density ~ age + abdomen + wrist, data = men_train) 
model.4 = lm(density ~ age + height + abdomen + wrist, data = men_train) 
model.8 = lm(density ~ age + height + neck + abdomen + 
               hip + thigh + forearm + wrist, data = men_train) 

summary(model.3)
summary(model.4)
summary(model.8)

vif(model.3)
vif(model.4)
vif(model.8)

#########  Bootstraping the refined models ######### 
####################################################

# NOT RUN {
m1 <- lm(Fertility ~ ., swiss)
betahat.boot <- Boot(m1, R=99) # 99 bootstrap samples--too small to be useful
summary(betahat.boot)  # default summary
confint(betahat.boot)
hist(betahat.boot)
# }

betahat.boot.3 = Boot(model.3, R = 2000)
summary(betahat.boot.3)
confint(betahat.boot.3)
hist(betahat.boot.3, main = "Model 3")

mse.boot.3 = Boot(model.3, f = get_mse, R = 2000)
# TODO: do bootstrap for r squared
hist(mse.boot.3, main = "Model 3 (MSE)")


betahat.boot.4 = Boot(model.4, R = 2000)
summary(betahat.boot.4)
confint(betahat.boot.4)
hist(betahat.boot.4, main = "Model 4")
