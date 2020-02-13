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
men_train = fatmen[train_ind,]
men_test = fatmen[-train_ind,]


############ Computing and analyzing full model ############ 
############################################################ 
model.full = lm(density ~ .-density, data = men_train) 
summary(model.full)
plot(model.full)
vif(model.full)

## Influential points
influ <- influence(model.full)

hii = influ$hat
p = 13
n = 198
large.hat = which(hii > 2*p/n) # Shows whichs points are influential 

#Cook's distance
ols_plot_cooksd_chart(model.full)

#DFFITS
ols_plot_dffits(model.full)

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

### Find that best are 2, 4 and 9

reg.best = regsubsets(density~., data = men_train, nvmax = 13)
coef(reg.best, 2)
coef(reg.best, 4)
coef(reg.best, 9)

#########  Computing reduced models ######### 
#############################################

model.2 = lm(density ~ weight + abdomen, data = men_train) 
model.4 = lm(density ~ weight + abdomen + forearm + wrist, data = men_train) 
model.9 = lm(density ~ age + weight + neck + abdomen + hip + thigh + ankle + forearm + wrist, data = men_train) 

summary(model.2)
summary(model.4)
summary(model.9)

vif(model.2)
vif(model.4)
vif(model.9)



