library(leaps)
library(caret)

SI <- function(men){
  # Function to convert the non-SI units to SI 
  men$weight <- c(1/2.2*men$weight)
  men$height <- c(2.54*men$height)
  return(men)
} 

# Import data 
fatmen = read.csv("bodyfatmen.csv")

# Converting non-SI units to SI units
fatmen = SI(fatmen)

# Splitting data in two datasets
## 80% of the sample size
smp_size <- floor(0.8 * nrow(fatmen))

## set the seed to make the partition reproducible
set.seed(1)
train_ind <- sample(seq_len(nrow(fatmen)), size = smp_size)
men_train = fatmen[train_ind,]
men_test = fatmen[-train_ind,]


# Computing a full model
model = lm(density ~ .-density, data = men_train) 
summary(model)
plot(model)

# Computing best subset models 
regfit.best = regsubsets(density~., data = men_train, nvmax = 13)

#Finding best reduced models
#ols_step_all_possible(model)
best_model = ols_step_best_subset(model)

# # Set up repeated k-fold cross validation   
# train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 20)
# # Train the model 
# step.model <- train(density~. -density, data = men_train,
#                   tuneGrid = data.frame(nvmax = 1:13),
#                   method = "leapForward",

#                   trControl = train.control
# )
# 
# 
# plot(step.model$results$RMSE, pch = 19, type = "b", ylab="RMSE")

## k-fold cross validation: recall

predict.regsubsets <- function(object, newdata, id,...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}


k = 10
set.seed(1)
folds = sample (1: k , nrow ( men_train ) , replace = TRUE )
cv.errors = matrix(NA, k, 13, dimnames = list(NULL,paste (1:13)))



for(j in 1:k){
  best.fit = regsubsets(density~., data = men_train[folds != j,], nvmax = 13)
  for(i in 1:13){
    pred <- predict(best.fit, men_train[folds == j,] , id = i)
    cv.errors[j,i] = mean((men_train$density[folds == j]-pred)^2)
  }
}

mean.cv.errors = apply(cv.errors, 2, mean)
par(mfrow = c(1 ,1))
plot(mean.cv.errors, type = 'b')

reg.best = regsubsets(density~., data = men_train, nvmax = 13)
coef(reg.best, 4)
