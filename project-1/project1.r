fatmen = read.csv("bodyfatmen.csv")

SI <- function(men){
  # Function to convert the non-SI units to SI 
  men$weight <- c(1/2.2*men$weight)
  men$height <- c(2.54*men$height)
  return(men)
}
SI_men = SI(fatmen)

#Removing outliers
fatmen <- fatmen[-c(39),]


# Computing a full model
model = lm(density ~ .-density, data= fatmen) 
summary(model)
plot(model)

#ols_step_all_possible(model)
#best_model = ols_step_best_subset(model)

model_3 = lm(density ~ weight + abdomen + wrist, data = fatmen)
summary(model_3)

model_4 = lm(density ~ weight + abdomen + biceps + wrist, data = fatmen)
summary(model_4)

model_5 = lm(density ~ age + weight + abdomen + thigh + wrist, data = fatmen)
summary(model_5)

model_6 = lm(density ~ age + weight + abdomen + thigh + forearm + wrist, data = fatmen)
summary(model_6)
  



