fatmen = read.csv("bodyfatmen.csv")
model = lm(density ~ .-density, data= fatmen) 
summary(model)

plot(model)

ols_step_all_possible(model)

best_model = ols_step_best_subset(model)
