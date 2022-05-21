library(car)

# load data
dt <- read.csv("r_data/blood_pressure.txt", header=T, sep=";")


# inspect data
str(dt)

plot(dt$drug_dose, dt$blood_pressure_change, 
     ylab="change in blood pressue (mm Hg)",
     xlab="drug dose (mg)", pch=19)


# fit linear model
model <- lm(formula=blood_pressure_change ~ drug_dose, data=dt) 
# fits the model blood_pressure_change = beta_0 + beta_1 * drug_dose


# inferences with fitted model
model
abline(model$coefficients)

summary(model)

linearHypothesis(model, hypothesis.matrix=c(0, 1), rhs=0) # from the car package

confint(model)
