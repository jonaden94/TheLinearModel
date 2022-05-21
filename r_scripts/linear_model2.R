library(car)

# load data
dt <- read.csv("r_data/growth_data.txt", header=T, sep=";")


# inspect data and recode variables
str(dt)

dt$treatment <- as.factor(dt$treatment)
dt$suntime <- dt$suntime - mean(dt$suntime)

summary(dt)


# fit linear model
model <- lm(formula=height ~ year + year:treatment + year:suntime, data=dt)


# inferences with fitted model
summary(model)
confint(model)
linearHypothesis(model, hypothesis.matrix=c(0, 0, 1, -1, 0), rhs=0)

