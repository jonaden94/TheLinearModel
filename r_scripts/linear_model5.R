# load data
dt1 <- read.csv("r_data/growth_data.txt", header=T, sep=";")


# recode variables
dt1$treatment <- as.factor(dt1$treatment)
dt1$suntime <- dt1$suntime - mean(dt1$suntime)


# fit linear model
model1 <- lm(formula=height ~ year + year:treatment + year:suntime, data=dt1)


# diagnostics
plot(model1, 3)
plot(model1, 4)


# check how model changes when removing observation 163
dt2 <- dt1[-163,]
model2 <- lm(formula=height ~ year + year:treatment + year:suntime, data=dt2)
summary(model2)


######################
"Example with outlier"
######################

# load data
dt3 <- read.csv("r_data/example_outlier.txt", header=T, sep=";")


# fit linear model
model3 <- lm(height ~ year, data=dt3)


# plot height vs. year
plot(dt3$year, dt3$height)
abline(model3$coefficients, col="red")
legend(1, 175, "estimated", fill="red")


# model diagnostics
plot(model3, 3)
plot(model3, 4)


# correct mistake and fit model again
dt3$height[252] <- -dt3$height[252]
model4 <- lm(height ~ year, data=dt3)

# compare estimates for the model with and without outliers
confint(model3)
confint(model4)
