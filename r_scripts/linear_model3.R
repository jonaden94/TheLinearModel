# load data
dt1 <- read.csv("r_data/growth_data.txt", header=T, sep=";")


# recode variables
dt1$treatment <- as.factor(dt1$treatment)
dt1$suntime <- dt1$suntime - mean(dt1$suntime)


# fit linear model
model1 <- lm(formula=height ~ year + year:treatment + year:suntime, data=dt1)


# diagnostics
plot(model1, 1)


########################################################################
"Example where equal variance assumption (homoscedasticity) is violated"
########################################################################

# fit misspecified linear model
model2 <- lm(height ~ year, data=dt1)


# plot height vs. year
plot(dt1$year, dt1$height)
abline(model2$coefficients, col="blue")
legend(1, 200, "estimated", fill="blue")


# model diagnostics
plot(model2, 1)
plot(model2, 3) # standardized residuals take leverage into account


##################################################
"Example where independence of errors is violated"
##################################################

# load data
dt2 <- read.csv("r_data/example_autocorrelation.txt", header=T, sep=";")


# fit linear model
model3 <- lm(height ~ time, data=dt2)


# plot height vs. year
plot(dt2$time, dt2$height)
abline(model3$coefficients, col="blue")
legend(0, 250, "estimated", fill="blue")


# model diagnostics
plot(model3, 1)


################################################
"Example where linearity assumption is violated"
################################################

# load data
dt3 <- read.csv("r_data/example_non_linear.txt", header=T, sep=";")


# fit linear model
model4 <- lm(height ~ year, data=dt3)


# plot height vs. year
plot(dt3$year, dt3$height)
abline(model4$coefficients, col="blue")
legend(1, 70, "estimated", fill="blue")


# model diagnostics
plot(model4, 1)
