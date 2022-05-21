# load data
dt1 <- read.csv("r_data/growth_data.txt", header=T, sep=";")


# recode variables
dt1$treatment <- as.factor(dt1$treatment)
dt1$suntime <- dt1$suntime - mean(dt1$suntime)


# fit linear model
model1 <- lm(formula=height ~ year + year:treatment + year:suntime, data=dt1)


# diagnostics
plot(model1, 2)


#############################################################
"Example where assumption of normal distribution is violated"
#############################################################

# load data
dt2 <- read.csv("r_data/example_non_normal.txt", header=T, sep=";")


# fit linear model
model2 <- lm(height ~ year, data=dt2)


# plot height vs. year
plot(dt2$year, dt2$height)


# model diagnostics
plot(model2, 2)



