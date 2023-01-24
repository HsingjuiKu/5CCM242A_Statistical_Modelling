seeds <- read.table("seeds.txt", header=TRUE)
attach(seeds)
head(seeds)

# Let's compute the proportion first:

seeds2 = cbind(seeds, prop = seeds$y/seeds$n) 

plot(seeds2)

# predictors needs to be factor here:
seeds2$extract<-factor(seeds2$extract)

seed1.glm <- glm(prop ~ seed*extract, family=binomial, weights=n, data=seeds2)
summary(seed1.glm)

seed2.glm <- glm(prop ~ extract, family=binomial, weights=n, data=seeds2)

anova(seed2.glm,seed1.glm)


# Deviance test:偏差实验

D<-seed2.glm$deviance-seed1.glm$deviance
D#D其实就是q值
qchisq(0.95,2)
D>qchisq(0.95,2)

# p-value
1-pchisq(D,2)
help("pchisq")
# Confidence intervals:

beta<-seed1.glm$coefficients
cf<-0

beta_se<-summary(seed1.glm)$coefficients[,2]
beta[cf+1]-beta_se[cf+1]*qnorm(0.975,0,1)
beta[cf+1]+beta_se[cf+1]*qnorm(0.975,0,1)


# You can also use the confint function, but it is doing something different:
confint(seed1.glm)



# Binary (logistic) regression - from the practical

# Data are collected for 40 patients receiving
# a new surgery technique. The variable surv takes the value 1
# if the patient show any  negative side effect in the 30 days after surgery and is 0 otherwise.
# The age (years) of the patient is also recorded. 
# 
# This are binary data, where the response is 0/1, and 
# we need to use a Bernoulli regression, also called LOGISTIC regression (from the name of the link function).
# The question of interest is to see how the probability $p$ of having negative side effects
#  depends on age.

# Import data and visual exploration:

surg <- read.table("surgery.txt",header=T)
attach(surg)
head(surg)
plot(Age, surv)

# Fit a logistic regression/ Bernoulli regression model:

surg1.glm <- glm(surv ~ Age, binomial, data=surg)
summary(surg1.glm)


D<-surg1.glm$null.deviance-surg1.glm$deviance
D>qchisq(0.99,1)


beta<-surg1.glm$coefficients
eta<-beta[1]+beta[2]*Age
points(Age, exp(eta)/(exp(eta)+1),type='l',lwd=2,col=4)