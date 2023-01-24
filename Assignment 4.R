#Exercise 1
getwd()
setwd("/Users/guxingrui/Documents/KCL/大二/Statistical modeling")
#a)
darts <- read.csv("Darts.csv")
attach(darts)
head(darts)
plot(darts)
View(darts)
name <- factor(Name)
model1<- lm(log(Width)~Length+name+I(Thickness^0.1))
model2 <- lm(Width~Length*name*Thickness)
summary(model2)
summary(model1)
step(model1)
model_ini <- lm(Width~Length+Thickness+name)


anova(model_ini,model1)
summary(model1)
summary(model_ini)
library(MASS)
b=boxcox(model2)#Construct the boxcox to find the fittest λ
I = which(b$y==max(b$y))#Get the highest point
Lamda <- b$x[I]
model_fit <- lm(Width^Lamda~Length+Thickness+name+Length*Thickness)
summary(model_fit)
step(model_fit)
par(mfrow=c(2,2))
plot(model_fit)
plot(hatvalues(model_fit))
plot(cooks.distance(model_fit))
plot(rstudent(model_fit))
data_value <- data.frame(Length=50,Thickness=8,name="Travis")
value <- predict(model_fit,data_value)
predict_value<- value^(1/Lamda)
predict_value
library(leaps)
states <- as.data.frame(darts)
leaps<-regsubsets(Width~Length+Thickness+name,data=states,nbest=3)
plot(leaps)


#exercise 2
#首先我们需要先加载路径创建模型
optimal <- read.csv("optimal.csv")
View(optimal)
plot(optimal)
attach(optimal)
model1 <- lm(y~x1+x2+I(x1^2)+I(x2^2))
summary(model1)
step(model1)
model2 <- lm(y~x1+x2)
model3 <- lm(y~I(x1^2)+I(x2^2))
anova(model1,model2)
anova(model1,model3)
model4<- lm(y~x1+x2+I(x1^2)+I(x2^2)+x1*x2)
anova(model1,model4)
par(mfrow=c(2,2))
plot(model4)
#所以综上来说model1最好
par(mfrow=c(2,2))
plot(model1)
s<-summary(model1)$sigma
n<-length(y)
df<-summary(model1)$df[2]
Cp<-(df)*(summary(model1)$sigma^2)/s^2+2*(n-df)-n
Cp
#所以相对来说这个模型是一个十分出色的模型
beta_parameter <- model1$coefficients 
beta_parameter
b1<- beta_parameter[2:3]
b1
help("matrix")
B1<-matrix(0,2,2)
B1[1,1]<- beta_parameter[4]
B1[2,2]<- beta_parameter[5]
B1
b1
x_opt<--0.5*solve(B1)%*%b1
x_opt
eigen(B1)$values
help("Chisquare")
#正数他是一个最小值
#x1=0.7241139
#x2=0.2224978
minvalue <- data.frame(x1=0.7241139,x2=0.2224978)
predict(model1,minvalue,interval = "confidence",level = 0.95)

#         fit        lwr        upr
#1 -0.1546188 -0.1578208 -0.1514169
