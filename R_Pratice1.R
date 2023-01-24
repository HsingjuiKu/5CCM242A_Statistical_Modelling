# Data of amount of weight gained on 3 diets in 2 different countries:
setwd("/Users/Statistical modeling")
source("factorial_data.R")

# explore data
Data
head(Data)
attach(Data)

Diet
Country
plot(Data)

# Main effect:

diet.name<-unique(Diet) # define the levels of the factor，unique函数可以去除重复项
diet.name   #"A" "B" "C"
main.effects<-c(mean(Weight_change[Diet==diet.name[1]]),mean(Weight_change[Diet==diet.name[2]]),mean(Weight_change[Diet==diet.name[3]])) # compute the mean of the response for each level
#计算每一个A,B,C每一个的平均数
#values of the mean effect for supplement
main.effects

# plot of the mean effect for supplement
#运用坐标系展现三种种类分别的平均数
plot(c(-1,0,1),main.effects,xaxt='n',type='b', xlab="Diet type",ylab="Mean weight change", main = "Main effect of diet type")
axis(2,at=seq(0.01,0.15,len=8))
axis(1,at=c(-1,0,1),labels=diet.name)


country.name<-unique(Country) # define the levels of the factor，分类国家
main.effects<-c(mean(Weight_change[Country==country.name[1]]),mean(Weight_change[Country==country.name[2]])) # compute the mean of the response for each level
#values of the mean effect for supplement
main.effects

# plot of the mean effect for the country
plot(c(-1,1),main.effects,xaxt='n',type='b', xlab="Country",ylab="Mean weight change", main = "Main effect of supplement type")
axis(2,at=seq(0.01,0.15,len=8))
axis(1,at=c(-1,1),labels=country.name)
#绘制出USA和UK的分别的图像

# interaction plots:
#交互图表
interaction.plot(x.factor     = Country,
                 trace.factor = Diet,
                 response     = Weight_change,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
)
help("interaction.plot")


# What can we learn from this?

model = lm(Weight_change ~ Country + Diet + Country:Diet,
           data = Data)

summary(model)

# We cannot remove the interaction term looking at individual t-tests:

model2<-lm(Weight_change ~ Country + Diet,
           data = Data)

anova(model2,model)

summary(model2)
plot(model2)

#### Example with both continuous and categorical variables:


library(MASS)
data(ships)
help(ships)
head(ships)
View(ships)
attach(ships)
help("as.factor")
year<-as.factor(year)#把他变成一个向量
year
year_label<-as.numeric(year)
year_label
period_label<-as.numeric(period)
period_label
type_label<-as.numeric(type)
type_label
plot(incidents~service,col=year_label,pch=type_label)

model<-lm(incidents~ service*year*period*type)
summary(model)


# We are forced to simplify the model:

# Forward search

model0<-lm(incidents~service)

step(model0,scope=formula(incidents~service*year*period*type))

model1<-lm(incidents~service + year + period + type + service:year + service:period + 
             year:type + service:type)
summary(model1)

plot(model1)



##### variable transformation attempt below, spoiler: won't help too much.


y<-incidents+1
y
model1<-lm(y~service + year + period + type + service:year + service:period + 
             year:type + service:type)
boxcox(model1)

y<-log(incidents+1)
model0<-lm(y~service)

step(model0,scope=formula(y~service*year*period*type))


model2<-lm(y ~ service + year + period + type + service:period + service:year + 
             year:type + service:type)
summary(model2)

plot(model2)