setwd("C://Users//PK//Desktop//imarticus//by me in class")
getwd()
insu <- read.csv("insurance.csv")
str(insu)
summary(insu$charges)
hist(insu$charges)
table(insu$region)
cor(insu[c("age","bmi", "children","charges")])
pairs(insu[c("age", "bmi", "children", "charges")])
hist(insu$age)
mean(insu$age)
x<- subset(insu, sex=="male")
y<- subset(insu, sex=="female")
mean(x$age) # nothing but anova test, comparing the mean of two samples
mean(y$age)
hist(x$age)
hist(y$age)
plot(insu$age, insu$bmi) # vcan say above 40 bmi , outliers
plot(x$age, x$bmi)
# now we are assigning bmi>40 r giving 1 and <40 as 0
insu$bmi_40<- ifelse(insu$bmi>40,1,0)
#what is the average age of bmi>40
x1<- subset(insu,bmi_40=="1")
y1 <- subset(insu, bmi_40=="0")
mean(x1$age)
mean(y1$age)
#--- mean charges w.r.t bmi

mean(x1$charges)
mean(y1$charges)
#--
x2<- subset(insu,bmi<"20")
mean(x2$charges)
#--- age w.r.t charges
plot(insu$age, insu$charges)

plot(insu$age^2, insu$charges)

#----coloring smoking
sp<- ggplot(insu, aes(x=age, y=charges,color=smoker))+geom_point()
sp
#---ggally package

#--using bmi

sp1 <- ggplot(insu, aes(x=age, y=charges,color=bmi_40))+geom_point()
sp1
plot(insu$children, insu$charges)
sp2<-  ggplot(insu, aes(x=children, y=charges,color=smoker))+geom_point()
sp2
#so with 0 children more smokers
#---
z<- subset(insu, insu$children=="5"& insu$smoker=="yes")
z
subset(insu, children=="5"& smoker=="yes")
insurance<- insu[-1086,]
insurance
#--- otherwise v can use !
z1<- subset(insu, !(children=="5"& smoker=="yes"))
z1
plot(insu$region, insu$charges)
#---- north east more than 50k charges---
z2<- subset(insu, (insu$region=="northeast"|region =="northwest"|region=="southeast") 
            & (insu$charges >50000.000))
z2
z3 <- subset(insu, insu$region=="northeast" & insu$charges > 50000)
z3
ggpairs(insu, aes(color=smoker, alpha=0.4))

rk<- subset(insu,insu$smoker=="yes")
plot(rk$bmi, rk$chages)
cor(rk[c("bmi", "charges")])
cor(rk$bmi,rk$charges)
#so strong linear relation is thers so that v can use linear regression
model_smoker<- lm(charges~bmi,data=rk)
summary(model_smoker)
#--- mana pithyam----
rk_0 <- subset(insu, insu$smoker=="no")
cor(rk_0$bmi, rk_0$charges)
model_non<- lm(charges~bmi, data=rk_0)
summary(model_non)
cor(insu$children,insu$charges)
cor(insu$age, insu$charges)
#______
predicted = predict(model_smoker, data.frame(bmi=rk$bmi))
plot(predicted, rk$charges) #just for performance of model
residuals_smo<- predicted - rk$charges
residuals_smo

cor(insu$age, insu$bmi)
#no relation betweeen age and bmi
a<- ggplot(insu, aes(x=age))+geom_area(stat="bin")
a
ggplot(insu, aes(x=age))+geom_density(kernel="gaussian")

x<- ggplot(insu, aes(x=age))
x+geom_histogram(binwidth = 5)
x+geom_histogram(binwidth = 10)+ggtitle("RAM")
x+geom_histogram(binwidth = 2)
ggplot(rk,aes(x=age))+geom_density(kernel="gaussian")
#-----15/04/2018-------

plot(predicted, residuals_smo)
abline(h=10, col="red")

mod_residuls<- abs(residuals_smo)
mod_residuls
plot(predicted, mod_residuls)
#---extracting top 50

s1<-sort(mod_residuls,decreasing=T)
head(s1,50)
rk$residul<- residuals_smo
rk_sort<- sort(rk, rk$residul, decreasing = T)
head(rk_sort,50)
#---dplyr loaded to extract

df= tbl_df(rk)
df %>%
  group_by(sex) %>%
  summarise(avg=mean(charges))

df= tbl_df(rk)
df %>%
  group_by(sex) %>%
  summarise(avg=mean(charges))%>%
  arrange(avg)
df %>%
  arrange(desc(charges))

rk$modres=mod_residuls
df=tbl_df(rk)
ram <- df %>%
  arrange(desc(mod_residuls))

ram_50<- head(ram,50)

plot(ram_50$children)
table(insu$sex)
table(rk$sex)

