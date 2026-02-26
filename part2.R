

setwd("~/UBC 2021/Courses/FRST436/Labs/Lab 4-Growth functions") #set directory

library(FAwR)
library(ggplot2)# for plotting
library (nlstools) # nls residuals
library(nlme)     # nonlinear regression
library(Metrics)  # calculate RMSE

data(gutten)
head(gutten)

tree1.1<-gutten[gutten$tree.ID=="1.1",] #choose tree 1, site 1

#For Part 2
##########
#1. Lundqvist-Korf equation
##########

Lundqvist<-function (A,k,m) {output<-A*(exp(-k*(1/tree1.1$age.bh^m))) #define function
return (output)
}


model.fit<-nls(dbh.cm~Lundqvist(A,k,m),              # fit nls to obtain A,k and m coefficients
                start=list(A=30,k=5,m=0.7),
                data=tree1.1)
summary(model.fit) #model summary- check of residual standard error


#run some other diagnostics of model fit
plot(nlsResiduals(model.fit),which=1) #diagnostics based on residuals
plot(nlsResiduals(model.fit),which=6)
AIC(model.fit)                       #AIC= the smaller the better
rmse(tree1.1$dbh.cm,predict(model.fit)) #RMSE= difference predicted and observed, smaller better


#plot to observe the fit
plot(dbh.cm~age.bh,data=tree1.1,xlim=c(0,max(tree1.1$age.bh)),   #plot question 1
     ylim=c(0,max(tree1.1$dbh.cm)),ylab="DBH",xlab="Age",pch=16)
lines(tree1.1$age.bh,predict(model.fit),col="red")# fit of model

#######
#2. Chapman-Richards equation
######

Chapman<-function (A,k,m) {output<-A*(1-exp(-k*tree1.1$age.bh^(1/(1-m))))
return (output)
}

model.fit2<-nls(dbh.cm~Chapman(A,k,m),
                start=list(A=50,k=0.1,m=0.1),
                data=tree1.1)
summary(model.fit2)

#######
#3. Hossfeld IV Equation
#######

Hossfeld<-function (A,k,c) {output<-((tree1.1$age.bh^k)/(c+(1/A)*(tree1.1$age.bh^k)))
return (output)
}


model.fit4<-nls(dbh.cm~Hossfeld(A,k,c),
                start=list(A=30,k=1,c=0.2),
                data=tree1.1)
summary(model.fit4)


#Now Part 2b: graphs

###########
# Lundqvist-graph
###########
#a) Varying the asymptote A and keeping parameters k and m constant
#A=40,…,100; k=3; m=0.7

t<-seq(1:30)

Lundqvist<-function (A,k,m) {output<-A*(exp(-3*(1/t)^0.7))
return (output)
}

#plot the function for variable "A" values
library(ggplot2)
ggplot(gutten,aes(x=age.bh,y=dbh.cm)) +
  xlim(0,50)+
  ylim(0,80)+
  xlab("Age")+
  ylab("Dbh")+
 geom_function(fun=function (age.bh) 50*(exp(-3*(1/age.bh)^0.7)),lwd=0.6)+
   geom_function(fun=function (age.bh) 60*(exp(-3*(1/age.bh)^0.7)),linetype=2,lwd=0.6)+
   geom_function(fun=function (age.bh) 70*(exp(-3*(1/age.bh)^0.7)),linetype=3,lwd=0.6)+
   geom_function(fun=function (age.bh) 80*(exp(-3*(1/age.bh)^0.7)),linetype=4,lwd=0.6)+
   geom_function(fun=function (age.bh) 90*(exp(-3*(1/age.bh)^0.7)),linetype=5,lwd=0.6)+
   geom_function(fun=function (age.bh) 100*(exp(-3*(1/age.bh)^0.7)),linetype=6,lwd=0.6)+
   ggtitle ("Lundqvist-Korf")+
  theme_classic() 
            
                          
###########
# 2. Chapman-Richards graph
###########

#b) Varying the asymptote A and keeping parameters k and m constant
# A=50,…,100; k=0.05; m=0.2

ggplot(gutten,aes(x=age.bh,y=dbh.cm)) +
  xlim(0,50)+
  ylim(0,100)+
  xlab("Age")+
  ylab("Dbh")+
  geom_function(fun=function (age.bh) 50*(1-exp(-0.05*age.bh^(1/(1-0.2)))),lwd=0.6)+
  geom_function(fun=function (age.bh) 60*(1-exp(-0.05*age.bh^(1/(1-0.2)))),linetype=2,lwd=0.6)+
  geom_function(fun=function (age.bh) 70*(1-exp(-0.05*age.bh^(1/(1-0.2)))),linetype=3,lwd=0.6)+
  geom_function(fun=function (age.bh) 80*(1-exp(-0.05*age.bh^(1/(1-0.2)))),linetype=4,lwd=0.6)+
  geom_function(fun=function (age.bh) 90*(1-exp(-0.05*age.bh^(1/(1-0.2)))),linetype=5,lwd=0.6)+
  geom_function(fun=function (age.bh) 100*(1-exp(-0.05*age.bh^(1/(1-0.2)))),linetype=6,lwd=0.6)+
  ggtitle ("Chapman-Richards")+
  theme_classic() 


#################################
# 3. Hossfeld graph
#################################


#b) Varying the asymptote A and keeping parameters k and m constant
# A=50,…,100; k=1.4; c=0.2

Hossfeld<-function (A,k,c) {output<-((t^k)/(c+(1/A)*(t^k)))
return (output)
}

ggplot(gutten,aes(x=age.bh,y=dbh.cm)) +
  xlim(0,50)+
  ylim(0,100)+
  xlab("Age")+
  ylab("Dbh")+
  geom_function(fun=function (age.bh) ((age.bh^1.4)/(0.2+(1/50)*(age.bh^1.4))),lwd=0.6)+
  geom_function(fun=function (age.bh) ((age.bh^1.4)/(0.2+(1/60)*(age.bh^1.4))),linetype=2,lwd=0.6)+
  geom_function(fun=function (age.bh) ((age.bh^1.4)/(0.2+(1/70)*(age.bh^1.4))),linetype=3,lwd=0.6)+
  geom_function(fun=function (age.bh) ((age.bh^1.4)/(0.2+(1/80)*(age.bh^1.4))),linetype=4,lwd=0.6)+
  geom_function(fun=function (age.bh) ((age.bh^1.4)/(0.2+(1/90)*(age.bh^1.4))),linetype=5,lwd=0.6)+
  geom_function(fun=function (age.bh) ((age.bh^1.4)/(0.2+(1/100)*(age.bh^1.4))),linetype=6,lwd=0.6)+
  ggtitle("Hossfeld IV")+
  theme_classic() 





