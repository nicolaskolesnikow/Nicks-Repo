
setwd("...") #set directory

library(FAwR)
library(ggplot2)# for plotting
library (nlstools) # nls residuals
library(nlme)     # nonlinear regression

#######################
# Model example
#######################

data(gutten) #read data
head(gutten)
ggplot(gutten,aes(x=age.bh,y=dbh.cm,color=tree.ID))+  #plot dbh over age
  geom_line(show.legend=F)+
  facet_wrap(~site)

tree1.1<-gutten[gutten$tree.ID=="1.1",] #choose tree 1, site 1
plot(tree1.1$age.bh,tree1.1$dbh.cm)     #plot tree 1.1, useful to decide starting function

model.fit1<-nls(dbh.cm~B1*(1-exp(-log(2)/B2*age.bh)),
                start=list(B1=30, B2=10),
                data=tree1.1)
summary(model.fit1)

#################################
# 2. Hossfeld Equation  #where fill, you have to write some code yourself!!
#################################

# fill in the hossfeld equation for predicting size (dbh.cm), as well as A, m, and k values
model.fit2<-nls(dbh.cm~ ),
                start=list(A= , m=  , k= ),
                data=tree1.1)

summary(model.fit2)







