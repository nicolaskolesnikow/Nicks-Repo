###############################################################################
# Lab 5: Site Productivity
#script to develop a model to estimate site index for cork oak
# as a function of site and climate variables
# Problem 1 - Exploratory analysis

###############################################################################
# change workspace
getwd()
setwd("....Lab 5-Site index")
getwd()  # show the new workspace

###############################################################################
# Load libraries needed, install packages if needed
library("dplyr")
library("tidyverse")
library("MASS")
library("olsrr")
library("car")
library("GGally")   # for nice pairwise plot matrix

###############################################################################
# Ask R to load the Sdata R file that was previously created 
load("Sdata.Rdata")
names(Sdata)
summary(Sdata)
str(Sdata)

###############################################################################
# keep the initial file
# create a new file without the lines with missing values in some soil variables
# that might be important for the "explanation" of S
Sdata.NA<-Sdata
Sdata<-na.omit(Sdata)
length(Sdata.NA$Soil_depth)
length(Sdata$Soil_depth)

##############################################################################
# now that we are not going to create new R files we may want to use 
# the attach() function
attach(Sdata)

###############################################################################
# Say how many plots per page you want - maybe adjusted
par(mfrow=c(1,1))


###############################################################################
# analyze the relationship between S=site index and each continuous variables
# for that it is advisable to create a subset of the Sdata with just the 
# continuous variables= Sdata.num

Sdata.num<-data.frame(S,Tavg,Tmin,Tmax,HR_9,HR_15_18,
                      P,NdaysP,Evap,Ndays_Tmin_l0,Ndays_Tmin_g20,Ndays_Tmin_g25,
                      Soil_depth,Soil_depth_A,NdaysFog,NdaysDew,NdaysFrost,
                      Martonne)

Sdata.num.cat<-data.frame(S,Tavg,Tmin,Tmax,HR_9,HR_15_18,
                          P,NdaysP,Evap,Ndays_Tmin_l0,Ndays_Tmin_g20,Ndays_Tmin_g25,
                          Soil_depth,Soil_depth_A,NdaysFog,NdaysDew,NdaysFrost,
                          Martonne,Soil_FAO2,Litology3,Soil_text2,Soil_text_A2) # add categorical

Sdata.num.d<-data.frame(S,Tavg,Tmin,Tmax,HR_9,HR_15_18,
                        P,NdaysP,Evap,Ndays_Tmin_l0,Ndays_Tmin_g20,Ndays_Tmin_g25,
                        Soil_depth,Soil_depth_A,NdaysFog,NdaysDew,NdaysFrost,
                        Martonne,Arenosols,Cambisols,Leptosols,Luvisols,Other_soil,
                        Sandstone,Sand,Schist,Shales,DFS,Granites,Other_litology,
                        Coarse,Medium,Fine,Coarse_A,Medium_A,Fine_A)            # add classes of 
# categorical

##############################################################################
# first step: look at the correlations between S and each continuous variable
cor(Sdata.num)
corr_S<-cor(Sdata.num)
corr_S
ggcorr(Sdata.num)              # requires package "GGally"
ggcorr(Sdata.num,label=TRUE,label_size=3.5,nbreaks=5,hjust=0.95)

#another coloring scheme
ggcorr(Sdata.num, geom = "blank",label_size=3.5, label = TRUE, hjust = 0.95) +
  geom_point(size = 9, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

##############################################################################
# look at the plots and linear regressions between S and each continuous variable

### Soil depth
plot(Soil_depth,S,xlab="Soil depth (cm)",ylab="Site index (base age 80)",pch=16)
abline(lm(S~Soil_depth))
S.depth<-lm(S ~ Soil_depth)
summary (S.depth)

### depth of the Ahorizon
plot(Soil_depth_A,S,xlab="Depth A horizon (cm)",ylab="Site index (base age 80)",pch=16)
abline(lm(S~Soil_depth_A))
S.Hsup<-lm(S~Soil_depth_A)
summary(S.Hsup)

### Evaporation
plot(Evap,S,xlab="Evaporation",ylab="Site index (base age 80)",pch=16)
abline(lm(S ~ Evap))
S.evap<-lm( S~ Evap)
summary(S.evap)

### Tavg
plot(Tavg,S,xlab="Temperature (?C)",ylab="Site index (base age 80)",pch=16)
abline(lm(S ~ Tavg))
S.Tavg<-lm( S~ Tavg)
summary(S.Tavg)

### Tmin
plot(Tmin,S,xlab="Minimum temperature (?C)",ylab="Site index (base age 80)",pch=16)
abline(lm(S ~ Tmin))
S.Tmin<-lm( S~ Tmin)
summary(S.Tmin)

### Tmax
plot(Tmax,S,xlab="Maximum temperature (?C)",ylab="Site index (base age 80)",pch=16)
abline(lm(S ~ Tmax))
S.Tmax<-lm( S~ Tmax)
summary(S.Tmax)

### HR_15_18
plot(HR_15_18,S,xlab="Relative humidity between 15 and 19",ylab="Site index (base age 80)",pch=16)
abline(lm(S ~ HR_15_18))
#identify(S,HR_15_18,tolerance=0.5)
S.HR_15_18<-lm( S ~ HR_15_18)
summary(S.HR_15_18)

###############################################################################
# analyze the relationship between S and each categorical variable
# do it automatically and using the dummy variables 
### Soil type
table(Soil_FAO2)
plot(as.factor(Soil_FAO2),S,xlab="Soil - FAO",ylab="Site index (base age 80)")
S.FAO.d <- lm(S ~ Arenosols + Cambisols + Luvisols + Leptosols)
summary(S.FAO.d)

S.FAO.d <- lm(S ~ Arenosols + Leptosols)
summary(S.FAO.d)

### Litology
plot(as.factor(Litology3),S,xlab="Litology",ylab="Site index (base age 80)")
S.lito_d <- lm(S ~ DFS + Granites + Sand + Sandstone + Schist + Shales)
summary(S.lito_d)

S.lito<-lm(S ~Litology3)
summary(S.lito)

### Soil texture
plot(as.factor(Soil_text2),S,xlab="Soil texture",ylab="Site index (base age 80)")
S.text.d <- lm(S ~ Medium + Fine)
summary(S.text.d)

S.text<-lm(S ~ Soil_text2)
summary(S.text)

### Soil texture A horizon
plot(as.factor(Soil_text_A2),S,xlab="Texture of A horizon",ylab="Site index (base age 80)")
S.textA<-lm(S ~ Soil_text_A2)
summary(S.textA)

###############################################################################
# saving the new datasets that were created as R data files
# Let's save it in R
save(Sdata.num,file="Sdata.num.Rdata")
save(Sdata.num.cat,file="Sdata.num.cat.Rdata")
save(Sdata.num.d,file="Sdata.num.d.Rdata")
