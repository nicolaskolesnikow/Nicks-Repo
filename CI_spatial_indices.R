#############################
# Competition indices-spatial
#############################
setwd("....Lab 2-Competition")#set working directory

library(spatstat)#package for spatial analysis
library(siplab) #package for computing competition indices
library(tidyverse)
library(dplyr)
#########Plot B1 3m clumped spacing
b1plot_93<-read.csv("b1plot93_alldata.csv", header=T)
b1plot_93<-filter(b1plot_93, !is.na(b1plot_93$dbh93))

names(b1plot_93)# names variables
head(b1plot_93)######### Plot is the plot number, assigning variables names, etc. here
tail(b1plot_93)
str(b1plot_93)

xcord <- b1plot_93$X
ycord <- b1plot_93$Y

dbh93 <- b1plot_93$dbh93
HT_93 <- b1plot_93$HT_93
SP <- b1plot_93$SP
BA93<- b1plot_93$BA93 # Basal area is in cm square per ha
bound <-as.factor(b1plot_93$P) 


MCW93 <- b1plot_93$MCW93
CW93 <- b1plot_93$CW93

b1plot_93$crad93 <-(MCW93)/2
crad93 <- b1plot_93$crad93

str(b1plot_93)

#Species and crown radius, dbh, height etc are used as marks 
marks93<-data.frame(crad93,dbh93,HT_93,bound)

#converting data to planar point patterns 

Win<-owin(poly=list(x=c(32.82, 25.75, -12.41,-5.00),y=c(9.44, 33.56,19.44,-5.00)))

patt93.dat <- ppp(xcord, ycord, window = Win, marks=marks93)

plot(patt93.dat)# if you want to visualize the spatial variables

##################################################################################
# Compute Hegyi index  using the siplab package with this function HegyiCI 
##################################################################################

hegyiCI <- function(imarks, jmarks, dists, dranks, par)  {     #imarks=subject plant
  (jmarks$dbh93 /imarks$dbh93) / dists                         #jmarks=competitors
}

# Calculate distances Hegyi indices using competitors within a radius of 5m)
Radius<-function(maxR){
  pairwise(patt93.dat, maxR=maxR, kernel=hegyiCI)
}
#convert ppp to a dataframe
B1_hegyiCI <- as.data.frame(Radius(5))
B1_hegyiCI <- filter(B1_hegyiCI,bound =="I")

B1_hegyiCI <-B1_hegyiCI %>% select('cindex')#select only column with CI
colnames(B1_hegyiCI)<-"hegyiCI"

# Write the output to a file
# write.csv(B1_hegyiCI, file = "HegCIR5_93_check.csv")
##################################################################################


##################################################################################
# Compute Martin and Ek index  using this function within a 5m radius distance
##################################################################################

MECI <-
  function(imarks, jmarks, dists, dranks, par)  {
      (jmarks$dbh93/imarks$dbh93) * exp((-16 * dists)/(jmarks$dbh93 + imarks$dbh93))
  }
# Calculate distances within a radius of 5m radius)
Radius<-function(maxR){
  pairwise(patt93.dat, maxR=maxR, kernel=MECI)
}
#convert ppp to a dataframe
B1_MECI <- as.data.frame(Radius(5))
B1_MECI <- filter(B1_MECI,bound =="I")
B1_MECI <-B1_MECI %>% select('cindex')#select only column with CI
colnames(B1_MECI)<-"MECI"

# Write the output to a file
# write.csv(B1_MECI, file = "MECI_93.csv")
##################################################################################

##################################################################################
# Compute Rouvinen and Kuuluvainen (1997) index 1  using this function 
##################################################################################
RKCI1 <-
  function(imarks, jmarks, dists, dranks, par)  {
    atan((jmarks$dbh93/dists)) 
  }
# Calculate distances within a radius of 5m radius)
Radius<-function(maxR){
  pairwise(patt93.dat, maxR=maxR, kernel=RKCI1)
}
#convert ppp to a dataframe
B1_RKCI1<- as.data.frame(Radius(5))
B1_RKCI1 <- filter(B1_RKCI1,bound =="I")
B1_RKCI1 <-B1_RKCI1 %>% select('cindex')#select only column with CI
colnames(B1_RKCI1)<-"RKCI1"
# Write the output to a file
#write.csv(B1_RKCI1, file = "RKCI1_93.csv")
##################################################################################
# Compute Rouvinen and Kuuluvainen (1997) index 2  using this function 
##################################################################################
RKCI2 <-
  function(imarks, jmarks, dists, dranks, par)  {
    (jmarks$dbh93/imarks$dbh93) * atan((jmarks$dbh93/dists)) 
  }
# Calculate distances within a radius of 5m radius)
Radius<-function(maxR){
  pairwise(patt93.dat, maxR=maxR, kernel=RKCI2)
}
#convert ppp to a dataframe
B1_RKCI2<- as.data.frame(Radius(5))
B1_RKCI2 <- filter(B1_RKCI2,bound =="I")
B1_RKCI2 <-B1_RKCI2 %>% select('cindex')#select only column with CI
colnames(B1_RKCI2)<-"RKCI2"
# Write the output to a file
#write.csv(B1_RKCI2, file = "RKCI2_93.csv")

##################################################################################

##################################################################################
# Compute Braathe 1980 competition index using this function . HT in m
##################################################################################

BCI <- function(imarks, jmarks, dists, dranks, par)  {
        (jmarks$HT_93/imarks$HT_93) / dists
}
# Calculate distances within a radius of 5m radius)
Radius<-function(maxR){
     pairwise(patt93.dat, maxR=maxR, kernel=BCI) 
}
#convert ppp to a dataframe
B1_BCI<- as.data.frame(Radius(5))
B1_BCI <- filter(B1_BCI,bound =="I")
B1_BCI <-B1_BCI %>% select('cindex')#select only column with CI
colnames(B1_BCI)<-"BCI"
# Write the output to a file
#write.csv(B1_BCI, file = "BCI_93.csv")

####################################################################################################
# Calculating the influence zone as a percentage aij = (ri+rj -Lij)/ri *100
####################################################################################################
Opie1 <- function(maxN){
  pairwise(patt93.dat, maxN=maxN, kernel= infzone1, select = powlinear.selOP1, selpar=list(ki=1, kj=1, p=1, r0=0, smark=1))
}


infzone1 <- function(imarks, jmarks, dists, dranks, par)  {
  
  ((jmarks$crad93 + imarks$crad93) - dists)/imarks$crad93 *100
}

powlinear.selOP1 <-
  function(imarks, jmarks, dists, dranks, par = list(ki=1, kj=1, p=1,r0=0, smark=1)) {
    # General competitor selection:  R < ki Si^p + kj Sj^p + r0
    with(as.list(par),
         dists < ki * imarks[[smark]]^p + kj * jmarks[smark]^p 
         
    )
  }  

B1_OPCI1<- as.data.frame(Opie1(20))

B1_OPCI1 <- filter(B1_OPCI1,bound =="I")
B1_OPCI1 <-B1_OPCI1 %>% select('cindex')#select only column with CI
colnames(B1_OPCI1)<-"OPCI1"
#write.csv(B1_OPCI1, file = "B1_OPCI1_93.csv")

####################################################################################################
# Calculating the influence zone as a percentage using Bella competition index
####################################################################################################
Bella1 <- function(maxN){
  pairwise(patt93.dat, maxN=maxN, kernel= bell1, select = powlinear.sel1, selpar=list(ki=1, kj=1, p=1, r0=0, smark=1))
}


bell1 <- function(imarks, jmarks, dists, dranks, par)  {
  
  (((jmarks$crad93 + imarks$crad93) - dists)/imarks$crad93 *100)*((jmarks$dbh93)/(imarks$dbh93))
}

powlinear.sel1 <-
  function(imarks, jmarks, dists, dranks, par = list(ki=1, kj=1, p=1,r0=0, smark=1)) {
    # General competitor selection:  R < ki Si^p + kj Sj^p + r0
    with(as.list(par),
         dists < ki * imarks[[smark]]^p + kj * jmarks[smark]^p 
         
    )
  }  

Bella1_index <- as.data.frame(Bella1(20))
B1_BLCI1 <- filter(Bella1_index,bound =="I")
B1_BLCI1 <-B1_BLCI1 %>% select('cindex')#select only column with CI
colnames(B1_BLCI1)<-"BLCI1"
#write.csv(B1_BLCI1, file = "B1_BLCI1_93.csv")

####################################################################################################
# Calculating the influence zone as a percentage using Monserud and EK competition index
####################################################################################################
MonEK1 <- function(maxN){
  pairwise(patt93.dat, maxN=maxN, kernel= mek1, select = powlinear.selEK1, selpar=list(ki=1, kj=1, p=1, r0=0, smark=1))
}


mek1 <- function(imarks, jmarks, dists, dranks, par)  {
  
  (((jmarks$crad93 + imarks$crad93) - dists)/imarks$crad93 *100) *
    ((jmarks$dbh93 *jmarks$HT_93)/(imarks$dbh93 *imarks$HT_93))
}

powlinear.selEK1 <-
  function(imarks, jmarks, dists, dranks, par = list(ki=1, kj=1, p=1,r0=0, smark=1)) {
    # General competitor selection:  R < ki Si^p + kj Sj^p + r0
    with(as.list(par),
         dists < ki * imarks[[smark]]^p + kj * jmarks[smark]^p 
         
    )
  }  

MonEK1_index<- as.data.frame(MonEK1(20))

B1_MonEK1 <- filter(MonEK1_index,bound =="I")
B1_MonEK1 <-B1_MonEK1 %>% select('cindex')#select only column with CI
colnames(B1_MonEK1)<-"MonEK1"
#write.csv(B1_MonEK1, file = "B1_MonEk_93.csv")

##################################################################################
#Combine all the spatial indices into one file 
##################################################################################
spatial_CI93 <- cbind(B1_hegyiCI,B1_MECI,B1_RKCI1,B1_RKCI2,B1_BCI,B1_OPCI1,B1_BLCI1,B1_MonEK1)#bind indices

write.csv(spatial_CI93, file="spatialCI_93_2026.csv") #export it
##################################################################################
