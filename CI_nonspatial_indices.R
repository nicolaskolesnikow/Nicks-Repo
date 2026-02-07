#hello

setwd("....Lab 2-Competition")# set your wd

#install packages if needed

library(spatstat)
library(siplab)
library(tidyverse)
library(dplyr)

#########Plot= B1 3m clumped spacing
b1plot_93<-read.csv("b1plot93_alldata.csv", header=T)
b1plot_93<-filter(b1plot_93, !is.na(b1plot_93$dbh93))

head(b1plot_93)######### Plot is the plot number, some variable renaming down here
tail(b1plot_93)
str(b1plot_93)


dbh93 <- b1plot_93$dbh93
HT_93 <- b1plot_93$HT_93
SP <- b1plot_93$SP
BA93<- b1plot_93$BA93 # Basal area is in cm square per ha
bound <-as.factor(b1plot_93$P) 

str(b1plot_93)

MCW93 <- b1plot_93$MCW93
CW93 <- b1plot_93$CW93
b1plot_93$crad93 <-(MCW93)/2
crad93 <- b1plot_93$crad93

##################################################################################
# compute Steneker and Jarvis competition index which is the sum of the basal area of 
# neighbouring trees j for a subject tree i. BA93 in cm square
##################################################################################

b1plot_93<- filter(b1plot_93, b1plot_93$P == "I") #The trees within the plot are used (=I)
SJCI1_93 <- sum(b1plot_93$BA93) - (b1plot_93$BA93)

#write.csv(SJCI, file ="SJCI_93.csv")
##################################################################################
# compute Lorimer 1983 competition index sums up the diameters of neighbours divided by the subject
#tree. DBH in cm
##################################################################################

b1plot_93<- filter(b1plot_93, b1plot_93$P == "I")
LCI2_93 <- sum(b1plot_93$dbh93)/(b1plot_93$dbh93) - b1plot_93$dbh93
#write.csv(LCI, file = "LCI_93.csv")

##################################################################################
# compute Glover and Hool competition index which is the diameter of subject tree divided
#by the quadratic mean diameter
##################################################################################

b1plot_93<- filter(b1plot_93, b1plot_93$P == "I")
len <- length(b1plot_93$dbh93)
dbh93_mean <- mean(dbh93)
qmean_93 <- (sum((b1plot_93$dbh93)^2) / len)^ 0.5 
GHCI3_93 <- b1plot_93$dbh93/qmean_93

#write.csv(GHCI_93, file="GHCI_93.csv")


##################################################################################
#compute canopy closure in Rivas et. al (2005) revised.  Here the maximum crown is used
##################################################################################

b1plot_93 <- filter(b1plot_93, b1plot_93$P == "I")
CCCI4_93 <- sum(pi * crad93^2)- b1plot_93$crad93

#write.csv(CCCI4_93, file="CCCI4_93.csv")

##################################################################################
# compute Wykoff's competition index which is the sum of the basal areas of the trees  
# with greater diameters than the subject tree. 
##################################################################################
Wyk <- c()
for(i in 1:nrow(b1plot_93)) {
  a <- filter(b1plot_93, b1plot_93$P == "I") %>%
    as_tibble() %>%
    filter(.$dbh93 > .$dbh93[i]) %>%
    dplyr::summarize(sum(.$BA93))
  Wyk <- c(Wyk, as.numeric(a))
}

#write.csv(yo, file="WykCI_93_1.csv")


##################################################################################
#Combine all the non spatial indices into one file 
##################################################################################
nspatial_CI93 <- cbind(SJCI1_93,LCI2_93, GHCI3_93,CCCI4_93,Wyk)#merge indices

write.csv(nspatial_CI93, file="nspatialCI_93_2026.csv")#export
##################################################################################

