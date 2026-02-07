
######################################################################
#Pearson correlation between competition indices and basal area growth
######################################################################

#set working directory
setwd("...Lab 2-Competition")

library(corrplot) # install it if missing

#bring in the two datasets, spatial and nonspatial

spatial<-read.csv("spatialCI_93_2026.csv") #load spatial indices
nonspatial<-read.csv("nspatialCI_93_2026.csv") # load non-spatial indices
dim(spatial)   #113 lines 
dim(nonspatial)
df<-read.csv("b1plot93_alldata.csv") #load dataset to obtain basal area growth
dim(df) #173 lines, 60 more
table(df$P) #113 for the trees that are "I"=inside the plot, the ones we calculated the CI's for!

df<-subset(df,P=="I") 
dim(df)# now we are good

df$growth<-df$BA97-df$BA93 #we calculate growth between 1997 and 1993

bind<-cbind(spatial,nonspatial,df$growth)# creat a dataframe with the CI indices and BA growth
head(bind)

# Explore the correlations
p<-cor(bind, method="pearson")[,13] # we can choose to do it just with last column, so it's easier
p                                   #disregards first columnn (just id)

#if you prefer to visualize the results
p<-cor(bind[,c(-1,-10)], method="pearson") #we need the full matrix
corrplot(p,method='number')
