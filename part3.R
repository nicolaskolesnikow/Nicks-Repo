library("FAwR")
library("nlstools")
data("gutten")
gutten$site<-as.factor(gutten$site) # need to specify that site is a factor

# Base model (fm1)
fm1<-nls(dbh.cm~age.bh^b3/(b2+age.bh^b3/b1),
                data=gutten,
                start=list(b1=29,b2=1,b3=1.3)
)
summary(fm1)

# Model with site as factor (fm2)
fm2<-nls(dbh.cm~age.bh^(b30+b31*(site=="2")+b32*(site=="3")+b33*(site=="4")+b34*(site=="5"))/
                 ((b20+b21*(site=="2")+b22*(site=="3")+b23*(site=="4")+b24*(site=="5"))+
                 age.bh^(b30+b31*(site=="2")+b32*(site=="3")+b33*(site=="4")+b34*(site=="5"))/b10),
                 data=gutten,
                 start=list(b10=29,
                            b20=1,b21=0,b22=0,b23=0,b24=0,
                            b30=1.3,b31=0,b32=0,b33=0,b34=0)
)
summary(fm2)



# Compare between fm1 and fm2, run diagnostics:
# Note you can also run aic() for the two models and use the code from part 2 to calculate an RMSE values

anova(fm1,fm2)

