###############################################################################
# Lab 5- Site productivity, PROBLEM 2
#script to develop a model to estimate site index for cork oak
# as a function of site and climate variables
# Selecting alternative "good" models 

###############################################################################
# change workspace
setwd("...")

###############################################################################

###############################################################################
# Ask R to load the Sdata.num R file that was previously created 
load("Sdata.num.d.Rdata")
str(Sdata.num.d)

###############################################################################
# algorithms for the selection of subsets of variables
# stepwise algorithms
# first we need to fit the "full model", or model with all the variables

### trying using all the variables in Sdata.num.d
### it does not work due to the excess of multicollinearity among the regressors
### leading to parameter estimates that are NA
S.full.model <- lm(S ~., data=Sdata.num.d) # with ~., you tell R get all variables
summary(S.full.model)


### let's delete some of the variables, until all the parameters can be estimated
### delete the categories of the dummy variables that were not so important
### in the exploratory analysis
summary(lm(S ~. -Ndays_Tmin_l0 -Ndays_Tmin_g20 -Martonne # with - you tell R minus this variable
           -Coarse -Medium -Coarse_A -Fine_A
           -Other_soil
           -Other_litology -DFS -Granites - Schist,
           data=Sdata.num.d))

#######################################################################################################################
# stepwise regression

### the full model has some parameters whose estimates do not significantly differ from zero
### we would have deleted them one at a time, by selecting the one that has a higher P-value (less significant)
### this is the stepwise/backward algorithm using P-value as a criteria

summary(lm(S ~. -Ndays_Tmin_l0 -Ndays_Tmin_g20 -Martonne
           -Coarse -Medium -Coarse_A -Fine_A
           -Other_soil
           -Other_litology -DFS -Granites - Schist,
           data=Sdata.num.d))
summary(lm(S ~. -Ndays_Tmin_l0 -Ndays_Tmin_g20 -Martonne
           -Coarse -Medium -Coarse_A -Fine_A
           -Other_soil
           -Other_litology -DFS -Granites - Schist
           -Medium_A,                                  #start taking out Medium_A
           data=Sdata.num.d))
summary(lm(S ~. -Ndays_Tmin_l0 -Ndays_Tmin_g20 -Martonne
           -Coarse -Medium -Coarse_A -Fine_A
           -Other_soil
           -Other_litology -DFS -Granites - Schist
           -Medium_A -Luvisols,                        #now taking out Luvisols, etc.
           data=Sdata.num.d))
summary(lm(S ~. -Ndays_Tmin_l0 -Ndays_Tmin_g20 -Martonne
           -Coarse -Medium -Coarse_A -Fine_A
           -Other_soil
           -Other_litology -DFS -Granites - Schist
           -Medium_A -Luvisols -Fine,
           data=Sdata.num.d))
summary(lm(S ~. -Ndays_Tmin_l0 -Ndays_Tmin_g20 -Martonne
           -Coarse -Medium -Coarse_A -Fine_A
           -Other_soil
           -Other_litology -DFS -Granites - Schist
           -Medium_A -Luvisols -Fine -NdaysFog,
           data=Sdata.num.d))
summary(lm(S ~. -Ndays_Tmin_l0 -Ndays_Tmin_g20 -Martonne
           -Coarse -Medium -Coarse_A -Fine_A
           -Other_soil
           -Other_litology -DFS -Granites - Schist
           -Medium_A -Luvisols -Fine -NdaysFog -Sandstone,
           data=Sdata.num.d))
summary(lm(S ~. -Ndays_Tmin_l0 -Ndays_Tmin_g20 -Martonne
           -Coarse -Medium -Coarse_A -Fine_A
           -Other_soil
           -Other_litology -DFS -Granites - Schist
           -Medium_A -Luvisols -Fine -NdaysFog -Sandstone -HR_15_18,
           data=Sdata.num.d))
summary(lm(S ~. -Ndays_Tmin_l0 -Ndays_Tmin_g20 -Martonne
           -Coarse -Medium -Coarse_A -Fine_A
           -Other_soil
           -Other_litology -DFS -Granites - Schist
           -Medium_A -Luvisols -Fine -NdaysFog -Sandstone -HR_15_18 -Soil_depth_A,
           data=Sdata.num.d))
summary(lm(S ~. -Ndays_Tmin_l0 -Ndays_Tmin_g20 -Martonne
           -Coarse -Medium -Coarse_A -Fine_A
           -Other_soil
           -Other_litology -DFS -Granites - Schist
           -Medium_A -Luvisols -Fine -NdaysFog -Sandstone -HR_15_18 -Soil_depth_A -Leptosols,
           data=Sdata.num.d))
summary(lm(S ~. -Ndays_Tmin_l0 -Ndays_Tmin_g20 -Martonne
           -Coarse -Medium -Coarse_A -Fine_A
           -Other_soil
           -Other_litology -DFS -Granites - Schist
           -Medium_A -Luvisols -Fine -NdaysFog -Sandstone -HR_15_18 -Soil_depth_A -Leptosols -Ndays_Tmin_g25,
           data=Sdata.num.d))
summary(lm(S ~. -Ndays_Tmin_l0 -Ndays_Tmin_g20 -Martonne
           -Coarse -Medium -Coarse_A -Fine_A
           -Other_soil
           -Other_litology -DFS -Granites - Schist
           -Medium_A -Luvisols -Fine -NdaysFog -Sandstone -HR_15_18 -Soil_depth_A -Leptosols -Ndays_Tmin_g25 -NdaysFrost,
           data=Sdata.num.d))       #now all variables are significant


### now use R stepwise algorithms
### start with a full model
S.full.model <- lm(S ~. -Ndays_Tmin_l0 -Ndays_Tmin_g20 -Martonne
                   -Coarse -Medium -Coarse_A -Fine_A
                   -Other_soil
                   -Other_litology -DFS -Granites - Schist,
                   data=Sdata.num.d)

# selects the subsets of predictors to retain using the AIC as criteria
# requires "MASS" package
step.model <- stepAIC(S.full.model,direction="both",trace=T) #you could also try direction
summary(step.model)                                              #backward or forward

#using another method=ols_step
ols_step_both_p(S.full.model) #both, you could also try backward and forward

##############################################################################
# comparing the candidate models, this is just an example!! use the variables you extract from above!
#anova tests whether reduction in the residual sum of squares is statistically significant or not

#using anova to compare models only makes sense if models are nested

S.mod5<-lm(S ~ Tmax + P + Evap + Soil_depth + Soil_depth_A, data=Sdata.num)#not best model! just an example
summary(S.mod5)
vif(S.mod5)  #variance inflation factor=indication of multicollinearity, if value is high >5 then worry!

S.mod3<-lm(S ~ Tmax + Evap + Soil_depth, data=Sdata.num) #we take out P and Soil_depth_A
summary(S.mod3)
vif(S.mod3)

anova(S.mod3,S.mod5)
#If the p-value is greater than 0.05, we should favor the simpler model, S.mod3

##################
#once you select your best models you can check for:
plot(S.mod3,which=1)#plotting residuals to check the assumptions of linearity, equal variance
plot(S.mod3,which=2)#check normality

#check R-squared, adjusted R-squared, AIC, VIF
r2 <- summary(S.mod3)$r.squared        # R2 always increases as we add new variables (no
# matter how random they are), tells us % variation explained
adj.r2 <- summary(S.mod3)$adj.r.squared # an improvement of R2 statistic

pred<-predict(S.mod3)
rmse<-sqrt(mean((Sdata$S - pred)^2)) #average distance between actual and predicted values
aic <- AIC(S.mod3) #AIC, Akaike's information criterion, the smallest the better
vif.max <- max(vif(S.mod3))


