# adopted from https://bodowinter.com/

#start with linear models then move to mixed effects

# pitch ~ sex

pitch =  c(233,204,242,130,112,142)
sex= c(rep("female",3), rep("male", 3))

my.df = data.frame(pitch, sex)


xmdl = lm(pitch ~ sex, my.df)
summary(xmdl)

#output

# multiple R-squared = .921; 92.1% of stuff that is happening
# in out dataset is explained by the model 

# R adjusted: .90; besides variance explained,R adjusted 
# takes into account teh number of fixed effects in the model
# the more effects you enter, the lower R Adjusted 
# (relative to multiple R Squared)

# in front of intercept,you see 226.33
# this sould be the mean of female pithces 
mean(my.df[my.df$sex == "female",]$pitch)

# The difference between intercept estimate and sexmale estimate 
# is 128,which is the mean of male pitches
mean(my.df[my.df$sex == "male",]$pitch)

# p values of coefficients correspond to test whether
# coefficients are non zero 

# female is chosen to be the intercept because f comes before m 

# why using linear model for ategoricla variales
# The slope of the linear model 
#is the differnece between categories 

# now switch to a continuous variable like age instead of sex 

age = c(14,23,35,48,52,67)
pitch = c(252,244,240,233,212,204)
my.df = data.frame(age, pitch)

xmdl = lm(pitch~age, my.df)
summary(xmdl)

# interpreting the output
# here the intercept is about age = 0 
# if somebody age is 0, their pitch would be 267.0765

# the estimate of age means 
# for every year increase in age
# the pitch will decearse .90 Hz

# age 0 with the pitch of 267 is meangless 
# we can make it meanigful by ventering the picth then
# making a model with the centered values 

# check the assumptions
plot(xmdl, which = 1)

# if residuals vs fited is not a straight line, options:
# add fixed effects
# transformation of response like log-transformation.
# log-transformation of predictors 
# Ifyou’re seeing stripes in your residual plot,
# thenyou’re most likely dealing with some kind of categorical data
# –and you would need to turn to different class of models
# such as logistic models.

# normality of residuals 
hist(residuals(xmdl)) 
qqnorm(residuals(xmdl))


# influetial outliers 
dfbeta(xmdl)


# of independence is violated the we switch to mixed-effects
library(lme4)

# get the data 
politeness=  read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv") 
# higher values of frequency means higher pitch 


#check the data 
colnames(politeness)


#frequency (pitch) as a function of politness and sex (fixed effects)
# subject were reaptedly measured so not indepndent so random effect
# sceanrios foreach subject again random effect 


# the relationship between politeness and pitch for males and females
boxplot(frequency~attitude*gender, data = politeness,
        col=c("white", 'grey'))

# makeing the model with two random intercept
politeness.model = lmer(frequency~attitude+(1|subject)+(1|scenario),
                        politeness)

summary(politeness.model)
# interpretation 
# in random effect section, variance and std so of residuals 
# shows variability that is not due to scenario or subject
# my self: not due to fixed effects either, it is error of predicted 


#fixed effect part is like standard linear models 
# estimate of attitudepol is teh slope for the categorical effect of 
# politeness 

# -19 means to go from “informal” to “polite”, 
# you have to go down -19.695 Hz.
# inf comes before pol so reference is what 
# comes first alphabateically



# t value of attitudepol is estimate devided by std error

# the estimate of intercept in fixed effect part,
# 202,588 is the average of male and female for informal condition


# statistical significance

# likelihood: the probability of seeing the data given the model

# likeliihood ratio: to compare likelihood of two models
# one model without factor of interest (null model)
# model with that factor 

# REML=FALSE if you want to compare models with likelihood ratio

# null model without the effect of interest for exampel gender 
# full model with all predictors 

politeness.null = lmer(frequency~gender+(1|subject)+(1|scenario),
                       data=politeness, REML=FALSE) 
summary(politeness.null)
politeness.model = lmer(frequency~attitude+gender+(1|subject)+(1|scenario),
                        data=politeness, REML=FALSE) 
summary(politeness.model)
# compare two models with anova
anova(politeness.null,politeness.model) 

# now you can report 
# politeness affected pitch(χ2(1)=11.62, p=0.00065),
# lowering it by about 19.7 Hz ± 5.6 (standard errors)
# 19.7 comes from the fixed effect of attitude form reduced model 

#two last models were random intercpt models
# now make random slope models
politeness.model = lmer(frequency~attitude+gender+
                          (1+attitude|subject)+
                          (1+attitude|scenario),
                        data = politeness,
                        REML = FALSE)
coef(politeness.model)

#null and full models should have same structures
# if one is random inrecept teh other should be the same
# if one is random slope the other should be random slope as well



