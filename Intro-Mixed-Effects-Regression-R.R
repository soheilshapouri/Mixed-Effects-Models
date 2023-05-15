# Adopted from OurCodingClub
# Almost all of the code and content from https://gkhajduk.github.io/


#get the dataset 
# https://gkhajduk.d.pr/9GPn/3nbbPoK6

load("dragons.RData")

head(dragons)
# we want to predict test scores from body size of dragons 



# standardizing predictor 
# standardizing predictors make them comparable

# mean of zero (“centering”) standard deviation of one (“scaling”)
dragons$bodyLength2 <- scale(dragons$bodyLength,
                             center = T,
                             scale = T)
#  Standardization of predictors does not change the shape of their distribution.
# It only changes the scale and location of the distribution.


# we want to predict test scores from body size of dragons 
# simple linear regression
basic.lm <- lm(testScore~bodyLength2, data = dragons)

summary(basic.lm)
# R squared .15.  bodysize a significant predictor of test score



# is the above-mentioned model good? 
library(tidyverse)


(prelim_plot <- ggplot(dragons, aes(bodyLength2, testScore))+
  geom_point()+
  geom_smooth(method = lm))
# shows the linear relationship between bodylength and test score 


#residuals vs fitted should be a straight line
plot(basic.lm, which = 1) 

# normal qq 
plot(basic.lm, which = 2)


# check the independence

ggplot(dragons, aes(bodyLength2, testScore))+
  geom_point()+
  facet_wrap(~mountainRange)+
  geom_smooth(method = 'lm')
# some mountain ranges have positive correlation
# some have negative correlation



# we can add mountain ragne as a fixed effect
# this might be used if we were interested in the effects of mountain range
mountain.lm <- lm(testScore~bodyLength2+mountainRange,
                  data = dragons)
summary(mountain.lm)
# R squared is improved by a wide margin .58
# bodylength is not a significant predictor anymore 


# But we are not interested in the effects of mountain range so
# switch to mixed-effects model 

library(lme4)

# fixed effects: body size 
# fixed effects: explanatory variables we are interested in 

# random effects: grouping variables (mountain range here)
# or what we try to control
# (in R random effects should be categorical) 

# if we were interested in the effects of mountain range 
# we would include it as a fixed effect as above
# but we just want to control it as we know it affects the relationship 
# between body size and test score 


# typically random effects have at least five levels, so
# sex as two-level factor is better to be considered as fixed effect


# fixed effect: bodyLength
# random effect: mountainRange 
mixed.lmer <- lmer(testScore~bodyLength2+(1|mountainRange),
                   data = dragons)
# (1|variable) for fitting random effects 

summary(mixed.lmer)

# from t value in the fixed effects section of the output, 
# it seems body length is not a predictor of testScore anymore 

# by dividing mountainRnage variance by total variance we can see 
# its importance
339.7 / (339.7+223.8) #.60
# so 60% of variance is explained by mountainRange
# after explaining variance by fixed effects 

# is this model good? 

# check the plots (probably residuals)
plot(mixed.lmer)

# qq plot (residuals)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))

# OK, Whatever comes after 1| in (1|varibale) is a random effect
# but random effects can be crossed or nested 

# while all hierarchical models are mixed, 
# not all mixed models are hierarchical


# if every dragon visits every mountainRange then it is crossed
# if some dragons visit some mountainRange it is partially crossed

# for every eight mountainRange there are three sites: a, b, c
# but these are meaningless without mountain ranges
# so nesting is implict here,and we shoudl make it explicit

dragons <- within(dragons, sample <- factor(mountainRange:site))

# WRONGLY write the codes for sites 
mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)  # treats the two random effects as if they are crossed
#this model assumes the effects are crossed 

# Is there an association between body length and intelligence 
# in dragons after controlling for variation in mountain ranges
# and sites within mountain ranges?

mixed.lmer2 <- lmer(testScore ~ bodyLength2 +
                      (1|mountainRange) +
                      (1|sample),
                    data = dragons)
summary(mixed.lmer2)

# alternatively you can use these codes 
# (1|mountainRange/site) 
# (1|mountainRange) + (1|mountainRange:site)

ggplot(dragons, aes(bodyLength2, testScore, color = site))+
  geom_point()+
  facet_wrap(~mountainRange)+
  geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)),
            aes(y = pred),
            size = 1)
# based on the plots all lines are parallel 
# because we have fitted a random_intercept model 
# we let intercept to freely vary
# we kept the slopes constant

# random intercept says some dragons are dumber some smarter
# so the intercept should vary
# but the relationship between body size and test score is consistent

# in life sciences, probably we need random intercept random slope 
# starting points are different but also the relationship changes 

mixed.randslop <- lmer(testScore~bodyLength2+
                         (1+bodyLength2|mountainRange/site),
                       data = dragons)

summary(mixed.randslop)
# the first body length is fixed effect
# rthe second body length is random slope
# mountain range is random intercept 


# plot random-intercept random-slope model

ggpredict(mixed.lmer2, terms = c("bodyLength2", "mountainRange"), type = "re") %>% 
  plot() +
  labs(x = "Body Length", y = "Test Score", title = "Effect of body size on intelligence in dragons") + 
  theme_minimal()


library(sjPlot)

# Visualise random effects 
(re.effects <- plot_model(mixed.randslop, type = "re", show.values = TRUE))

# show summary
summary(mixed.randslop)

stargazer(mixed.lmer2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")




