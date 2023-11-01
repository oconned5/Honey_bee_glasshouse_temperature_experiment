########################################################################################
# Model selection for glasshouse experiment investigating the impact of 
# temperature on honey bee physiology


# Housekeeping

rm(list=ls()) # remove everything currently held in the R memory

graphics.off() # close all open graphics windows 

# Packages that may be needed, remove the # to install

#install.packages("arm")
#install.packages("lme4")
#install.packages("MuMIn")
#install.packages("car")
#install.packages("LMERConvenienceFunctions")
#install.packages("ggplot2")
#install.packages("sjPlot")
#install.packages("tab")
#install.packages("rsq")
#install.packages("MASS")
#install.packages("dplyr")
#install.packages("tidyr")



#################################################################################

## read in the data
dframe1 <- read.csv(file.choose())    # Select "Honey_bee_glasshouse_temperature_experiment"
head(dframe1)
summary(dframe1)

## subset out the experimental period, during the day, when data was recorded
dframe2 <- subset(dframe1, period == "experiment" & glasshouse_temperatue_setting == "day" &
                    bees_out != "NA" & temperature_glasshouse_ibutton_01 != "NA")


#################################################################

## Model 1----

# Relationship between worker movement and glasshouse temperature
# plus Experimental_group as a fixed effect, with hive and experimental day as random terms

# examine the response variable
hist(dframe2$bees_out) ## count data with a poisson or negative binomial style distribution

# examine the continuous explanatory variable 
hist(dframe2$temperature_glasshouse_ibutton_01)  # quite normally distributed

## as we'll be dealing with poisson models it's important to check overdispersion
### write function to calculate overdispersion

library(dplyr)
library(tidyr)
overdisp_fun <- function(model) {vpars <- function (m)    ## run in one block
{nrow(m)*(nrow(m) + 1)/2}
model.df <- sum(sapply(VarCorr(model), vpars)) +
  length(fixef(model))
rdf <- nrow(model.frame(model)) -model.df
rp <- residuals(model, type="pearson")
Pearson.chisq <- sum(rp^2)
prat <- Pearson.chisq/rdf
pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
c(chisq=Pearson.chisq, ratio=prat, rdf=rdf, p=pval)}

#################################################################


### poisson family for count response----

## All variables plus interactions model selection
## carry out model selection with different link functions and compare for fit

### log link function

library(lme4)
global.model1a <- glmer(bees_out  ~                          # count dependent variable
                        temperature_glasshouse_ibutton_01 +  # continuous explanatory variable
                          Experimental_group +                          # fixed term
                          temperature_glasshouse_ibutton_01:Experimental_group + # interaction term 
                          (1|experiment_day) + (1|hive),   # the random terms - hive ID and experiment day
                      na.action = na.pass,
                      family = "poisson" (link="log"), data = dframe2) # poisson model of count data 

summary(global.model1a) #the warning message here suggests we have a scaling issue, need to standardise variablies

# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.global.model1a <- standardize(global.model1a, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.global.model1a)

## assess the full global model to see whether it's worth dropping terms
library(MuMIn)
model.set1a <- dredge(stdz.global.model1a)
model.set1a    ## the full model comes out as the best


# R-squared 
library(MuMIn)
r.squaredGLMM(stdz.global.model1a)

## check the residuals
plot(stdz.global.model1a, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals

sresid <- resid(stdz.global.model1a, type = "pearson")
hist(sresid)

## overdispersion
overdisp_fun(stdz.global.model1a)   ### this mode has overdispersion >2, try another model



### sqrt link function

library(lme4)
library(lme4)
global.model1b <- glmer(bees_out  ~                           # count dependent variable
                          temperature_glasshouse_ibutton_01 + # continuous explanatory variable
                          Experimental_group +                          # fixed term
                          temperature_glasshouse_ibutton_01:Experimental_group + # interaction term 
                          (1|experiment_day) + (1|hive),   # the random terms - hive ID and experiment day
                        na.action = na.pass,
                        family = "poisson" (link="sqrt"), data = dframe2) # poisson model of count data 

summary(global.model1b) #the warning message here suggests we have a scaling issue, need to standardise variablies

# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.global.model1b <- standardize(global.model1b, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.global.model1b)

## assess the full global model to see whether it's worth dropping terms
library(MuMIn)
model.set1b <- dredge(stdz.global.model1b)
model.set1b    ## the full model comes out as the best


# R-squared 
library(MuMIn)
r.squaredGLMM(stdz.global.model1b)

## check the residuals
plot(stdz.global.model1b, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals

sresid <- resid(stdz.global.model1b, type = "pearson")
hist(sresid)

## overdispersion
overdisp_fun(stdz.global.model1b)   ### this mode has overdispersion >2, try another model


### identity link function - fails, identity link not appropriate

library(lme4)
global.model1c <- glmer(bees_out  ~                           # count dependent variable
                          temperature_glasshouse_ibutton_01 + # continuous explanatory variable
                          Experimental_group +                          # fixed term
                          temperature_glasshouse_ibutton_01:Experimental_group + # interaction term 
                          (1|experiment_day) + (1|hive),   # the random terms - hive ID and experiment day
                        na.action = na.pass,
                        family = "poisson" (link="identity"), data = dframe2) # poisson model of count data 
summary(global.model1c) 


### negative binomial family for count response----

## as the poisson was overdispersed use negative binomial

#######################################################################################
#### FINAL MODEL FOR MODEL 1 ##########################################################
#######################################################################################

library(lme4)
library(MASS)
global.model1d <- glmer.nb(bees_out  ~                            # count dependent variable
                             temperature_glasshouse_ibutton_01 +  # continuous explanatory variable
                             Experimental_group +                          # fixed term
                             temperature_glasshouse_ibutton_01:Experimental_group + # interaction term 
                             (1|experiment_day) + (1|hive),   # the random terms - hive ID and experiment day
                           na.action = na.pass, nAGQ = 1, 
                           data = dframe2)                       # negative bionomial model of count data 

summary(global.model1d) #the warning message here suggests we have a scaling issue, need to standardise variablies

# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.global.model1d <- standardize(global.model1d, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.global.model1d)

## assess the full global model to see whether it's worth dropping terms
library(MuMIn)
model.set1d <- dredge(stdz.global.model1d)
model.set1d    ## the full model comes out as the best
write.csv(model.set1d, file = "model_1_negative_binomial.csv")

# R-squared 

library(MuMIn)
r.squaredGLMM(stdz.global.model1d)

## check residuals
plot(stdz.global.model1d, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals

sresid <- resid(stdz.global.model1d, type = "pearson")
hist(sresid)


## calculate overdispersion
overdisp_fun(stdz.global.model1d)  ## overdispersion back down to the 1-2 zone!




#################################################################

## Model 2----

# Relationship between hive temperature and glasshouse temperature
# plus Experimental_group as a fixed effect, with hive and experimental day as random terms

## investigate distribution of response variable
hist(dframe2$temperature_C_hive_ibutton_01)  ## fairly normally distributed


#################################################################


### gaussian family for continuous response----

## All variables plus interactions model selection
## carry out model selection with different link functions and compare for fit

### identity link function ----

#######################################################################################
#### FINAL MODEL FOR MODEL 2 ##########################################################
#######################################################################################

library(lme4)
global.model2a <- glmer(temperature_C_hive_ibutton_01  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Experimental_group +   # fixed term
                          temperature_glasshouse_ibutton_01:Experimental_group +    # interaction terms
                          (1|experiment_day) + (1|hive),                   # the random terms 
                        family = gaussian (link = identity),
                        na.action = na.pass,
                        data = dframe2) # gaussian model of temperature data 

summary(global.model2a) 


library(arm)                                    
stdz.global.model2a <- standardize(global.model2a, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.global.model2a)

## assess the full global model to see whether it's worth dropping terms
library(MuMIn)
model.set2a <- dredge(stdz.global.model2a)
model.set2a              ## the full model comes out as the best


# R-squared 
library(MuMIn)
r.squaredGLMM(stdz.global.model2a)


## inspect residuals
plot(stdz.global.model2a, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals

sresid <- resid(stdz.global.model2a, type = "pearson")
hist(sresid)


### log link function ----

### doesn't converge even after altering optimizer

library(lme4)
global.model2b <- glmer(temperature_C_hive_ibutton_01  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Experimental_group +   # fixed term
                          temperature_glasshouse_ibutton_01:Experimental_group +    # interaction terms
                          (1|experiment_day) + (1|hive),                # the random term 
                        family = gaussian (link = log),
                        #  control=glmerControl(optimizer="bobyqa",       
                        #                      optCtrl=list(maxfun=2e5)), # try Nelder_Mead, nloptwrap and bobyqa optimisers
                        # for full model to aid convergence
                        na.action = na.pass,
                        data = dframe2) # gaussian model of temperature data 

summary(global.model2b) 

### inverse link function ----

### doesn't converge even after altering optimizer

library(lme4)
global.model2c <- glmer(temperature_C_hive_ibutton_01  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Experimental_group +   # fixed term
                          temperature_glasshouse_ibutton_01:Experimental_group +    # interaction terms
                          (1|experiment_day) + (1|hive),                # the random term 
                        family = gaussian (link = "inverse"),
                        #  control=glmerControl(optimizer="bobyqa",       
                        #                      optCtrl=list(maxfun=2e5)), # try Nelder_Mead, nloptwrap and bobyqa optimisers
                        # for full model to aid convergence
                        na.action = na.pass,
                        data = dframe2) # gaussian model of temperature data 

summary(global.model2c) 

### Gamma family for continuous response----


### identity link function ----


library(lme4)
global.model2d <- glmer(temperature_C_hive_ibutton_01  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Experimental_group +   # fixed term
                          temperature_glasshouse_ibutton_01:Experimental_group +    # interaction terms
                          (1|experiment_day) + (1|hive),                # the random term 
                        family = Gamma (link = identity),
                        control=glmerControl(optimizer="bobyqa",       
                                             optCtrl=list(maxfun=2e5)), # bobyqa optimiser aids convergence
                        na.action = na.pass,
                        data = dframe2) # Gamma model of temperature data 

summary(global.model2d) 

library(arm)                                    
stdz.global.model2d <- standardize(global.model2d, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.global.model2d)

## assess the full global model to see whether it's worth dropping terms
library(MuMIn)
stdz.model.set2d <- dredge(global.model2d)
stdz.model.set2d           ## the full model comes out as the best


# R-squared 
library(rsq)
rsq.glmm(stdz.global.model2d)

## inspect residuals
plot(stdz.global.model2d, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals

sresid <- resid(stdz.global.model2d, type = "pearson")
hist(sresid)


### log link function ----

### doesn't converge even after altering optimizer

library(lme4)
global.model2e <- glmer(temperature_C_hive_ibutton_01  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Experimental_group +   # fixed term
                          temperature_glasshouse_ibutton_01:Experimental_group +    # interaction terms
                          (1|experiment_day) + (1|hive),                # the random term 
                        family = Gamma (link = log),
                        #   control=glmerControl(optimizer="bobyqa",       
                        #                     optCtrl=list(maxfun=2e5)),  # try Nelder_Mead, nloptwrap and bobyqa optimisers
                        # for full model to aid convergence
                        na.action = na.pass,
                        data = dframe2) # Gamma model of temperature data 

summary(global.model2e) 

### inverse link function ----

### doesn't converge even after altering optimizer

library(lme4)
global.model2f <- glmer(temperature_C_hive_ibutton_01  ~                   # the dependent variable
                          temperature_glasshouse_ibutton_01 + Experimental_group +  # fixed term
                          temperature_glasshouse_ibutton_01:Experimental_group +    # interaction terms
                          (1|experiment_day) + (1|hive),                   # the random term 
                        family = Gamma (link = "inverse"),
                      #  control=glmerControl(optimizer="nloptwrap",       
                       #                      optCtrl=list(maxfun=2e5)),    # try Nelder_Mead, nloptwrap and bobyqa optimisers
                        # for full model to aid convergence
                        na.action = na.pass,
                        data = dframe2) # Gamma model of temperature data 

summary(global.model2f) 

### Model 2 - compare the top models and select the model with the best fit ----

model.set2a # Gaussian family, identity link
model.set2d # Gamma family, identity link

write.csv(model.set2a, file = "model_2_gaussian_identity.csv")
write.csv(model.set2d, file = "model_2_gamma_identity.csv")




#################################################################

## Model 3----

# Relationship between hive humidity and glasshouse temperature
# plus Experimental_group as a fixed effect, with hive and experimental day as random terms

## lets have a look at our continuous variables
hist(dframe2$humidity_percentage_hive_ibutton_01)  ### long negative left-skewed tail

## explore low values for humidity
df_explore <- subset(dframe2, humidity_percentage_hive_ibutton_01 <50)
View(df_explore)                   ## all from one hive (hive_02), maybe a placement issues,
                                   ## or equipment failure?                                              


### is it the hive or the ibutton which is producing strangely low humidity numbers?
## the ibutton was used on another hive (hive_25), lets check that 
df_explore2 <- subset(dframe2, hive == "hive_25" & hive_ibutton_01 == "No.02")
View(df_explore2) 
hist(df_explore2$humidity_percentage_hive_ibutton_01)   #### a normal humidity distribution

## so it's probably not the ibutton, is it the hive?
df_explore3 <- subset(dframe2, hive == "hive_02" & hive_ibutton_01 == "No.02")
View(df_explore3) 
hist(df_explore3$humidity_percentage_hive_ibutton_01)  ### it's the hive

## possibly there was an unknown issue with the hive
## exclude it so as not to strongly skew the model
dframe2a <- subset(dframe2, hive != "hive_02")

hist(dframe2a$humidity_percentage_hive_ibutton_01)  ## still has a a negative skew, though not very dramatic

## try a negative square root transformation for a moderate transformation to reduce the skew
neg_sqrt_humidity <- sqrt(max(dframe2a$humidity_percentage_hive_ibutton_01+1) - dframe2a$humidity_percentage_hive_ibutton_01) 

hist(neg_sqrt_humidity) ## success! the humidity data is no longer skewed

hist(dframe2a$temperature_glasshouse_ibutton_01)  ## subsectioned temperature still has a normal distribution


#################################################################


### gaussian family for continuous response----

## All variables plus interactions model selection
## carry out model selection with different link functions and compare for fit

### identity link function ----

#######################################################################################
#### FINAL MODEL FOR MODEL 3 ##########################################################
#######################################################################################

library(lme4)
global.model3a <- glmer(neg_sqrt_humidity ~                             # the dependent variable - neg. sqrt humidity
                        temperature_glasshouse_ibutton_01 +             # interaction terms
                        Experimental_group +                                     # fixed term
                        temperature_glasshouse_ibutton_01:Experimental_group +   # interaction terms
                        (1|experiment_day) + (1|hive),                  # the random terms 
                        family = gaussian (link = identity),
                        control=glmerControl(optimizer="Nelder_Mead",   # Nedler_Mead optimiser aided convergence    
                                             optCtrl=list(maxfun=2e5)), 
                        na.action = na.pass,
                        data = dframe2a)            # gaussian model of humidity data 

summary(global.model3a) 

library(arm)                                    
stdz.global.model3a <- standardize(global.model3a, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.global.model3a)

## assess the full global model to see whether it's worth dropping terms
library(MuMIn)
stdz.model.set3a <- dredge(global.model3a)
stdz.model.set3a           ## the full model comes out as the best

# R-squared 
library(MuMIn)
r.squaredGLMM(global.model3a)


## check residuals
plot(global.model3a, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals

sresid <- resid(global.model3a, type = "pearson")
hist(sresid)



### log link function ----

### doesn't converge even after altering optimizer

library(lme4)
global.model3b <- glmer(neg_sqrt_humidity ~                               # the dependent variable - neg. sqrt humidty
                          temperature_glasshouse_ibutton_01 +             # interaction terms
                          Experimental_group +                                     # fixed term
                          temperature_glasshouse_ibutton_01:Experimental_group +   # interaction terms
                          (1|experiment_day) + (1|hive),                  # the random terms 
                        family = gaussian (link = log),
                      #  control=glmerControl(optimizer="Nelder_Mead",    # try Nelder_Mead, nloptwrap and bobyqa optimisers  
                      #                      optCtrl=list(maxfun=2e5)), 
                        na.action = na.pass,
                        data = dframe2a) # gaussian model of temperature data 

summary(global.model3b) 


### inverse link function ----

### doesn't converge even after altering optimizer

library(lme4)
global.model3c <- glmer(neg_sqrt_humidity ~                               # the dependent variable - neg. sqrt humidty
                          temperature_glasshouse_ibutton_01 +             # interaction terms
                          Experimental_group +                                     # fixed term
                          temperature_glasshouse_ibutton_01:Experimental_group +   # interaction terms
                          (1|experiment_day) + (1|hive),                  # the random terms 
                        family = gaussian (link = inverse),
                        #  control=glmerControl(optimizer="Nelder_Mead",    # try Nelder_Mead, nloptwrap and bobyqa optimisers  
                        #                      optCtrl=list(maxfun=2e5)), 
                        na.action = na.pass,
                        data = dframe2a) # gaussian model of temperature data 

summary(global.model3c) 



### Gamma family for continuous response----


### identity link function ----


library(lme4)
global.model3d <- glmer(neg_sqrt_humidity ~                             # the dependent variable - neg. sqrt humidty
                          temperature_glasshouse_ibutton_01 +             # interaction terms
                          Experimental_group +                                     # fixed term
                          temperature_glasshouse_ibutton_01:Experimental_group +   # interaction terms
                          (1|experiment_day) + (1|hive),                  # the random terms 
                        family = Gamma (link = identity),                 
                        control=glmerControl(optimizer="Nelder_Mead",   # Nedler_Mead optimiser aided convergence    
                                             optCtrl=list(maxfun=2e5)), 
                        na.action = na.pass,
                        data = dframe2a) # Gamma model of temperature data 

summary(global.model3d) #the warning message here suggests we have a scaling issue, need to standardise variablies


library(arm)                                    
stdz.global.model3d <- standardize(global.model3d, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.global.model3d)

## assess the full global model to see whether it's worth dropping terms
library(MuMIn)
stdz.model.set3d <- dredge(global.model3d)
stdz.model.set3d           ## the full model comes out as the best

# R-squared 
library(rsq)
rsq.glmm(global.model3d)

## check residuals
plot(global.model3d, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals

sresid <- resid(global.model3d, type = "pearson")
hist(sresid)


### log link function ----

### doesn't converge even after altering optimizer

library(lme4)
global.model3e <- glmer(neg_sqrt_humidity ~                             # the dependent variable - neg. sqrt humidty
                          temperature_glasshouse_ibutton_01 +             # interaction terms
                          Experimental_group +                                     # fixed term
                          temperature_glasshouse_ibutton_01:Experimental_group +   # interaction terms
                          (1|experiment_day) + (1|hive),                  # the random terms 
                        family = Gamma (link = log),     
                        #   control=glmerControl(optimizer="bobyqa",       
                        #                     optCtrl=list(maxfun=3e5)),  # try Nelder_Mead, nloptwrap and bobyqa optimisers
                        # for full model to aid convergence
                        na.action = na.pass,
                        data = dframe2a) # Gamma model of temperature data 

summary(global.model3e) 


### doesn't converge even after altering optimizer

library(lme4)
global.model3f <- glmer(neg_sqrt_humidity ~                             # the dependent variable - neg. sqrt humidty
                          temperature_glasshouse_ibutton_01 +             # interaction terms
                          Experimental_group +                                     # fixed term
                          temperature_glasshouse_ibutton_01:Experimental_group +   # interaction terms
                          (1|experiment_day) + (1|hive),                  # the random terms 
                        family = Gamma (link = inverse),     
                        control=glmerControl(optimizer="nloptwrap",       
                                             optCtrl=list(maxfun=3e5)),    # try Nelder_Mead, nloptwrap and bobyqa optimisers
                        # for full model to aid convergence
                        na.action = na.pass,
                        data = dframe2a) # Gamma model of temperature data 

summary(global.model3f)  ### scaling issues!!!




### Model 3 - compare the top models and select the model with the best fit ----

stdz.model.set3a # Gaussian family, identity link
stdz.model.set3d # Gamma family, identity link

write.csv(stdz.model.set3a, file = "model_3_gaussian_identity.csv")
write.csv(stdz.model.set3d, file = "model_3_gamma_identity.csv")

