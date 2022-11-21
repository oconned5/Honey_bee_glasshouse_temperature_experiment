########################################################################################
# Model selection for glasshoue experiment invesitgating the impact of 
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
#install.packages("devtools")
#install.packages("glmmTMB")


### write function to calculate overdispersion

library(dplyr)
library(tidyr)
overdisp_fun <- function(model) {vpars <- function (m)
{nrow(m)*(nrow(m) + 1)/2}
model.df <- sum(sapply(VarCorr(model), vpars)) +
  length(fixef(model))
rdf <- nrow(model.frame(model)) -model.df
rp <- residuals(model, type="pearson")
Pearson.chisq <- sum(rp^2)
prat <- Pearson.chisq/rdf
pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
c(chisq=Pearson.chisq, ratio=prat, rdf=rdf, p=pval)}


#################################################################################


dframe1 <- read.csv(file.choose())    # Select "Glasshouse_experiment_database"
head(dframe1)
summary(dframe1)

dframe2 <- subset(dframe1, period == "experiment" & glasshouse_temperatue_setting == "day" &
                    bees_out != "NA" & temperature_glasshouse_ibutton_01 != "NA")


#################################################################

## Model 1----

# Relationship between worker movement and glasshouse temperature
# plus Replicate, and date, with hive as a random term

hist(dframe2$bees_out) 

#################################################################


### poisson family for count response----

## All variables plus interactions model selection
## carry out model selection with different link functions and compare for fit

### log link function

library(lme4)
global.model1a <- glmer(bees_out  ~                 # the dependent variable
                        temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                        date_julian + 
                        (1|hive),                # the random term 
                      na.action = na.pass,
                      family = "poisson" (link="log"), data = dframe2) # poisson model of count data 

summary(global.model1a) #the warning message here suggests we have a scaling issue, need to standardise variablies



# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.global.model1a <- standardize(global.model1a, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.global.model1a)


# R-squared 

library(MuMIn)
r.squaredGLMM(stdz.global.model1a)

## model validation

library(LMERConvenienceFunctions)
mcp.fnc(stdz.global.model1a) # plots of model fit
plot(stdz.global.model1a, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals


sresid <- resid(stdz.global.model1a, type = "pearson")
hist(sresid)

fits <- fitted(stdz.global.model1a)
plot(sresid ~ fits)


## overdispersion
overdisp_fun(stdz.global.model1a)

theta <- stdz.global.model1a$deviance / stdz.global.model1a$df.residual
theta

library(MuMIn)
model.set1a <- dredge(stdz.global.model1a)
model.set1a

### sqrt link function

library(lme4)
global.model1b <- glmer(bees_out  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                          date_julian + 
                          (1|hive),                # the random term 
                        na.action = na.pass,
                        family = "poisson" (link="sqrt"), data = dframe2) # poisson model of count data 

summary(global.model1b) #the warning message here suggests we have a scaling issue, need to standardise variablies



# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.global.model1b <- standardize(global.model1b, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.global.model1b)


# R-squared 

library(MuMIn)
r.squaredGLMM(stdz.global.model1b)

## model validation

library(LMERConvenienceFunctions)
mcp.fnc(stdz.global.model1b) # plots of model fit
plot(stdz.global.model1b, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals


sresid <- resid(stdz.global.model1b, type = "pearson")
hist(sresid)

fits <- fitted(stdz.global.model1b)
plot(sresid ~ fits)

## calculate overdispersion
overdisp_fun(stdz.global.model1b)

library(MuMIn)
model.set1b <- dredge(stdz.global.model1b)
model.set1b

### identity link function - fails 

library(lme4)
global.model1c <- glmer(bees_out  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                          date_julian + 
                          (1|hive),                # the random term 
                        na.action = na.pass,
                        family = "poisson" (link="identity"), data = dframe2) # poisson model of count data 

summary(global.model1c) 


### negative binomial family for count response----

## All variables plus interactions model selection
## carry out model selection with different link functions and compare for fit

### negative bionomial with lme4

library(lme4)
library(MASS)
global.model1d <- glmer.nb(bees_out  ~                 # the dependent variable
                             temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                             date_julian + 
                             (1|hive),                # the random term 
                           na.action = na.pass, nAGQ = 1, 
                           data = dframe2) # negative bionomial model of count data 

summary(global.model1d) #the warning message here suggests we have a scaling issue, need to standardise variablies



# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.global.model1d <- standardize(global.model1d, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.global.model1d)


# R-squared 

library(MuMIn)
r.squaredGLMM(stdz.global.model1d)

## model validation

library(LMERConvenienceFunctions)
mcp.fnc(stdz.global.model1d) # plots of model fit
plot(stdz.global.model1d, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals


sresid <- resid(stdz.global.model1d, type = "pearson")
hist(sresid)

fits <- fitted(stdz.global.model1d)
plot(sresid ~ fits)

## calculate overdispersion
overdisp_fun(stdz.global.model1d)

library(MuMIn)
model.set1d <- dredge(stdz.global.model1d)
model.set1d


### negative bionomial with lme4

#### set hive to be a factor
dframe2$hive <- as.factor(dframe2$hive)

library(glmmADMB)
global.model1e <- glmmadmb(bees_out  ~                 # the dependent variable
                             temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                             date_julian + 
                             (1|hive),                # the random term 
                           family = "nbinom", link = "log",
                           data = dframe2) # negative bionomial model of count data 

summary(global.model1e) #the warning message here suggests we have a scaling issue, need to standardise variablies



# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales


# R-squared 

library(rsq)
rsq.glmm(global.model1e)

## model validation

library(LMERConvenienceFunctions)
mcp.fnc(global.model1e) # plots of model fit
plot(global.model1e, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals


sresid <- resid(global.model1e, type = "pearson")
hist(sresid)

fits <- fitted(global.model1e)
plot(sresid ~ fits)

## calculate overdispersion
overdisp_fun(global.model1e)

library(MuMIn)
model.set1e <- dredge(global.model1e)
model.set1e





#################################################################

## Model 2----

# Relationship between hive temperature and glasshouse temperature
# plus Replicate, and date, with hive as a random term

hist(dframe2$temperature_C_hive_ibutton_01)  ## fairly normally distributed
hist(dframe2$temperature_glasshouse_ibutton_01) 


#################################################################


### gaussian family for continuous response----

## All variables plus interactions model selection
## carry out model selection with different link functions and compare for fit

### identity link function ----


library(lme4)
global.model2a <- glmer(temperature_C_hive_ibutton_01  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                          temperature_glasshouse_ibutton_01:Replicate +    # interaction terms
                          (1|experiment_day) + (1|hive),                # the random term 
                        family = gaussian (link = identity),
                        na.action = na.pass,
                        data = dframe2) # gaussian model of temperature data 

summary(global.model2a) 

# R-squared 

library(MuMIn)
r.squaredGLMM(global.model2a)


## model validation

library(LMERConvenienceFunctions)
mcp.fnc(global.model2a) # plots of model fit
plot(global.model2a, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals


sresid <- resid(global.model2a, type = "pearson")
hist(sresid)

fits <- fitted(global.model2a)
plot(sresid ~ fits)



## overdispersion
overdisp_fun(global.model2a)


library(MuMIn)
model.set2a <- dredge(global.model2a)
model.set2a


### log link function ----

### doesn't converge even after altering optimizer

library(lme4)
global.model2b <- glmer(temperature_C_hive_ibutton_01  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                          temperature_glasshouse_ibutton_01:Replicate +    # interaction terms
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
                          temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                          temperature_glasshouse_ibutton_01:Replicate +    # interaction terms
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
                          temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                          temperature_glasshouse_ibutton_01:Replicate +    # interaction terms
                          (1|experiment_day) + (1|hive),                # the random term 
                        family = Gamma (link = identity),
                        control=glmerControl(optimizer="bobyqa",       
                                             optCtrl=list(maxfun=2e5)), # bobyqa optimiser aids convergence
                        na.action = na.pass,
                        data = dframe2) # Gamma model of temperature data 

summary(global.model2d) 


# R-squared 

library(MuMIn)
r.squaredGLMM(global.model2d)
library(rsq)
rsq.glmm(global.model2d)

## model validation

library(LMERConvenienceFunctions)
mcp.fnc(global.model2d) # plots of model fit
plot(global.model2d, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals


sresid <- resid(global.model2d, type = "pearson")
hist(sresid)

fits <- fitted(global.model2d)
plot(sresid ~ fits)


## overdispersion
overdisp_fun(global.model2d)


library(MuMIn)
model.set2d <- dredge(global.model2d)
model.set2d

### log link function ----

### doesn't converge even after altering optimizer

library(lme4)
global.model2e <- glmer(temperature_C_hive_ibutton_01  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                          temperature_glasshouse_ibutton_01:Replicate +    # interaction terms
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
                          temperature_glasshouse_ibutton_01 + Replicate +  # fixed term
                          temperature_glasshouse_ibutton_01:Replicate +    # interaction terms
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
# plus Replicate, and date, with hive as a random term

hist(dframe2a$humidity_percentage_hive_ibutton_01)  ### long left-skewed tail
hist(dframe2a$temperature_glasshouse_ibutton_01) 

## explore low values
df_explore <- subset(dframe2, humidity_percentage_hive_ibutton_01 <50)
View(df_explore)                   ## all from one sensor, probably sensor failure                                               

dframe2a <- subset(dframe2, hive != "hive_02")

hist(dframe2a$humidity_percentage_hive_ibutton_01)  ## now a normal distribution
hist(dframe2a$temperature_glasshouse_ibutton_01) 




#################################################################


### gaussian family for continuous response----

## All variables plus interactions model selection
## carry out model selection with different link functions and compare for fit

### identity link function ----


library(lme4)
global.model3a <- glmer(humidity_percentage_hive_ibutton_01  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                          temperature_glasshouse_ibutton_01:Replicate +    # interaction terms
                          (1|experiment_day) + (1|hive),                # the random term 
                        family = gaussian (link = identity),
                        na.action = na.pass,
                        data = dframe2a) # gaussian model of temperature data 

summary(global.model3a) 


# R-squared 

library(MuMIn)
r.squaredGLMM(global.model3a)


## model validation

library(LMERConvenienceFunctions)
mcp.fnc(global.model3a) # plots of model fit
plot(global.model3a, pch = 30, col = "black", lty = "dotted") # fitted values against the residuals


sresid <- resid(global.model3a, type = "pearson")
hist(sresid)

fits <- fitted(global.model3a)
plot(sresid ~ fits)



## overdispersion
overdisp_fun(global.model3a)


library(MuMIn)
model.set3a <- dredge(global.model3a)
model.set3a


### log link function ----

### doesn't converge even after altering optimizer

library(lme4)
global.model3b <- glmer(humidity_percentage_hive_ibutton_01  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                          temperature_glasshouse_ibutton_01:Replicate +    # interaction terms
                          (1|experiment_day) + (1|hive),                # the random term 
                        family = gaussian (link = log),
                        #  control=glmerControl(optimizer="bobyqa",       
                        #                      optCtrl=list(maxfun=3e5)), # try Nelder_Mead, nloptwrap and bobyqa optimisers
                        # for full model to aid convergence
                        na.action = na.pass,
                        data = dframe2a) # gaussian model of temperature data 

summary(global.model3b) 



### inverse link function ----

### doesn't converge even after altering optimizer

library(lme4)
global.model3c <- glmer(humidity_percentage_hive_ibutton_01  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                          temperature_glasshouse_ibutton_01:Replicate +    # interaction terms
                          (1|experiment_day) + (1|hive),                # the random term 
                        family = gaussian (link = "inverse"),
                        #  control=glmerControl(optimizer="bobyqa",       
                        #                      optCtrl=list(maxfun=3e5)), # try Nelder_Mead, nloptwrap and bobyqa optimisers
                        # for full model to aid convergence
                        na.action = na.pass,
                        data = dframe2a) # gaussian model of temperature data 

summary(global.model3c) 



### Gamma family for continuous response----


### identity link function ----


library(lme4)
global.model3d <- glmer(humidity_percentage_hive_ibutton_01  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                          temperature_glasshouse_ibutton_01:Replicate +    # interaction terms
                          (1|experiment_day) + (1|hive),                # the random term 
                        family = Gamma (link = identity),
                        control=glmerControl(optimizer="bobyqa",       
                                             optCtrl=list(maxfun=3e5)), #  bobyqa optimiser aids convergence
                        na.action = na.pass,
                        data = dframe2a) # Gamma model of temperature data 

summary(global.model3d) 


# R-squared 

library(MuMIn)
r.squaredGLMM(global.model3d)
library(rsq)
rsq.glmm(global.model3d)

## model validation

library(LMERConvenienceFunctions)
mcp.fnc(global.model3d) # plots of model fit
plot(global.model3d, pch = 30, col = "black", lty = "dotted") # fitted values against the residuals


sresid <- resid(global.model3d, type = "pearson")
hist(sresid)

fits <- fitted(global.model3d)
plot(sresid ~ fits)


## overdispersion
overdisp_fun(global.model3d)


library(MuMIn)
model.set3d <- dredge(global.model3d)
model.set3d

### log link function ----

### doesn't converge even after altering optimizer

library(lme4)
global.model3e <- glmer(humidity_percentage_hive_ibutton_01  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                          temperature_glasshouse_ibutton_01:Replicate +    # interaction terms
                          (1|experiment_day) + (1|hive),                # the random term 
                        family = Gamma (link = log),
                        #   control=glmerControl(optimizer="bobyqa",       
                        #                     optCtrl=list(maxfun=3e5)),  # try Nelder_Mead, nloptwrap and bobyqa optimisers
                        # for full model to aid convergence
                        na.action = na.pass,
                        data = dframe2a) # Gamma model of temperature data 

summary(global.model3e) 



library(lme4)
global.model3f <- glmer(humidity_percentage_hive_ibutton_01  ~                   # the dependent variable
                          temperature_glasshouse_ibutton_01 + Replicate +  # fixed term
                          temperature_glasshouse_ibutton_01:Replicate +    # interaction terms
                          (1|experiment_day) + (1|hive),                   # the random term 
                        family = Gamma (link = "inverse"),
                        control=glmerControl(optimizer="nloptwrap",       
                                             optCtrl=list(maxfun=3e5)),    # try Nelder_Mead, nloptwrap and bobyqa optimisers
                        # for full model to aid convergence
                        na.action = na.pass,
                        data = dframe2a) # Gamma model of temperature data 

summary(global.model3f) 



### Model 3 - compare the top models and select the model with the best fit ----

model.set3a # Gaussian family, identity link
model.set3d # Gamma family, identity link

write.csv(model.set3a, file = "model_3_gaussian_identity.csv")
write.csv(model.set3d, file = "model_3_gamma_identity.csv")

