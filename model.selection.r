########################################################################################
# Model selection for glasshouse experiment investigating the impact of 
# temperature on honey bee physiology


# Housekeeping

rm(list=ls()) # remove everything currently held in the R memory

graphics.off() # close all open graphics windows 

# Packages that may be needed, remove the # to install

#install.packages("glmmTMB")
#install.packages("bbmle")
#install.packages("MuMIn")
#install.packages("LMERConvenienceFunctions")
#install.packages("rsq")
#install.packages("MASS")
#install.packages("performance")

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

# Relationship between worker movement, glasshouse temperature and days spent in glasshouse
# plus Experimental_group as a fixed effect, with hive ID and the treatment temperature from the previous day as random terms

# examine the response variable
hist(dframe2$bees_out) ## count data with a poisson or negative binomial style distribution

# examine the continuous explanatory variable 
hist(dframe2$temperature_glasshouse_ibutton_01)  # quite normally distributed


#################################################################


### poisson family for count response----

## All variables plus interactions model selection

library(glmmTMB)
library(bbmle) 
global.model1a <- glmmTMB(bees_out  ~                          # count dependent variable
                          Experimental_group +                 # categorical variable   
                          temperature_glasshouse_ibutton_01 +  # continuous explanatory variable
                          days_in_glasshouse +                 # continuous explanatory variable
                          temperature_glasshouse_ibutton_01:days_in_glasshouse  # interaction term 
                        + (1|hive) + (1|previous_day_treatment_temperature_C),  # the random terms
                        family = "poisson", na.action = "na.pass", # poisson model of count data 
                        data = dframe2)                       
summary(global.model1a)


## assess the full global model to see whether it's worth dropping terms
library(MuMIn)
model.set1a <- dredge(global.model1a)
model.set1a    ## the full model comes out as the best

library(data.table)
fwrite(model.set1a, file = "model_1a.csv")

# R-squared 
library(MuMIn)
r.squaredGLMM(global.model1a)

## check residuals
sresid <- resid(global.model1a, type = "pearson")
hist(sresid)

library(MASS)
library(performance)

## overdispersion
check_overdispersion(global.model1a)   ### this mode has overdispersion >2, try another model


#######################################################################################
#### FINAL MODEL FOR MODEL 1 ##########################################################
#######################################################################################

### negative binomial family for count response using nbinom1 implementation (linear parameterization)----

## All variables plus interactions model selection

library(glmmTMB)
library(bbmle) 
global.model1b <- glmmTMB(bees_out  ~                          # count dependent variable
                            Experimental_group +                 # categorical variable   
                            temperature_glasshouse_ibutton_01 +  # continuous explanatory variable
                            days_in_glasshouse +                 # continuous explanatory variable
                            temperature_glasshouse_ibutton_01:days_in_glasshouse  # interaction term 
                          + (1|hive) + (1|previous_day_treatment_temperature_C),  # the random terms
                          family = "nbinom1", na.action = "na.pass", # negative binomial model of count data 
                          data = dframe2)                       
summary(global.model1b)


## assess the full global model to see whether it's worth dropping terms
library(MuMIn)
model.set1b <- dredge(global.model1b)
model.set1b    ## the full model comes out as the best

library(data.table)
fwrite(model.set1b, file = "model_1b.csv")

# R-squared 
library(MuMIn)
r.squaredGLMM(global.model1b)

## check residuals
sresid <- resid(global.model1b, type = "pearson")
hist(sresid)

library(MASS)
library(performance)

## overdispersion
check_overdispersion(global.model1b)   ### overdispersion fixed!


#######################################################################################
#######################################################################################


### negative binomial family for count response using nbinom2 implementation (quadratic parameterization)----

## All variables plus interactions model selection

library(glmmTMB)
library(bbmle) 
global.model1c <- glmmTMB(bees_out  ~                          # count dependent variable
                            Experimental_group +                 # categorical variable   
                            temperature_glasshouse_ibutton_01 +  # continuous explanatory variable
                            days_in_glasshouse +                 # continuous explanatory variable
                            temperature_glasshouse_ibutton_01:days_in_glasshouse  # interaction term 
                          + (1|hive) + (1|previous_day_treatment_temperature_C),  # the random terms
                          family = "nbinom2", na.action = "na.pass", # negative binomial model of count data 
                          data = dframe2)                       
summary(global.model1c)


## assess the full global model to see whether it's worth dropping terms
library(MuMIn)
model.set1c <- dredge(global.model1c)
model.set1c    ## the full model comes out as the best

library(data.table)
fwrite(model.set1c, file = "model_1c.csv")


# R-squared 
library(MuMIn)
r.squaredGLMM(global.model1c)

## check residuals
sresid <- resid(global.model1c, type = "pearson")
hist(sresid)

library(MASS)
library(performance)

## overdispersion
check_overdispersion(global.model1c)   ### overdispersion fixed!



#################################################################

## Model 2----

# Relationship between hive temperature, glasshouse temperature and days spent in glasshouse
# plus Experimental_group as a fixed effect, with hive ID and the treatment temperature from the previous day as random terms
## investigate distribution of response variable
hist(dframe2$temperature_C_hive_ibutton_01)  ## fairly normally distributed


#################################################################

#######################################################################################
#### FINAL MODEL FOR MODEL 2 ##########################################################
#######################################################################################

### gaussian family for continuous response----

## All variables plus interactions model selection


library(glmmTMB)
library(bbmle) 
global.model2a <- glmmTMB (temperature_C_hive_ibutton_01  ~                 # the continuous dependent variable
                           Experimental_group +                           # categorical variable  
                           temperature_glasshouse_ibutton_01 +            # continuous explanatory variable
                           days_in_glasshouse +                           # continuous explanatory variable
                           temperature_glasshouse_ibutton_01:days_in_glasshouse  # interaction term
                           + (1|hive)+ (1|previous_day_treatment_temperature_C),  # the random terms
                         family = "gaussian", na.action = "na.pass",      # gaussian model for continuous variable
                         data = dframe2)            
summary(global.model2a) 


## assess the full global model to see whether it's worth dropping terms
library(MuMIn)
model.set2a <- dredge(global.model2a)
model.set2a              ## the full model comes out as the best

library(data.table)
fwrite(model.set2a, file = "model_2a.csv")

# R-squared 
library(MuMIn)
r.squaredGLMM(global.model2a)


sresid <- resid(global.model2a, type = "pearson")
hist(sresid)

#######################################################################################
#######################################################################################


### gamma family for continuous response----

## All variables plus interactions model selection

### doesn't converge even after altering optimizer


library(glmmTMB)
library(bbmle) 
global.model2b <- glmmTMB (temperature_C_hive_ibutton_01  ~                 # the continuous dependent variable
                             Experimental_group +                           # categorical variable  
                             temperature_glasshouse_ibutton_01 +            # continuous explanatory variable
                             days_in_glasshouse +                           # continuous explanatory variable
                             temperature_glasshouse_ibutton_01:days_in_glasshouse  # interaction term
                           + (1|hive)+ (1|previous_day_treatment_temperature_C),  # the random terms 
                           family = "Gamma", na.action = "na.pass",      # gamma model for continuous variable
                           data = dframe2)            
summary(global.model2b) 


#################################################################

## Model 3----

# Relationship between hive humidity, glasshouse temperature and days in the glasshouse
# plus Experimental_group as a fixed effect, with hive ID and the treatment temperature from the previous day as random terms

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


#######################################################################################
#### FINAL MODEL FOR MODEL 3 - full model minus the Experimental Group variable #######
#######################################################################################


library("glmmTMB")
library("bbmle") 
global.model3a <- glmmTMB(neg_sqrt_humidity ~                          # the dependent variable - neg. sqrt humidity
                           Experimental_group +                        # categorical variable 
                           temperature_glasshouse_ibutton_01 +         # continuous explanatory variable
                           days_in_glasshouse +                        # continuous explanatory variable
                           temperature_glasshouse_ibutton_01:days_in_glasshouse  # interaction term
                         + (1|hive) + (1|previous_day_treatment_temperature_C),  # the random terms
                         family = "gaussian", na.action = "na.pass", 
                         data = dframe2a)            # gaussian model of humidity data 
summary(global.model3a)  


## assess the full global model to see whether it's worth dropping terms
library(MuMIn)
model.set3a <- dredge(global.model3a)
model.set3a           ## the model removing Experimental group is the best

library(data.table)
fwrite(model.set3a, file = "model_3a.csv")

# R-squared 
library(MuMIn)
r.squaredGLMM(global.model3a)

sresid <- resid(global.model3a, type = "pearson")
hist(sresid)


### gamma family for continuous response----

## All variables plus interactions model selection

### doesn't converge even after altering optimizer


library("glmmTMB")
library("bbmle") 
global.model3b <- glmmTMB(neg_sqrt_humidity ~                          # the dependent variable - neg. sqrt humidity
                            Experimental_group +                        # categorical variable 
                            temperature_glasshouse_ibutton_01 +         # continuous explanatory variable
                            days_in_glasshouse +                        # continuous explanatory variable
                            temperature_glasshouse_ibutton_01:days_in_glasshouse  # interaction term
                          + (1|hive) + (1|previous_day_treatment_temperature_C),  # the random terms
                          family = "Gamma", na.action = "na.pass", 
                          data = dframe2a)            # gamma model of humidity data 
summary(global.model3b) 



