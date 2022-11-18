########################################################################################
# Model selection for glasshoue experiment invesitgating the impact of 
# temperature on honey bee physiology


# Housekeeping

rm(list=ls()) # remove everything currently held in the R memory

graphics.off() # close all open graphics windows 

# Packages that may be needed, remove the # to install

#install.packages("arm")
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("MuMIn")
#install.packages("car")
#install.packages("LMERConvenienceFunctions")
#install.packages("ggplot2")
#install.packages("effects")
#install.packages("sjPlot")
#install.packages("tab")
#install.packages("rsq")
#install.packages("MASS")
install.packages("dplyr")
install.packages("tidyr")

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
global.model1a <- glmer(bees_out  ~                 # the dependent vaiable
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

## calculate overdispersion
# write function
overdisp_fun <- function(model) {vpars <- function (m)
                {nrom(m)*(nrow(m) + 1)/2}
model.df <- sum(sappply(VarCorr(model), vpars)) +
            length(fixef(model))
rdf <- nrow(model.frame(model)) -model.df
rp <- residuals(model, type="person")
Pearson.chisq <- sum(rp^2)
prat <- Pearson.chisq/rdf
pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
c(chisq=Pearson.chisq, ratio=prat, rdf=rdf, p=pval)}

## overdispersion
overdisp_fun(stdz.global.model1a)

library(MuMIn)
model.set1a <- dredge(stdz.global.model1a)
model.set1a

