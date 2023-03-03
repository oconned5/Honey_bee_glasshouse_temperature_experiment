########################################################################################
# Analyses of glasshouse experiment data, measuring the effect of temperature 
# on honey bee movement and brood maintenance


# Housekeeping

rm(list=ls()) # remove everything currently held in the R memory

graphics.off() # close all open graphics windows 

# Packages that may be needed, remove the # to install

#install.packages("arm")
#install.packages("lme4")
#install.packages("MuMIn")
#install.packages("car")
#install.packages("ggplot2")
#install.packages("sjPlot")
#install.packages("tab")
#install.packages("MASS")
#install.packages("LMERConvenienceFunctions")


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
# plus Replicate as a fixed effect, with hive and experimental day as random terms

library(lme4)
library(MASS)
final.model1 <- glmer.nb(bees_out  ~                            # count dependent variable
                             temperature_glasshouse_ibutton_01 +  # continuous explanatory variable
                             Replicate +                          # fixed term
                             temperature_glasshouse_ibutton_01:Replicate + # interaction term 
                             (1|experiment_day) + (1|hive),   # the random terms - hive ID and experiment day
                           na.action = na.pass, nAGQ = 1, 
                           data = dframe2)                       # negative bionomial model of count data 

summary(final.model1) #the warning message here suggests we have a scaling issue, need to standardise variablies

# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.final.model1 <- standardize(final.model1, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.final.model1)

# R-squared 

library(MuMIn)
r.squaredGLMM(stdz.final.model1)


## summary table
library(sjPlot)
tab_model(stdz.final.model1, show.est = TRUE, show.se = TRUE, show.stat = TRUE)

## check residuals
plot(stdz.final.model1, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals

sresid <- resid(stdz.final.model1, type = "pearson")
hist(sresid)

### description of final model paramters
print(stdz.final.model1, corr = F)

library(LMERConvenienceFunctions)
plotLMER.fnc(stdz.final.model1) # influence of individual explanatory variables


## Figures model 1 ----

library(ggplot2)
library(sjPlot)

## Plotting model 2 output

dev.new()

ww1 <- theme_set(theme_bw())
ww2 <- plot_model(final.model1, type = "pred", terms = c("temperature_glasshouse_ibutton_01 [all]", "Replicate"))
ww3 <- ww2 + labs(color = "Replicate")
ww4 <- ww3 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=18), #change legend title font size
                   legend.text = element_text(size=10)) 
ww5 <- ww4 + geom_line(size = 2)
ww6 <- ww5 + ylab("Worker movement")
ww7 <- ww6 + xlab("Hive temperature (°C)")
ww10 <- ww7 + theme(axis.title.y=element_text(face="bold", size=18, vjust=1.5))
ww11 <- ww10 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
ww12 <- ww11 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
ww13 <- ww12 + theme(plot.title = element_text(color="black", size=18, face="bold"))
ww14 <- ww13 + ggtitle("") ## blanking top title
ww15 <- ww14 + theme(strip.text.x = element_text(face="bold", size=15, colour = "black"))
ww15

### exporting high res image
# PDF
pdf(file = "Figure_1_Worker_movement.pdf", width = 10, height = 6, family = "Helvetica")
ww15
dev.off()

# TIFF
tiff("Figure_1_Worker_movement.tiff", height = 12, width = 21, units = 'cm', res = 300)
ww15
dev.off()


#################################################################

## Model 2----

# Relationship between hive temperature and glasshouse temperature
# plus Replicate as a fixed effect, with hive and experimental day as random terms

library(lme4)
final.model2 <- glmer(temperature_C_hive_ibutton_01  ~                 # the dependent variable
                          temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                          temperature_glasshouse_ibutton_01:Replicate +    # interaction terms
                          (1|experiment_day) + (1|hive),                   # the random terms 
                        family = gaussian (link = identity),
                        na.action = na.pass,
                        data = dframe2) # gaussian model of temperature data 

summary(final.model2) 


library(arm)                                    
stdz.final.model2 <- standardize(final.model2, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.final.model2)


# R-squared 
library(MuMIn)
r.squaredGLMM(stdz.final.model2)

## summary table
library(sjPlot)
tab_model(stdz.final.model2, show.est = TRUE, show.se = TRUE, show.stat = TRUE)


## inspect residuals
plot(stdz.final.model2, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals

sresid <- resid(stdz.final.model2, type = "pearson")
hist(sresid)

### description of final model paramters
print(stdz.final.model2, corr = F)

library(LMERConvenienceFunctions)
plotLMER.fnc(stdz.final.model2) # influence of individual explanatory variables


## Figures model 2 ----

library(ggplot2)
library(sjPlot)

## Plotting model 2 output

zz1 <- theme_set(theme_bw())
zz2 <- plot_model(final.model2, type = "pred", terms = c("temperature_glasshouse_ibutton_01 [all]", "Replicate"))
zz3 <- zz2 + labs(color = "Replicate")
zz4 <- zz3 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=18), #change legend title font size
                   legend.text = element_text(size=10)) 
zz5 <- zz4 + geom_line(size = 2)
zz6 <- zz5 + ylab("Hive temperature (°C)")
zz7 <- zz6 + xlab("Glasshouse temperature (°C)")
zz10 <- zz7 + theme(axis.title.y=element_text(face="bold", size=18, vjust=1.5))
zz11 <- zz10 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
zz12 <- zz11 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
zz13 <- zz12 + theme(plot.title = element_text(color="black", size=18, face="bold"))
zz14 <- zz13 + ggtitle("") ## blanking top title
zz15 <- zz14 + theme(strip.text.x = element_text(face="bold", size=15, colour = "black"))
zz15

### exporting high res image
# PDF
pdf(file = "Figure_2_Hive_temperature.pdf", width = 10, height = 6, family = "Helvetica")
zz15
dev.off()

# TIFF
tiff("Figure_2_Hive_temperature.tiff", height = 12, width = 21, units = 'cm', res = 300)

zz15
dev.off()




## Model 3----

# Relationship between hive humidity and glasshouse temperature
# plus Replicate as a fixed effect, with hive and experimental day as random terms

## exclude hive_02 as it's humidity readings don't seem to have worked
dframe2a <- subset(dframe2, hive != "hive_02")
## try a negative square root transformation f to reduce the negative skew in the humidity data
neg_sqrt_humidity <- sqrt(max(dframe2a$humidity_percentage_hive_ibutton_01+1) - dframe2a$humidity_percentage_hive_ibutton_01) 


library(lme4)
final.model3 <- glmer(neg_sqrt_humidity ~                             # the dependent variable - neg. sqrt humidity
                          temperature_glasshouse_ibutton_01 +             # interaction terms
                          Replicate +                                     # fixed term
                          temperature_glasshouse_ibutton_01:Replicate +   # interaction terms
                          (1|experiment_day) + (1|hive),                  # the random terms 
                        family = gaussian (link = identity),
                        control=glmerControl(optimizer="Nelder_Mead",   # Nedler_Mead optimiser aided convergence    
                                             optCtrl=list(maxfun=2e5)), 
                        na.action = na.pass,
                        data = dframe2a)            # gaussian model of humidity data 

summary(final.model3)  #the warning message here suggests we have a scaling issue, need to standardise variablies

# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.final.model3 <- standardize(final.model3, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.final.model3)


# R-squared 
library(MuMIn)
r.squaredGLMM(final.model3)

## summary table
library(sjPlot)
tab_model(stdz.final.model3, show.est = TRUE, show.se = TRUE, show.stat = TRUE)

## check residuals
plot(final.model3, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals

sresid <- resid(final.model3, type = "pearson")
hist(sresid)

### description of final model paramters
print(stdz.final.model3, corr = F)

library(LMERConvenienceFunctions)
plotLMER.fnc(stdz.final.model3) # influence of individual explanatory variables


## Figures model 3 ----

library(ggplot2)
library(sjPlot)



## Plotting model 3 output

ff1 <- theme_set(theme_bw())
ff2 <- plot_model(final.model3, type = "pred", terms = c("temperature_glasshouse_ibutton_01 [all]", "Replicate"))
ff3 <- ff2 + labs(color = "Replicate")
ff4 <- ff3 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=18), #change legend title font size
                   legend.text = element_text(size=10)) 
ff5 <- ff4 + geom_line(size = 2)
ff6 <- ff5 + ylab("Percentage humidity (negative sqrt transformation)")
ff7 <- ff6 + xlab("Glasshouse temperature (°C)")
ff10 <- ff7 + theme(axis.title.y=element_text(face="bold", size=17, vjust=1.5))
ff11 <- ff10 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
ff12 <- ff11 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
ff13 <- ff12 + theme(plot.title = element_text(color="black", size=18, face="bold"))
ff14 <- ff13 + ggtitle("") ## blanking top title
ff15 <- ff14 + theme(strip.text.x = element_text(face="bold", size=15, colour = "black"))
ff15

### exporting high res image
# PDF
pdf(file = "Figure_3_Humidity.pdf", width = 10, height = 6, family = "Helvetica")
ff15
dev.off()

# TIFF
tiff("Figure_3_Humidity.tiff", height = 12, width = 21, units = 'cm', res = 300)
ff10 <- ff7 + theme(axis.title.y=element_text(face="bold", size=12, vjust=1.5))
ff11 <- ff10 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
ff12 <- ff11 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
ff13 <- ff12 + theme(plot.title = element_text(color="black", size=18, face="bold"))
ff14 <- ff13 + ggtitle("") ## blanking top title
ff15 <- ff14 + theme(strip.text.x = element_text(face="bold", size=15, colour = "black"))
ff15
dev.off()

