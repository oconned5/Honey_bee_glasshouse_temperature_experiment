########################################################################################
# Analyses of glasshouse experiment data, measuring the effect of temperature 
# on honey bee movement and brood maintenance


# Housekeeping

rm(list=ls()) # remove everything currently held in the R memory

graphics.off() # close all open graphics windows 

# Packages that may be needed, remove the # to install

#install.packages("glmmTMB")
#install.packages("bbmle")
#install.packages("ggplot2")
#install.packages("sjPlot")
#install.packages("effects")
#install.packages("MuMIn")


#################################################################################

## read in the data
dframe1 <- read.csv(file.choose())    # Select "Honey_bee_glasshouse_temperature_experiment"
head(dframe1)
summary(dframe1)

## subset out the experimental period, during the day, when data was recorded
dframe2 <- subset(dframe1, period == "experiment" & glasshouse_temperatue_setting == "day" &
                    bees_out != "NA" & temperature_glasshouse_ibutton_01 != "NA")


#################################################################

#### bees moving into the hive and bees moving out highly correlated,
#### just use one to represent bee movement
cor.test (dframe2$bees_out, dframe2$bees_in)

#############################
####  Model 1 ###############
#############################

# Relationship between worker movement, days spent in the glasshouse and glasshouse temperature
# plus Experimental_group as a fixed effect, with hive ID and the treatment temperature from the previous day as random terms

library(glmmTMB)
library(bbmle) 
final.model1 <- glmmTMB(bees_out  ~                          # count dependent variable
                            Experimental_group +                 # categorical variable   
                            temperature_glasshouse_ibutton_01 +  # continuous explanatory variable
                            days_in_glasshouse +                 # continuous explanatory variable
                            temperature_glasshouse_ibutton_01:days_in_glasshouse  # interaction term 
                          + (1|hive) + (1|previous_day_treatment_temperature_C),  # the random terms
                          family = "nbinom1", na.action = "na.pass", # negative binomial model of count data 
                          data = dframe2)                       
summary(final.model1)

# R-squared 

library(MuMIn)
r.squaredGLMM(final.model1)


## summary table
library(sjPlot)
tab_model(final.model1, show.est = TRUE, show.se = TRUE, show.stat = TRUE)

## check residuals
sresid <- resid(final.model1, type = "pearson")
hist(sresid)

### description of final model paramters
print(final.model1, corr = F)


#############################
## Plotting model 1 output
#############################


library(ggplot2)
library(sjPlot)
library(effects)

### the modelled impact of temperature on worker movement

ww1 <- theme_set(theme_bw())
ww2 <- plot_model(final.model1, type = "pred", terms = c("temperature_glasshouse_ibutton_01 [all]", "Experimental_group"))
ww3 <- ww2 + labs(color = "Experimental group")
ww4 <- ww3 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=15), #change legend title font size
                   legend.text = element_text(size=10)) 
ww5 <- ww4 + geom_line(linewidth = 2)
ww6 <- ww5 + ylab("Worker movement")
ww7 <- ww6 + xlab("Glasshouse temperature (°C)")
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


### the modeled impact of number of days in the glasshouse on worker movement

qqq1 <- theme_set(theme_bw())
qqq2 <- plot_model(final.model1, type = "pred", terms = c("days_in_glasshouse [all]", "Experimental_group"))
qqq3 <- qqq2 + labs(color = "Experimental group")
qqq4 <- qqq3 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                     legend.key.height = unit(1, 'cm'), #change legend key height
                     legend.key.width = unit(1, 'cm'), #change legend key width
                     legend.title = element_text(size=15), #change legend title font size
                     legend.text = element_text(size=10)) 
qqq5 <- qqq4 + geom_line(linewidth = 2)
qqq6 <- qqq5 + ylab("Worker movement")
qqq7 <- qqq6 + xlab("Number of days in the glasshouse")
qqq10 <- qqq7 + theme(axis.title.y=element_text(face="bold", size=18, vjust=1.5))
qqq11 <- qqq10 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
qqq12 <- qqq11 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
qqq13 <- qqq12 + theme(plot.title = element_text(color="black", size=18, face="bold"))
qqq14 <- qqq13 + ggtitle("") ## blanking top title
qqq15 <- qqq14 + theme(strip.text.x = element_text(face="bold", size=15, colour = "black"))
qqq15


### exporting high res image
# PDF
pdf(file = "Figure_S8_Worker_movement_vs_days.pdf", width = 10, height = 6, family = "Helvetica")
qqq15
dev.off()

# TIFF
tiff("Figure_S8_Worker_movement_vs_days.tiff", height = 12, width = 21, units = 'cm', res = 300)
qqq15
dev.off()


### the modeled impact on bee worker movement of the interaction between 
### glasshouse temperature and the number of days in the glasshouse

ppp1 <- theme_set(theme_bw())
ppp2 <- plot_model(final.model1, type = "int")
ppp3 <- ppp2 + labs(color = "Days in glasshouse")
ppp4 <- ppp3 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                     legend.key.height = unit(1, 'cm'), #change legend key height
                     legend.key.width = unit(1, 'cm'), #change legend key width
                     legend.title = element_text(size=15), #change legend title font size
                     legend.text = element_text(size=10)) 
ppp5 <- ppp4 + geom_line(linewidth = 2)
ppp6 <- ppp5 + ylab("Worker movement")
ppp7 <- ppp6 + xlab("Glasshouse temperature (°C)")
ppp10 <- ppp7 + theme(axis.title.y=element_text(face="bold", size=18, vjust=1.5))
ppp11 <- ppp10 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
ppp12 <- ppp11 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
ppp13 <- ppp12 + theme(plot.title = element_text(color="black", size=18, face="bold"))
ppp14 <- ppp13 + ggtitle("") ## blanking top title
ppp15 <- ppp14 + theme(strip.text.x = element_text(face="bold", size=15, colour = "black"))
ppp15


### exporting high res image
# PDF
pdf(file = "Figure_S9_Worker_movement_vs_interaction.pdf", width = 10, height = 6, family = "Helvetica")
ppp15
dev.off()

# TIFF
tiff("Figure_S9_Worker_movement_vs_interaction.tiff", height = 12, width = 21, units = 'cm', res = 300)
ppp15
dev.off()



#############################
####  Model 2 ###############
#############################

# Relationship between colony brood temperature, days spent in the glasshouse and glasshouse temperature
# plus Experimental_group as a fixed effect, with hive ID and the treatment temperature from the previous day as random terms

library(glmmTMB)
library(bbmle) 
final.model2 <- glmmTMB (temperature_C_hive_ibutton_01  ~                 # the continuous dependent variable
                             Experimental_group +                           # categorical variable  
                             temperature_glasshouse_ibutton_01 +            # continuous explanatory variable
                             days_in_glasshouse +                           # continuous explanatory variable
                             temperature_glasshouse_ibutton_01:days_in_glasshouse  # interaction term
                           + (1|hive) + (1|previous_day_treatment_temperature_C),  # the random terms
                           family = "gaussian", na.action = "na.pass",      # gaussian model for continuous variable
                           data = dframe2)            
summary(final.model2) 


# R-squared 
library(MuMIn)
r.squaredGLMM(final.model2)

## summary table
library(sjPlot)
tab_model(final.model2, show.est = TRUE, show.se = TRUE, show.stat = TRUE)


sresid <- resid(final.model2, type = "pearson")
hist(sresid)

### description of final model parameters
print(final.model2, corr = F)



#############################
## Plotting model 2 output
#############################

library(ggplot2)
library(sjPlot)
library(effects)


### the modeled impact on colony brood temperature of the interaction between 
### glasshouse temperature and the number of days in the glasshouse

vvv1 <- theme_set(theme_bw())
vvv2 <- plot_model(final.model2, type = "int")
vvv3 <- vvv2 + labs(color = "Days in glasshouse")
vvv4 <- vvv3 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                     legend.key.height = unit(1, 'cm'), #change legend key height
                     legend.key.width = unit(1, 'cm'), #change legend key width
                     legend.title = element_text(size=15), #change legend title font size
                     legend.text = element_text(size=10)) 
vvv5 <- vvv4 + geom_line(linewidth = 2)
vvv6 <- vvv5 + ylab("Colony brood temperature (°C)")
vvv7 <- vvv6 + xlab("Glasshouse temperature (°C)")
vvv10 <- vvv7 + theme(axis.title.y=element_text(face="bold", size=18, vjust=1.5))
vvv11 <- vvv10 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
vvv12 <- vvv11 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
vvv13 <- vvv12 + theme(plot.title = element_text(color="black", size=18, face="bold"))
vvv14 <- vvv13 + ggtitle("") ## blanking top title
vvv15 <- vvv14 + theme(strip.text.x = element_text(face="bold", size=15, colour = "black"))
vvv15

### exporting high res image
# PDF
pdf(file = "Figure_2_Brood_temperature.pdf", width = 10, height = 6, family = "Helvetica")
vvv15
dev.off()

# TIFF
tiff("Figure_2_Brood_temperature.tiff", height = 12, width = 21, units = 'cm', res = 300)
vvv15
dev.off()



### the modeled impact of glasshouse temperature on colony brood temperature

zz1 <- theme_set(theme_bw())
zz2 <- plot_model(final.model2, type = "pred", terms = c("temperature_glasshouse_ibutton_01 [all]", "Experimental_group"))
zz3 <- zz2 + labs(color = "Experimental group")
zz4 <- zz3 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=15), #change legend title font size
                   legend.text = element_text(size=10)) 
zz5 <- zz4 + geom_line(linewidth = 2)
zz6 <- zz5 + ylab("Colony brood temperature (°C)")
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
pdf(file = "Figure_S11_Brood_temperature_vs_glasshouse_temp.pdf", width = 10, height = 6, family = "Helvetica")
zz15
dev.off()

# TIFF
tiff("Figure_S11_Brood_temperature_vs_glasshouse_temp.tiff", height = 12, width = 21, units = 'cm', res = 300)

zz15
dev.off()



### the modeled impact on colony brood temperature of the interaction between 
### glasshouse temperature and the number of days in the glasshouse, displaying all days



lll1 <- plot(predictorEffect("temperature_glasshouse_ibutton_01", final.model2),
        type = "response", 
        xlab="Glasshouse temperature (°C)", ylab="Colony brood temperature (°C)", 
        main="The interaction between glasshouse temperature and number of days spent in the glasshouse")
lll1

### exporting high res image
# PDF
pdf(file = "Figure_S12_Brood_temperature_vs_temperature_days.pdf", width = 15, height = 6, family = "Helvetica")
lll1
dev.off()

# TIFF
tiff("Figure_S12_Brood_temperature_vs_temperature_days.tiff", height = 12, width = 30, units = 'cm', res = 300)
lll1
dev.off()



#############################
####  Model 3 ###############
#############################

# Relationship between hive humidity and glasshouse temperature
# plus Experimental_group as a fixed effect, with hive ID and the treatment temperature from the previous day as random terms

## exclude hive_02 as it's humidity readings don't seem to have worked
dframe2a <- subset(dframe2, hive != "hive_02")
## implement a negative square root transformation to reduce the negative skew in the humidity data
neg_sqrt_humidity <- sqrt(max(dframe2a$humidity_percentage_hive_ibutton_01+1) - dframe2a$humidity_percentage_hive_ibutton_01) 



library(glmmTMB)
library(bbmle) 
final.model3 <- glmmTMB(neg_sqrt_humidity ~                          # the dependent variable - neg. sqrt humidity
                            temperature_glasshouse_ibutton_01 +         # continuous explanatory variable
                            days_in_glasshouse +                        # continuous explanatory variable
                            temperature_glasshouse_ibutton_01:days_in_glasshouse  # interaction term
                          + (1|hive) + (1|previous_day_treatment_temperature_C),  # the random terms
                          family = "gaussian", na.action = "na.pass", 
                          data = dframe2a)            # gaussian model of humidity data 
summary(final.model3)  


# R-squared 
library(MuMIn)
r.squaredGLMM(final.model3)

## summary table
library(sjPlot)
tab_model(final.model3, show.est = TRUE, show.se = TRUE, show.stat = TRUE)

## check residuals
sresid <- resid(final.model3, type = "pearson")
hist(sresid)

### description of final model paramters
print(final.model3, corr = F)


#############################
## Plotting model 3 output
#############################

library(ggplot2)
library(sjPlot)
library(effects)

### the modeled impact on Neg_RH of glasshouse air temperature

nnn1 <- plot(Effect(focal.predictors = c("temperature_glasshouse_ibutton_01"), 
            mod=final.model3),
            type = "response", 
            ylab="Neg RH", 
            xlab="Glasshouse temperature (°C)")
nnn1

### exporting high res image
# PDF
pdf(file = "Figure_3_Neg_RH_vs_temperature.pdf", width = 8, height = 6, family = "Helvetica")
nnn1
dev.off()

# TIFF
tiff("Figure_3_Neg_RH_vs_temperature.tiff", height = 12, width = 16, units = 'cm', res = 300)
nnn1
dev.off()



### the modeled impact on Neg_RH of the interaction between 
### glasshouse temperature and the number of days in the glasshouse, displaying all days



mmm1 <- plot(predictorEffect("temperature_glasshouse_ibutton_01", final.model3),
             type = "response", 
             xlab="Glasshouse temperature (°C)", ylab="Neg RH", 
             main="The interaction between between glasshouse temperature and number of days spent in the glasshouse")
mmm1

### exporting high res image
# PDF
pdf(file = "Figure_S13_Neg_RH_vs_temperature_days.pdf", width = 15, height = 6, family = "Helvetica")
mmm1
dev.off()

# TIFF
tiff("Figure_S13_Neg_RH_vs_temperature_days.tiff", height = 12, width = 30, units = 'cm', res = 300)
mmm1
dev.off()

