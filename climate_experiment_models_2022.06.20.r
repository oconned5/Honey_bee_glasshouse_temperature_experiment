########################################################################################
# Analyses of glasshouse experiment data, measuring the effect of temperature 
# on honey bee movement and brood maintenance


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

#################################################################################


dframe1 <- read.csv(file.choose())    # Select "Glasshouse_experiment_database"
head(dframe1)
summary(dframe1)

dframe2 <- subset(dframe1, period == "experiment" & glasshouse_temperatue_setting == "day" &
                    bees_out != "NA" & temperature_glasshouse_ibutton_01 != "NA")


#################################################################

## Model 1

# Relationship between worker movement and glasshouse temperature----
# plus Replicate, and date, with hive as a random term

hist(dframe2$bees_out) 


library(lme4)
final.model1 <- glmer(bees_out  ~                 # the dependent variable
                        temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                        date_julian + 
                        (1|hive),                # the random term 
                      na.action = na.pass,
                      family = "poisson" (link="log"), data = dframe2) # poisson model of count data 

summary(final.model1) #the warning message here suggests we have a scaling issue, need to standardise variablies



# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.final.model1 <- standardize(final.model1, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.final.model1)


library(car)
Anova(stdz.final.model1, type = 3) 


# R-squared 

library(MuMIn)
r.squaredGLMM(stdz.final.model1)


## summary table
library(sjPlot)
tab_model(stdz.final.model1, show.est = TRUE, show.se = TRUE, show.stat = TRUE)


## model validation

library(LMERConvenienceFunctions)
mcp.fnc(stdz.final.model1) # plots of model fit
plot(stdz.final.model1, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals


sresid <- resid(stdz.final.model1, type = "pearson")
hist(sresid)

fits <- fitted(stdz.final.model1)
plot(sresid ~ fits)



### description of final model paramters
print(stdz.final.model1, corr = F)

plotLMER.fnc(stdz.final.model1) # influence of individual explanatory variables



## Figures model 1 ----

library(ggplot2)
library(sjPlot)



## 

ff1 <- theme_set(theme_bw())
ff2 <- plot_model(final.model1, type = "pred", terms = c("temperature_glasshouse_ibutton_01", "Replicate"))
ff3 <- ff2 + labs(color = "Replicate")
ff4 <- ff3 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=18), #change legend title font size
                   legend.text = element_text(size=10)) 
ff5 <- ff4 + geom_line(size = 2)
#ff5 <- ff4 + theme(panel.grid.major = element_blank(),
#                 panel.grid.minor = element_blank()) +
#theme(panel.background = element_blank())
ff6 <- ff5 + ylab("Worker movement")
ff7 <- ff6 + xlab("Glasshouse Temperature (°C)")
#ff8 <- ff7 + theme(axis.line = element_line(colour = "black"))
#ff9 <- ff8 + scale_color_manual(values=c("black", "red"))
ff10 <- ff7 + theme(axis.title.y=element_text(face="bold", size=18, vjust=1.5))
ff11 <- ff10 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
ff12 <- ff11 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
ff13 <- ff12 + theme(plot.title = element_text(color="black", size=18, face="bold"))
ff14 <- ff13 + ggtitle("") ## blanking top title
ff15 <- ff14 + theme(strip.text.x = element_text(face="bold", size=18, colour = "black"))
ff15

### exporting high res image
# PDF
pdf(file = "Figure_2_Worker_movement.pdf", width = 10, height = 6, family = "Helvetica")
ff15
dev.off()

# TIFF
tiff("Figure_2_Worker_movement.tiff", height = 12, width = 21, units = 'cm', res = 300)
ff15
dev.off()

## Predicted effects of day in the year

rr1 <- theme_set(theme_bw())
rr2 <- plot_model(final.model1, type = "pred", terms = c("date_julian [all]", "Replicate"))
rr3 <- rr2 + labs(color = "Replicate")
rr4 <- rr3 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=18), #change legend title font size
                   legend.text = element_text(size=10)) 
rr5 <- rr4 + geom_line(size = 2)
#rr5 <- rr4 + theme(panel.grid.major = element_blank(),
#                 panel.grid.minor = element_blank()) +
#theme(panel.background = element_blank())
rr6 <- rr5 + ylab("Worker movement")
rr7 <- rr6 + xlab("Time of year (Julian day)")
#rr8 <- rr7 + theme(axis.line = element_line(colour = "black"))
#rr9 <- rr8 + scale_color_manual(values=c("black", "red"))
rr10 <- rr7 + theme(axis.title.y=element_text(face="bold", size=18, vjust=1.5))
rr11 <- rr10 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
rr12 <- rr11 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
rr13 <- rr12 + theme(plot.title = element_text(color="black", size=18, face="bold"))
rr14 <- rr13 + ggtitle("") ## blanking top title
rr15 <- rr14 + theme(strip.text.x = element_text(face="bold", size=18, colour = "black"))
rr15

### exporting high res image
# PDF
pdf(file = "Figure_S2_Worker_movement.pdf", width = 10, height = 6, family = "Helvetica")
rr15
dev.off()

# Tiff
tiff("Figure_S2_Worker_movement.tiff", height = 12, width = 21, units = 'cm', res = 300)
rr15
dev.off()


#################################################################

## Model 2

# Relationship between worker movement and glasshouse temperature----
# plus Replicate, and date, with hive as a random term

hist(dframe2$temperature_C_hive_ibutton_01)  ## fairly normally distributed


library(lme4)
library(lmerTest)
final.model2 <- lmer(temperature_C_hive_ibutton_01  ~                 # the dependent variable
                       temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                       date_julian + 
                       (1|hive),                # the random term 
                     na.action = na.pass,
                     data = dframe2) # gaussian model of temperature data 

summary(final.model2) 


# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on diiierent scales

library(arm)                                    
stdz.final.model2 <- standardize(final.model2, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.final.model2)


library(car)
Anova(stdz.final.model2, type = 3) 


# R-squared 

library(MuMIn)
r.squaredGLMM(stdz.final.model2)


## summary table
library(sjPlot)
tab_model(stdz.final.model2, show.est = TRUE, show.se = TRUE, show.stat = TRUE)


## model validation

library(LMERConvenienceFunctions)
mcp.fnc(stdz.final.model2) # plots of model fit
plot(stdz.final.model2, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals


sresid <- resid(stdz.final.model2, type = "pearson")
hist(sresid)

fits <- fitted(stdz.final.model2)
plot(sresid ~ fits)



### description of final model paramters
print(stdz.final.model2, corr = F)

plotLMER.fnc(stdz.final.model2) # influence of individual explanatory variables




## Figures model 2 ----

library(ggplot2)
library(sjPlot)



## Predicted probabilities of nests being in Nestboxes

ii1 <- theme_set(theme_bw())
ii2 <- plot_model(final.model2, type = "pred", terms = c("temperature_glasshouse_ibutton_01", "Replicate"))
ii3 <- ii2 + labs(color = "Replicate")
ii4 <- ii3 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=18), #change legend title font size
                   legend.text = element_text(size=10)) 
ii5 <- ii4 + geom_line(size = 2)
#ii5 <- ii4 + theme(panel.grid.major = element_blank(),
#                 panel.grid.minor = element_blank()) +
#theme(panel.background = element_blank())
ii6 <- ii5 + ylab("Hive Brood Temperature (°C)")
ii7 <- ii6 + xlab("Glasshouse Temperature (°C)")
#ii8 <- ii7 + theme(axis.line = element_line(colour = "black"))
#ii9 <- ii8 + scale_color_manual(values=c("black", "red"))
ii10 <- ii7 + theme(axis.title.y=element_text(face="bold", size=18, vjust=1.5))
ii11 <- ii10 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
ii12 <- ii11 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
ii13 <- ii12 + theme(plot.title = element_text(color="black", size=18, face="bold"))
ii14 <- ii13 + ggtitle("") ## blanking top title
ii15 <- ii14 + theme(strip.text.x = element_text(face="bold", size=18, colour = "black"))
ii15

### exporting high res image
# PDF
pdf(file = "Figure_3_Hive_brood_temperature.pdf", width = 10, height = 6, family = "Helvetica")
ii15
dev.off()

# Tiff
tiff("Figure_3_Hive_brood_temperature.tiff", height = 12, width = 21, units = 'cm', res = 300)
ii15
dev.off()


## Predicted effects of day in the year

ll1 <- theme_set(theme_bw())
ll2 <- plot_model(final.model2, type = "pred", terms = c("date_julian [all]", "Replicate"))
ll3 <- ll2 + labs(color = "Replicate")
ll4 <- ll3 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=18), #change legend title font size
                   legend.text = element_text(size=10)) 
ll5 <- ll4 + geom_line(size = 2)
#ll5 <- ll4 + theme(panel.grid.major = element_blank(),
#                 panel.grid.minor = element_blank()) +
#theme(panel.background = element_blank())
ll6 <- ll5 + ylab("Hive Brood Temperature (°C)")
ll7 <- ll6 + xlab("Time of year (Julian day)")
#ll8 <- ll7 + theme(axis.line = element_line(colour = "black"))
#ll9 <- ll8 + scale_color_manual(values=c("black", "red"))
ll10 <- ll7 + theme(axis.title.y=element_text(face="bold", size=18, vjust=1.5))
ll11 <- ll10 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
ll12 <- ll11 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
ll13 <- ll12 + theme(plot.title = element_text(color="black", size=18, face="bold"))
ll14 <- ll13 + ggtitle("") ## blanking top title
ll15 <- ll14 + theme(strip.text.x = element_text(face="bold", size=18, colour = "black"))
ll15

### exporting high res image
# PDF
pdf(file = "Figure_S3_Hive_brood.pdf", width = 10, height = 6, family = "Helvetica")
ll15
dev.off()

# Tiff
tiff("Figure_S3_Hive_brood.tiff", height = 12, width = 21, units = 'cm', res = 300)
ll15
dev.off()

#################################################################

## Model 3

# Relationship between worker movement and glasshouse temperature----
# plus Replicate, and date, with hive as a random term

hist(dframe2$humidity_percentage_hive_ibutton_01)  ## fairly normally distributed

log_hum <- log10(dframe2$humidity_percentage_hive_ibutton_01)
hist(log_hum)

dframe3 <- subset(dframe2, humidity_percentage_hive_ibutton_01 >50)

hist(dframe3$humidity_percentage_hive_ibutton_01)

library(lme4)
library(lmerTest)
final.model3 <- lmer(humidity_percentage_hive_ibutton_01  ~                 # the dependent variable
                       temperature_glasshouse_ibutton_01 + Replicate +   # fixed term
                       date_julian + 
                       (1|hive),                # the random term 
                     na.action = na.pass,
                     data = dframe2) # gaussian model of humidity data 

summary(final.model3) 



# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on dppierent scales

library(arm)                                    
stdz.final.model3 <- standardize(final.model3, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.final.model3)


library(car)
Anova(stdz.final.model3, type = 3) 


# R-squared 

library(MuMIn)
r.squaredGLMM(stdz.final.model3)


## summary table
library(sjPlot)
tab_model(stdz.final.model3, show.est = TRUE, show.se = TRUE, show.stat = TRUE)


## model validation

library(LMERConvenienceFunctions)
mcp.fnc(stdz.final.model3) # plots of model fit
plot(stdz.final.model3, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals


sresid <- resid(stdz.final.model3, type = "pearson")
hist(sresid)

fits <- fitted(stdz.final.model3)
plot(sresid ~ fits)



### description of final model paramters
print(stdz.final.model3, corr = F)

plotLMER.fnc(stdz.final.model3) # influence of individual explanatory variables




## Figures model 3 ----

library(ggplot2)
library(sjPlot)



## Predicted effects of glasshouse temperature

pp1 <- theme_set(theme_bw())
pp2 <- plot_model(final.model3, type = "pred", terms = c("temperature_glasshouse_ibutton_01", "Replicate"))
pp3 <- pp2 + labs(color = "Replicate")
pp4 <- pp3 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=18), #change legend title font size
                   legend.text = element_text(size=10)) 
pp5 <- pp4 + geom_line(size = 2)
#pp5 <- pp4 + theme(panel.grid.major = element_blank(),
#                 panel.grid.minor = element_blank()) +
#theme(panel.background = element_blank())
pp6 <- pp5 + ylab("Hive Brood Humidity (%)")
pp7 <- pp6 + xlab("Glasshouse Temperature (°C)")
#pp8 <- pp7 + theme(axis.line = element_line(colour = "black"))
#pp9 <- pp8 + scale_color_manual(values=c("black", "red"))
pp10 <- pp7 + theme(axis.title.y=element_text(face="bold", size=18, vjust=1.5))
pp11 <- pp10 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
pp12 <- pp11 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
pp13 <- pp12 + theme(plot.title = element_text(color="black", size=18, face="bold"))
pp14 <- pp13 + ggtitle("") ## blanking top title
pp15 <- pp14 + theme(strip.text.x = element_text(face="bold", size=18, colour = "black"))
pp15

### exporting high res image
# PDF
pdf(file = "Figure_4_Hive_humidity.pdf", width = 10, height = 6, family = "Helvetica")
pp15
dev.off()

# Tiff
tiff("igure_4_Hive_humidity.tiff", height = 12, width = 21, units = 'cm', res = 300)
pp15
dev.off()

## Predicted effects of day in the year

oo1 <- theme_set(theme_bw())
oo2 <- plot_model(final.model3, type = "pred", terms = c("date_julian [all]", "Replicate"))
oo3 <- oo2 + labs(color = "Replicate")
oo4 <- oo3 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=18), #change legend title font size
                   legend.text = element_text(size=10)) 
oo5 <- oo4 + geom_line(size = 2)
#oo5 <- oo4 + theme(panel.grid.major = element_blank(),
#                 panel.grid.minor = element_blank()) +
#theme(panel.background = element_blank())
oo6 <- oo5 + ylab("Hive Brood Humidity (%)")
oo7 <- oo6 + xlab("Time of year (Julian day)")
#oo8 <- oo7 + theme(axis.line = element_line(colour = "black"))
#oo9 <- oo8 + scale_color_manual(values=c("black", "red"))
oo10 <- oo7 + theme(axis.title.y=element_text(face="bold", size=18, vjust=1.5))
oo11 <- oo10 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
oo12 <- oo11 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
oo13 <- oo12 + theme(plot.title = element_text(color="black", size=18, face="bold"))
oo14 <- oo13 + ggtitle("") ## blanking top title
oo15 <- oo14 + theme(strip.text.x = element_text(face="bold", size=18, colour = "black"))
oo15

### exporting high res image
# PDF
pdf(file = "Figure_S4_Hive_humidity.pdf", width = 10, height = 6, family = "Helvetica")
oo15
dev.off()

# Tiff
tiff("Figure_S4_Hive_humidity.tiff", height = 12, width = 21, units = 'cm', res = 300)
oo15
dev.off()
