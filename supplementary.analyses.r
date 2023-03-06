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


### subset Replicate 2 which had such a strong positive relationship between
### worker movement and glasshouse temperature
### check that this result is still consistent even without that replicate

dframeS2 <- subset(dframe2, Replicate != "replicate_02")

#################################################################

## Model S1----

# Relationship between worker movement and glasshouse temperature
# plus Replicate as a fixed effect, with hive and experimental day as random terms

library(lme4)
library(MASS)
final.modelS1 <- glmer.nb(bees_out  ~                            # count dependent variable
                           temperature_glasshouse_ibutton_01 +  # continuous explanatory variable
                           Replicate +                          # fixed term
                           temperature_glasshouse_ibutton_01:Replicate + # interaction term 
                           (1|experiment_day) + (1|hive),   # the random terms - hive ID and experiment day
                         na.action = na.pass, nAGQ = 1, 
                         data = dframeS2)                       # negative bionomial model of count data 

summary(final.modelS1) #the warning message here suggests we have a scaling issue, need to standardise variablies

# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.final.modelS1 <- standardize(final.modelS1, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.final.modelS1)

# R-squared 

library(MuMIn)
r.squaredGLMM(stdz.final.modelS1)


## summary table
library(sjPlot)
tab_model(stdz.final.modelS1, show.est = TRUE, show.se = TRUE, show.stat = TRUE)

## check residuals
plot(stdz.final.modelS1, pch = 20, col = "black", lty = "dotted") # fitted values against the residuals

sresid <- resid(stdz.final.modelS1, type = "pearson")
hist(sresid)

### description of final model paramters
print(stdz.final.modelS1, corr = F)

library(LMERConvenienceFunctions)
plotLMER.fnc(stdz.final.modelS1) # influence of individual explanatory variables


## Figures Model S1 ----

library(ggplot2)
library(sjPlot)

## Plotting Model S1 output


yy1 <- theme_set(theme_bw())
yy2 <- plot_model(final.modelS1, type = "pred", terms = c("temperature_glasshouse_ibutton_01 [all]", "Replicate"))
yy3 <- yy2 + labs(color = "Replicate")
yy4 <- yy3 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=18), #change legend title font size
                   legend.text = element_text(size=10)) 
yy5 <- yy4 + geom_line(size = 2)
yy6 <- yy5 + ylab("Worker movement")
yy7 <- yy6 + xlab("Glasshouse temperature (°C)")
yy10 <- yy7 + theme(axis.title.y=element_text(face="bold", size=18, vjust=1.5))
yy11 <- yy10 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
yy12 <- yy11 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
yy13 <- yy12 + theme(plot.title = element_text(color="black", size=18, face="bold"))
yy14 <- yy13 + ggtitle("") ## blanking top title
yy15 <- yy14 + theme(strip.text.x = element_text(face="bold", size=15, colour = "black"))
yy15

### exporting high res image
# PDF
pdf(file = "Figure_S2_Worker_movement.pdf", width = 10, height = 6, family = "Helvetica")
yy15
dev.off()

# TIFF
tiff("Figure_S2_Worker_movement.tiff", height = 12, width = 21, units = 'cm', res = 300)
yy15
dev.off()
