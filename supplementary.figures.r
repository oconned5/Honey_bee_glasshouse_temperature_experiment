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



#### plotting raw variables
#### air temperature vs bee movement


tt1 <- ggplot(dframe2, aes(x=temperature_glasshouse_ibutton_01, y=bees_out, color=Experimental_group, shape=Experimental_group)) +
  geom_point() + 
  geom_smooth(method=lm, aes(fill=Experimental_group))
tt2 <- tt1 + theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
tt3 <- tt2 + ylab("Worker movement")
tt4 <- tt3 + xlab("Glasshouse temperature (°C)")
tt5 <- tt4 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
tt6 <- tt5 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
tt7 <- tt6 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
tt8 <- tt7 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=15), #change legend title font size
                   legend.text = element_text(size=10)) 
tt8


### exporting high res image
# PDF
pdf(file = "Figure_S6_Raw_variables_worker_movement_vs_air_temperature.pdf", width = 10, height = 6, family = "Helvetica")
tt8
dev.off()

# TIFF
tiff("Figure_S6_variables_worker_movement_vs_air_temperature.tiff", height = 12, width = 21, units = 'cm', res = 300)
tt8
dev.off()


#### air temperature vs hive temperature

dd1 <- ggplot(dframe2, aes(x=temperature_glasshouse_ibutton_01, y=temperature_C_hive_ibutton_01, color=Experimental_group, shape=Experimental_group)) +
  geom_point() + 
  geom_smooth(method=lm, aes(fill=Experimental_group))
dd2 <- dd1 + theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
dd3 <- dd2 + ylab("Colony brood temperature (°C)")
dd4 <- dd3 + xlab("Glasshouse temperature (°C)")
dd5 <- dd4 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
dd6 <- dd5 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
dd7 <- dd6 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
dd8 <- dd7 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=15), #change legend title font size
                   legend.text = element_text(size=10)) 
dd8


### exporting high res image
# PDF
pdf(file = "Figure_S8_Raw_variables_hive_temperature_vs_air_temperature.pdf", width = 10, height = 6, family = "Helvetica")
dd8
dev.off()

# TIFF
tiff("Figure_S8_variables_hive_temperature_vs_air_temperature.tiff", height = 12, width = 21, units = 'cm', res = 300)
dd8
dev.off()




#### air temperature vs negative square root transformation of humidity data

## exclude hive_02 as it's humidity readings don't seem to have worked
dframe2a <- subset(dframe2, hive != "hive_02")

neg_sqrt_humidity <- sqrt(max(dframe2a$humidity_percentage_hive_ibutton_01+1) - dframe2a$humidity_percentage_hive_ibutton_01) 


ll1 <- ggplot(dframe2a, aes(x=temperature_glasshouse_ibutton_01, y=neg_sqrt_humidity, color=Experimental_group, shape=Experimental_group)) +
  geom_point() + 
  geom_smooth(method=lm, aes(fill=Experimental_group))
ll2 <- ll1 + theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
ll3 <- ll2 + ylab("Neg_RH")
ll4 <- ll3 + xlab("Glasshouse temperature (°C)")
ll5 <- ll4 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
ll6 <- ll5 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
ll7 <- ll6 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
ll8 <- ll7 + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=15), #change legend title font size
                   legend.text = element_text(size=10)) 
ll8



### exporting high res image
# PDF
pdf(file = "Figure_S9_Raw_variables_Neg_RH_vs_air_temperature.pdf", width = 10, height = 6, family = "Helvetica")
ll8
dev.off()

# TIFF
tiff("Figure_S9_Raw_variables_Neg_RH_vs_air_temperature.tiff", height = 12, width = 21, units = 'cm', res = 300)
ll8
dev.off()

