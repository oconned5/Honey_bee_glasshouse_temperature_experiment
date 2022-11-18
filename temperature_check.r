
##############################################################################
## graphing temperature - target vs reality ###################
#############################################################################



# Housekeeping

rm(list=ls()) # remove everything currently held in the R memory

graphics.off() # close all open graphics windows 

# Packages that may be needed, remove the # to install


#install.packages("ggplot2")
#install.packages("tidyr")

############################

# Exploration of Teagasc glasshouse experiment data

dframe1 <- read.csv(file.choose())    # Select "Glasshouse_experiment_database"
head(dframe1)
summary(dframe1)



library(ggplot2)
library(tidyr)

dframe4 <- subset(dframe1, period == "experiment" & glasshouse_temperatue_setting == "day" &
                    temperature_glasshouse_ibutton_01 != "NA", 
                  select=c(date_julian, unix_timestamp, treatment_temperature_C, temperature_glasshouse_ibutton_01))

dframe5 <- pivot_longer(dframe4, cols=3:4, names_to = "category", values_to = "temperature")


## by day

ff1 <- ggplot(dframe5, aes(date_julian, temperature, colour = category)) +
  geom_point() +
  geom_smooth(method = "loess") # + ylim(0, 2.5)
ff2 <- ff1 + theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
ff3 <- ff2 + ylab("Temperature")
ff4 <- ff3 + xlab("Julian date")
ff5 <- ff4 + theme(axis.line = element_line(colour = "black"))
ff6 <- ff5 + scale_color_manual(values=c("black", "red"))
ff7 <- ff6 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
ff8 <- ff7 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
ff9 <- ff8 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
ff10 <- ff9 + geom_point(size = 3)
ff10

### exporting high res image
# PDF
pdf(file = "Temperature by day.pdf", width = 12, height = 6, family = "Helvetica")
ff10
dev.off()

# TIFF
tiff("Temperature by day.tiff", height = 12, width = 22, units = 'cm', res = 300)
ff10
dev.off()




## by ~ 5 minutes intervals

dframe6 <- subset(dframe5, unix_timestamp != "NA")


hh1 <- ggplot(dframe5, aes(unix_timestamp, temperature, colour = category)) +
  geom_line() 
hh2 <- hh1 + theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
hh3 <- hh2 + ylab("Temperature")
hh4 <- hh3 + xlab("Unix timestamp")
hh5 <- hh4 + theme(axis.line = element_line(colour = "black"))
hh6 <- hh5 + scale_color_manual(values=c("black", "red"))
hh7 <- hh6 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
hh8 <- hh7 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
hh9 <- hh8 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
hh10 <- hh9 + geom_line(size = 1.5)
hh10

### exporting high res image
# PDF
pdf(file = "Temperature by 5 minute interval.pdf", width = 12, height = 6, family = "Helvetica")
hh10
dev.off()

# TIFF
tiff("Temperature by 5 minute interval.tiff", height = 12, width = 22, units = 'cm', res = 300)
hh10
dev.off()

