###############################################################
####### choosing 4 random combinations of a 6 day temperature regime


install.packages("StatRank")
library(StatRank)

### temperature regime for six days
temp <- c(7, 12, 17, 22, 27, 36)


# do 100 random scrambles of the temperatuer regime
random_temp1 <- scramble(temp)
random_temp2 <- scramble(temp)
random_temp3 <- scramble(temp)
random_temp4 <- scramble(temp)
random_temp5 <- scramble(temp)
random_temp6 <- scramble(temp)
random_temp7 <- scramble(temp)
random_temp8 <- scramble(temp)
random_temp9 <- scramble(temp)
random_temp10 <- scramble(temp)
random_temp11 <- scramble(temp)
random_temp12 <- scramble(temp)
random_temp13 <- scramble(temp)
random_temp14 <- scramble(temp)
random_temp15 <- scramble(temp)
random_temp16 <- scramble(temp)
random_temp17 <- scramble(temp)
random_temp18 <- scramble(temp)
random_temp19 <- scramble(temp)
random_temp20 <- scramble(temp)
random_temp21 <- scramble(temp)
random_temp22 <- scramble(temp)
random_temp23 <- scramble(temp)
random_temp24 <- scramble(temp)
random_temp25 <- scramble(temp)
random_temp26 <- scramble(temp)
random_temp27 <- scramble(temp)
random_temp28 <- scramble(temp)
random_temp29 <- scramble(temp)
random_temp30 <- scramble(temp)
random_temp31 <- scramble(temp)
random_temp32 <- scramble(temp)
random_temp33 <- scramble(temp)
random_temp34 <- scramble(temp)
random_temp35 <- scramble(temp)
random_temp36 <- scramble(temp)
random_temp37 <- scramble(temp)
random_temp38 <- scramble(temp)
random_temp39 <- scramble(temp)
random_temp40 <- scramble(temp)
random_temp41 <- scramble(temp)
random_temp42 <- scramble(temp)
random_temp43 <- scramble(temp)
random_temp44 <- scramble(temp)
random_temp45 <- scramble(temp)
random_temp46 <- scramble(temp)
random_temp47 <- scramble(temp)
random_temp48 <- scramble(temp)
random_temp49 <- scramble(temp)
random_temp50 <- scramble(temp)
random_temp51 <- scramble(temp)
random_temp52 <- scramble(temp)
random_temp53 <- scramble(temp)
random_temp54 <- scramble(temp)
random_temp55 <- scramble(temp)
random_temp56 <- scramble(temp)
random_temp57 <- scramble(temp)
random_temp58 <- scramble(temp)
random_temp59 <- scramble(temp)
random_temp60 <- scramble(temp)
random_temp61 <- scramble(temp)
random_temp62 <- scramble(temp)
random_temp63 <- scramble(temp)
random_temp64 <- scramble(temp)
random_temp65 <- scramble(temp)
random_temp66 <- scramble(temp)
random_temp67 <- scramble(temp)
random_temp68 <- scramble(temp)
random_temp69 <- scramble(temp)
random_temp70 <- scramble(temp)
random_temp71 <- scramble(temp)
random_temp72 <- scramble(temp)
random_temp73 <- scramble(temp)
random_temp74 <- scramble(temp)
random_temp75 <- scramble(temp)
random_temp76 <- scramble(temp)
random_temp77 <- scramble(temp)
random_temp78 <- scramble(temp)
random_temp79 <- scramble(temp)
random_temp80 <- scramble(temp)
random_temp81 <- scramble(temp)
random_temp82 <- scramble(temp)
random_temp83 <- scramble(temp)
random_temp84 <- scramble(temp)
random_temp85 <- scramble(temp)
random_temp86 <- scramble(temp)
random_temp87 <- scramble(temp)
random_temp88 <- scramble(temp)
random_temp89 <- scramble(temp)
random_temp90 <- scramble(temp)
random_temp91 <- scramble(temp)
random_temp92 <- scramble(temp)
random_temp93 <- scramble(temp)
random_temp94 <- scramble(temp)
random_temp95 <- scramble(temp)
random_temp96 <- scramble(temp)
random_temp97 <- scramble(temp)
random_temp98 <- scramble(temp)
random_temp99 <- scramble(temp)
random_temp100 <- scramble(temp)


### select four of the random scrambles at random
choose_scramble_numbers <- sample(1:100, 4, replace=F)
choose_scramble_numbers 

### these were the four random temperature regimes that came up for me
### you may wish to run it again yourself and get a different four
random_temp77
random_temp69 
random_temp27 
random_temp2 


# Housekeeping
graphics.off() 
rm(list=ls())