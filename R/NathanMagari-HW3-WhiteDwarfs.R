## Visualizing Nearby White Dwarfs
#  © J.L. Turner 2022
#  In this project, you will use data from Gaia to obtain
#  distances and absolute magnitudes for nearby white dwarfs.
#  The data consists of coordinates, parallaxes, proper motions,
#  apparent magnitudes and colors.

# load contributed packages with pacman
library(pacman)
# library(tidyverse)  # w/o pacman
# library(ggplot2)  # w/o pacman
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
  ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny,
  stringr, tidyr)

###################################################################
# First set working directory. Then read csv data into dat
setwd("~/Astro 115")
dat=read.csv("WD.csv")

###################################################################
# Convert Gaia parallaxes to distances from the sun, Dsun.  
# Insert an equation for Dsun in units of parsecs from problem 4a.
# Take note of the units for the Gaia parallaxes, and check
# your distances. This will be assigned to variable dat$Dsun in
# the dat dataframe.
dat$Dsun = 10.e3/dat$parallax
hist(dat$Dsun)
max(dat$Dsun)
###################################################################
# Use these distances to assign absolute magnitudes. This variable
# will be "dat$GMagnitude" since it is Gaia G Magnitude. Using 
# your expression from 4b, insert an equation for absolute magnitude.
# The variable phot_g_mean_mag is the apparent g magnitude observed 
# by Gaia. 
dat$GMagnitude = dat$phot_g_mean_mag - 5.*log10(dat$Dsun/100.)


###################################################################
# Make a histogram of magnitudes. This is why R is great. 
hist(dat$GMagnitude, breaks=11, freq = FALSE,main = "Gaia WD Absolute Magnitudes",xlab = "Absolute G filter magnitude",ylab = "Fraction of white dwarfs",col="lightblue")

###################################################################
# Convert Gmagnitudes to luminosities using the Gaia G calibration of
# Casagrande & VandenBerg. In Vega magnitudes, the sun's
# G magnitude is 4.67. Use the expression you derived in HW#3
# to obtain luminosities from GMagnitudes. Express in 
# units of solar luminosities
dat$logL_Lsun = 1.868 - 0.4*dat$GMagnitude

###################################################################
# Make a histogram of L. This follows the histogram of G mag.
# But make note of the numbers in the summary, especially
# median, and highest and lowest values of L.
hist((dat$logL_Lsun), breaks=11, xlim=c(-4.6,-2),main = "White dwarf luminosities",xlab = "log(L/Lsun)",ylab = "Frequency of white dwarfs",col="lightblue")
summary(dat$logL_Lsun)
summary(10^(dat$logL_Lsun))

# Next, we'll use an expression from Jordi+ to assign Teff based on
# the Gaia color bp-rp. It is only good for bp_rp < 1.5, but
# that is fine for WD, which are bluer than most stars. Their
# expression is for log(Teff). (Bear in mind that log=log10 in
# R but we're just using this as a name)
dat$logTeff = 3.999 - 0.654*dat$bp_rp +0.709*(dat$bp_rp)^2 -
  0.316*(dat$bp_rp)^3
hist(10^(dat$logTeff), breaks=11)
summary(10^(dat$logTeff))

###################################################################
# White dwarfs are confined to a particular region of the HR
# diagram, because they are hot and small.
# Since you know from the Althaus+ article that the average 
# WD is very predictable, use the statistics of your sample
# to predict a median radius. Define a new variable, dat$logR_Rsun 
# based on dat$logL_Lsun and dat$logTeff using your work from
# Problem 4c. When you have dat$logR_Rsun, get an analysis of the 
# this variable from the sample using summary(). Make note of these values. 
# (Remember that R requires log10(), be sure to include the 10)
dat$logR_Rsun = 0.5*dat$logL_Lsun - 2.*dat$logTeff + 7.524
hist((dat$logR_Rsun), breaks=20, xlim=c(-2.6,-1.2),main = "White dwarf radii",xlab = "log(R/Rsun)",ylab = "Frequency of white dwarfs",col="lightblue")
summary(10^(dat$logR_Rsun))  # In case you prefer this to logs
summary(dat$logR_Rsun)


################################################################
# With absolute magnitudes in hand, one can make a color-magnitude 
# (HR) diagram. Here, the color variable is dat$bp-rp (blue-red).
# Your task is to choose limits on x or y if necessary with 
# xlim=c(x1,x2) or ylim=c(y1,y2) to make it look like a conventional CMD
# with brighter stars to the top and bluer to the left.
plot( dat$bp_rp, dat$GMagnitude,ylim=c(16,10) )

###################################################################
# WD fall on a fairly tight line in the CMD. What does this mean? 
# First, let's plot dat$L_Lsun vs. dat$logTeff to get a feeling
# for the luminosities and temperatures.Adjust the limits
# to get a standard HR diagram.
plot( dat$logL_Lsun ~ dat$logTeff, ylim=c(-4.5,-2.5),xlim=c(4.2,3.6),pch=16,col="lightblue3",
      main='WD Temperature Luminosity Relation',xlab='log(Teff/K)',ylab='log(L/Lsun)')

###################################################################
# Unlike in MS stars, Teff is predominantly a function of age for WD. 
# (Caveat: Teff is a number describing the radiant output from the 
# surface of the WD, and is not the same as Tc)
# So the question is, what causes the tight distribution of WD
# in the logL - Log Teff plane? Is it a function of R?
#
# First, use the function "model = lm(y ~ x)" to fit a linear model
# to the data of the form y = mx + b. Then summary(model) will
# give you the intercept (b) and slope (m) of this line.
model=lm( dat$logL_Lsun ~ dat$logTeff)
summary(model)
abline(model, col="blue",lwd=2)

###################################################################
# An interesting visual to help us consider this question is 
# putting lines of constant R on the HR. From the summary information,
# pick the median R,  and two other meaningful values of R for WD,
# such as 1st and 3rd quartiles, or other values, specify these
# in your writeup. Use these values of log(R_Rsun) to define
# three lines, the equation you obtained in problem 6h. The lines
# contain the x variable which is dat$logTeff. Then the commands
# "lines" plot these on top of your CMD. You could add a legend
# following last week's script if you wanted to.
y_median = 4*dat$logTeff + 2*median(dat$logR_Rsun)-15.05
y_1 = 4*dat$logTeff + 2*quantile(dat$logR_Rsun,0.25)-15.05
y_2 = 4*dat$logTeff + 2*quantile(dat$logR_Rsun,0.75)-15.05
lines( y_median ~ dat$logTeff, col="green4",lty="dashed" ,lwd=2)
lines( y_1 ~ dat$logTeff, col="orange",lty="dashed",lwd=2 )
lines( y_2 ~ dat$logTeff, col="red",lty="dashed",lwd=2 )

legend(x = "bottomleft",
       inset = .03,
       legend = c("Linear fit","1st Qu. radius","Median radius", "3rd Qu. radius"),
       bty = "n",   # removes legend box
       lty = c("solid", "dashed", "dashed","dashed"),
       lwd = c(2, 2, 2, 2),
       col = c("blue", 
                    "orange", 
                    "green4", 
                    "red"))
############# SAVE/PRINT THIS PLOT AND YOU ARE DONE! ##############

###################################################################
##############################  FIN!  #############################

# Unload contributed packages
# p_unload(all) 

# Unload base packages such as datasets
# detach("package:datasets", unload=TRUE)


