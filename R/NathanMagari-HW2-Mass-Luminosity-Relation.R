## Mass-Luminosity Relation
#  © J.L. Turner 2022
#
#  In this project, you will digitize a plot from the book to 
#  obtain values for Teff, L/Lsun, and M for main sequence stars.
#  Using this data you will fit a linear correlation and a mass-
#  luminosity relation.

# load contributed packages with pacman
library(pacman)
# library(tidyverse)  # w/o pacman
# library(ggplot2)  # w/o pacman
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
  ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny,
  stringr, tidyr)

#############################################################
# First set working directory.Since we are  not reading
# any data in for this exercise, this is just the 
# directory you prefer to work in.
setwd("~/Astro 115/")

#############################################################
# Read in digitized dat file; you have digitized the L-Teff
# plot at the different values of M in the figure and 
# have added a column with these M with their corresponding
# L and Teff. You should then have in your csv file columns
# with log(Teff), log(L/Lsun), and mass for several "observations".
dat=read.csv("MassLuminosityDigitized.csv")

#############################################################
# We want to determine a mass luminosity relation. We need a
# relation that looks roughly linear. Does a plot of M vs.
# log L/Lsun look linear?  You can always define another column
# in the dat dataframe by assigning values to a new variable
# using dat$VARIABLE = XXXX, or you can recognize that plot can
# handle basic functions, such as plot(sin(x), y) or 
# plot(log10(x), y) etc. Play around till you get something
# linear.

plot(dat$LogL, log10(dat$M), main = "Mass/Luminosity Relationship",
     xlab = 'log(L/Lsun)',ylab = 'log(M/Msun)',pch = 3,col="red",lwd=3)

#############################################################
# The following command does a linear regression.
# Take the two variables you found to be linear, and execute
# the command lm(y ~ x). Like plot(), lm is smart enough to
# handle simple math functions.

model=lm(log10(dat$M)~dat$LogL)
abline(model,lty=3,lwd=2,col="darkgray")
summary(model)
m = format(model$coefficients[2],digits=3)
b = format(model$coefficients[1],digits=1)
legend(x = "topleft",
       inset = .03,
       legend = c("Data", "Linear model"),
       bty = "n",   # removes legend box
       lty = c(0, 3),
       lwd = c(3, 2),
       pch = c(3,26),
       col = c("red", "darkgray"))
legend(x="bottomright",paste("y = ",m,"x +",b), inset = 0.09, bty = "n",lty = 3,lwd=2,col="darkgray")

#############################################################
#  Note the intercept and slope for your answer.

#############################################################
######################  FIN!  ###############################
#############################################################

# Unload contributed packages
# p_unload(all) 

# Unload base packages such as datasets
# detach("package:datasets", unload=TRUE)


