## Basis of the spectral sequence: Boltzmann & Saha equations
#  and Balmer fraction in stars
#  © J.L. Turner 2022
#
#  In this project, you will plot the ratio of the second
#  level of hydrogen to the total number of hydrogen atoms
#  and nuclei (ionized). Absorption from the 2nd level
#  is responsible for the Balmer absorption series in stars. 
#  Here we determine which stars have the largest value by 
#  plotting this ratio, to determine which Teff are most 
#  favorable for producing strong Balmer lines.

# load contributed packages with pacman
library(pacman)
# library(tidyverse)  # w/o pacman
# library(ggplot2)  # w/o pacman
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
  ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny,
  stringr, tidyr)

#########################################################
# First set working directory.Since we are  not reading
# any data in for this exercise, this is just the 
# directory you prefer to work in.
setwd("~/Astro 115")
#dat=read.csv("filename.csv")

# R allows us to do equations but it treats everything
# as data. So we need to define our values for the X axis.
# Since the X-axis will be effective stellar surface
# temperature, we'll define a sequence of x-values, 
# in a variable called "temp", specifying 
# a beginning, an end, and a spacing. To make a smooth
# plot we'll use a spacing of 100 degrees.

# A question mark before an R command gives a help screen
?seq

########################################################
# You need to put in the range of stellar effective 
# temperatures, including beginning, end, and spacing.
# Then typing the variable in RStudio will print this
# vector for you. As an example of how to input big
# numbers into R, specify 1,000,000 by 1e6.
temp = seq(from = 1e3, to = 2e4, by = 100.)
temp

# Define tfour as temperature in units of 10^4K.
tfour = temp/1e4
# pressure in units of dynes/cm2, equivalent to erg/cm3
pressure = 15.

########################################################
# Here, put in an expression for nii_over_ni, the ratio
# of ionized H to neutral H, n(HII)/n(HI), using the Saha 
# equation. Using the "tfour" variable will simplify this
# expression, but is not required if you prefer to use
# the variable "temp". Since this is vectorized, the
# resulting variable "nii_over_ni" will also be a vector
# of the same "length" as tfour (or temp). The command
# length(nii_over_ni) demonstrates this.
nii_over_ni = (3.3338e9) * tfour^2.5 * exp(-15.782/tfour)/pressure
length(temp)
length(tfour)
length(nii_over_ni)
# Check math with book values. Bracket since T step is 100K.
nii_over_ni[which(temp == 5700)]
nii_over_ni[which(temp == 5800)]

########################################################
# Define nii_over_ntot from nii_over_ni following book p 235. 
# Use 15 to mark this variable to retain it for later plots
nii_over_ntot_15 = nii_over_ni/(1+nii_over_ni)

#########################################################
# Now create an expression for n2_over_n1, the ratio of
# the n=1 level population to the n=2 level population
# that is responsible for Balmer absorption. This is the
# Boltzmann equation. Remember the degeneracy (p 238)
n2_over_n1 = 4*exp(-11.8366/tfour)
# Check values
n2_over_n1[which(temp == 5700)]
n2_over_n1[which(temp == 5800)]

########################################################
# Following C&O p 236, define balmerfraction variable
# as the fraction of all H, both ionized and neutral, 
# that is in HI, n=2 state
balmerfraction_15 = n2_over_n1/((1+n2_over_n1)*(1+nii_over_ni))
balmerfraction_15[which(temp == 5700)]
balmerfraction_15[which(temp == 5800)]

########################################################
# Now redo this, first for pressure = 1 dyne per cm^2 
# Note that retyping a command with different global
# inputs (here, pressure) will write over the previous
# values of nii_over_ni, etc.
# First, redo for pressure = 1 dyne/cm^2
pressure = 1.
##########INSERT CODE HERE#############################
nii_over_ni = (3.3338e9) * tfour^2.5 * exp(-15.782/tfour)/pressure
nii_over_ntot_1 = nii_over_ni/(1+nii_over_ni)
balmerfraction_1 = n2_over_n1/((1+n2_over_n1)*(1+nii_over_ni))
########################################################
# Now redo for pressure = 200 dyne/cm^2
pressure = 200.
##########INSERT CODE HERE#############################
nii_over_ni = (3.3338e9) * tfour^2.5 * exp(-15.782/tfour)/pressure
nii_over_ntot_200 = nii_over_ni/(1+nii_over_ni)
balmerfraction_200 = n2_over_n1/((1+n2_over_n1)*(1+nii_over_ni))
########################################################
# Finally redo for pressure = 1000 dyne/cm^2
pressure = 1000.
##########INSERT CODE HERE#############################
nii_over_ni = (3.3338e9) * tfour^2.5 * exp(-15.782/tfour)/pressure
nii_over_ntot_1000 = nii_over_ni/(1+nii_over_ni)
balmerfraction_1000 = n2_over_n1/((1+n2_over_n1)*(1+nii_over_ni))
# First we'll plot the ionized fraction, nii_over_ntot.
# This is the most basic plot, all defaults.
# One can type either plot(y ~ x), or plot(x,y)
plot(nii_over_ntot_15 ~ temp)

#######################################################
# Since values of T are arbitrary, we'll draw lines instead
# using type="l", and add labels.
# You must set the range of interest to better see the
# transition region by setting limits in the xlim vector.
# Repeat this command till you are happy with the plot.
plot(nii_over_ntot_15 ~ temp, type="l", xlim=c(4.5e3,13e3),
     ylab="Ionized hydrogren fraction", 
     xlab="Effective temperature (K)",
     main="Saha Ionization at Different Electron Pressures",col="orange")

# one can add to the basic plot using command "lines"
# Let's add the pressure=1, 15, and 1000 values to the plot
lines(nii_over_ntot_1 ~ temp, col = "red")
lines(nii_over_ntot_200 ~ temp, col = "blue")
lines(nii_over_ntot_1000 ~ temp, col = "purple")

# Add a legend; insert the colors you used for each value of P.
legend(x = "topleft",
       inset = .03,
       legend = c("1", "15", "200", "1000"),
       title = "Free electron\npressure\n(erg/cm^3)",
       bty = "n",   # removes legend box
       lty = c(1, 1, 1, 1),
       lwd = c(1, 1, 1, 1),
       col = c("red", 
               "orange", 
               "blue", 
               "purple"))

########################################################
# Let's do the same for the Balmer fraction. This determines
# where the Balmer lines are strongest.
plot(balmerfraction_15 ~ temp, type="l", xlim=c(7e3,18e3),
     ylim=c(0,2.5e-5),
     ylab="Hydrogen n=2 fraction", 
     xlab="Effective temperature (K)",
     main="Hydrogen Responsible for Balmer Lines (n=2)",col="orange")
lines(balmerfraction_1 ~ temp, col = "red")
lines(balmerfraction_200 ~ temp, col = "blue")
lines(balmerfraction_1000 ~ temp, col = "purple")

# Add a legend; insert the colors you used for each value of P.
legend(x = "topright",
       inset = .02,
       legend = c("1", "15", "200", "1000"),
       bty = "n",   # removes legend box
       title = "Free electron\npressure\n(erg/cm^3)",
       lty = c(1, 1, 1, 1),
       lwd = c(1, 1, 1, 1),
       col = c("red", 
               "orange", 
               "blue", 
               "purple"))

########################################################
# Final step: find max values of Balmer fraction for each pressure
# and then the temperatures at which these maxima occur.
# Enclosing a variable in parentheses automatically prints
# it in RStudio, in addition to the environment window. The
# "which.max" function gives the index of the maximum of the
# variable Balmerfraction. We use this as the index
# of the the temp vector (or tfour) to get the
# temperature corresponding to the maximum Balmer fraction
(Balmermax_15 = max(balmerfraction_15))
(Balmermax_1 = max(balmerfraction_1))
(Balmermax_200 = max(balmerfraction_200))
(Balmermax_1000 = max(balmerfraction_1000))
# now for temps at this index; the square brackets in
# R correspond to a value at a particular index
(TempBalmermax_15 = temp[which.max(balmerfraction_15)])
(TempBalmermax_1 = temp[which.max(balmerfraction_1)])
(TempBalmermax_200 = temp[which.max(balmerfraction_200)])
(TempBalmermax_1000 = temp[which.max(balmerfraction_1000)])

################################################################
######################  FIN!  ##################################
################################################################

# Unload contributed packages
# p_unload(all) 

# Unload base packages such as datasets
# detach("package:datasets", unload=TRUE)


