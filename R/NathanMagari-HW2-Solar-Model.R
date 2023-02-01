## Examining Pressure in the Solar Model
#  © J.L. Turner 2022
#
#  In this project, you will examine the mean molecular weight
#  and the run of pressure within the sun. You will see how this
#  compares to the expression for hydrostatic equilibrium. 
#  We will work with the BSB solar model (Bahcall, Serenelli, 
#  & Basu 2006).

# load contributed packages with pacman
library(pacman)
# library(tidyverse)  # w/o pacman
# library(ggplot2)  # w/o pacman
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny,
               stringr, tidyr)

#############################################################
# First set working directory.
setwd("~/Astro 115")

# These plots are just to have a look at the run of density
# and pressure in the sun.
dat = read.csv("BSBmodel.csv")
# Get the lie of the land
# plot rho vs R
plot(dat$Rho ~ dat$R.Rsun)
#plot log(rho) vs R
plot(log10(dat$Rho) ~ dat$R.Rsun)
plot(log10(dat$P) ~ dat$R.Rsun)

#############################################################
## Make a column with mean molecular weight, fully ionized case.
## INSERT AN EXPRESSION FOR MEAN MOLECULAR WEIGHT HERE.
## Use the expression for full ionization. When
## H in particular becomes neutral, mu rises again, but this
## is probably only the very outermost layers of the sun.
dat$mean.mol.weight = 1/(2*dat$X+0.75*dat$Y.He4.)    # "mu" 

#############################################################
# Now plot X, Y, O (for Z), and mu. You will have to fiddle 
# with a multiplier to get O to show up on the plot and still
# see detail in the others. Make it at least as high as Y.

plot(dat$X ~ dat$R.Rsun, type="l", xlab="R/Rsun",
     ylab="Mass fraction/ molecular weight (mu)", ylim=c(0,1), col="red",
     main = "Mean Molecular Weight Solar Model")    # title of plot
points(dat$Y.He4. ~ dat$R.Rsun, type="l", col="blue")   # plot Y
points(rep(0,1268) ~ dat$R.Rsun, type="l", col="darkgray")    # plot O*XXXX
# Make the mu curve a little heavier weight
points(dat$mean.mol.weight ~ dat$R.Rsun, type="l", col="black", lwd=3) # plot mu

# Add legends. They fit better if done two at a time.
legend(x = "top",
       inset = .02,
       legend = c("X (H)", "Y (He4)"),  # variables 1, 2
       bty = "n",        # removes legend box
       lty = c(1, 1),    # line type for variable 1, 2
       lwd = c(1, 1),    # line weight for variable 1, 2
       col = c("red", "blue"))  # color of variable 1,2

legend(x = "topright",
       inset = .02,
       legend = c("Z", "mu"),   # other two variables
       bty = "n",    
       lty = c(1, 1), 
       lwd = c(1, 3),    # heavier weight for mu
       col = c("darkgray", "black"))

####### End of Problem 2a & b. Proceed for rest of Problem 2. ##


##########################################################
#  Hydrostatic equilibrium check

# To save computing time, define Gkonstant so that inputs to 
# the computations are in M/Msun and R/Rsun as given in model

# You need to show (offline) that the units of Gkonstant are dynes,
# given the model inputs, M/Msun, R/Rsun. The model has pressure
# in dynes. In your writeup you must explain the unit conversion,
# and give the value (R computes it; see environment)
Gkonstant = 6.67e-9 * 1.989e30 / (6.96e8)**2

# dP/dr using table values for R gives units of dynes/solar radii,
# With what number must one divide dR (see below) to get dynes/cm?
Rcon = 1e-10/6.96

# Determine dP/dr using centered differences & compare to rho*g
# Don't forget to insert Rcon where it is needed
#
for (i in 2:(length(dat$R.Rsun)-1)) {
  dat$dP.dr[i] = -Rcon*(dat$P[i+1]-dat$P[i-1])/(dat$R.Rsun[i+1]-dat$R.Rsun[i-1])
  dat$rho.g = Gkonstant * dat$Rho * dat$M.Msun / (dat$R.Rsun^2)
  dat$ratio.dpdr.rhog = dat$dP.dr/dat$rho.g
} 
##########################################################
# Examine your output and plot the ratio vs R.Rsun. Your
# first plot will show some departures
R_depart = dat$R.Rsun[which.max(dat$ratio.dpdr.rhog)]

#########################################################
# Replot, with ylimits to look at the rest of the data
plot(dat$ratio.dpdr.rhog ~ dat$R.Rsun, ylim=c(0.9,1.1),
     pch=16,
     col=alpha("red",0.35),
     lwd=1.6,
     xlabs = "R/Rsun",ylab = "Ratio of dP/dr to rho*g",
     main = "Departure from Solar Hydrostatic Equilibrium")
abline(h=1,lwd=1.6)

#############################################################
######################  FIN!  ###############################
#############################################################

# Unload contributed packages
# p_unload(all) 

# Unload base packages such as datasets
# detach("package:datasets", unload=TRUE)




