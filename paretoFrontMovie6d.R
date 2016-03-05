# functions and script to read-in data for a 6-d
# Pareto front at several "generations" 
# (number of function evaluations) and generate a
# movie of the result
# dimensions used are 3-space (x,y,z), size, color, and transparency

# libraries
library(plot3D)          # 3-d scatter plot
library(grDevices)       # color palettes and transparency
library(animation)       # animation of plots
library(phytools)        # color bar

######################################
# User-Defined Functions Loaded-in
######################################

# setting working directory for user-defined function files
# note: Windows needs to use \\ because \ is an escape character
setwd('/Users/calvinwhealton/GitHub/ParetoFrontMovie')

source("colSizeTrans.R")
source("dataClean.R")
source("makeParetoMovie.R")

######################################
# Main Portion of Code
######################################
# setting working directory for input file
# note: Windows needs to use \\ because \ is an escape character
setwd('/Users/calvinwhealton/Desktop')

# file name, CHANGE !!!!!!!!!!!!!!!!!!!!
fileName <- "susq_256_16I_1000MC.runtime.txt"

# calling function to make movie
# no ideal point
# mainly default values
df_calcs1 <- makeParetoMovie(fname=fileName
                              ,mnmx=c(rep('min',5),'max') # first 5 columns were minimization
                              )


# calling function to make movie
# ideal point
# many adjusted values
df_calcs2 <- makeParetoMovie(fname=fileName
                              ,mnmx=c(rep('min',5),'max')  # first 5 columns were minimization
                              ,idealAdd=T                  # adding ideal point
                              ,aniName ='ParetoFront2.mp4' # name for animimation
                              ,aniRot='all'                # rotating all points
                              ,colPal=topo.colors(10)      # new color palette
                             ,idealMult = 8               # size multiplier for the ideal point
                             ,idealPch=18
                             )