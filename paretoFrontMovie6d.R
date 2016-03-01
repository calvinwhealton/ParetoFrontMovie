# functions and script to read-in data for a 6-d
# Pareto front at several "generations" 
# (number of function evaluations) and generate a
# movie of the result
# dimensions used are 3-space (x,y,z), size, color, and transparency

# libraries
library(scatterplot3d)   # for 3-d scatter plot
library(grDevices)       # some color palettes
library(animation)       # animation of plots
library(phytools)        # for color bar

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
makeParetoMovie(fname=fileName
                ,mnmx=c(rep('min',5),'max'))