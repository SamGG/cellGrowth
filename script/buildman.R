# build documnetation
# 
# Author: gagneur
###############################################################################

library(roxygen2)
args = commandArgs(TRUE) ## package folder
cat("roxygenize the folder",args[1],"\n")
roxygenize(args[1])
