# Analysis of Surveys of Rural Cambodian Latrine Owners
# Written by James Harper, PE, ENV SP of the University of Colorado Boulder
# Started October 1, 2017
# Last updated March 26, 2018

###############################################################################
# INITIALIZE, LOAD DATA AND CLEAN DATA
###############################################################################
rm(list = ls())                                      # Clear global environment
cat("\014")                                          # Clear console window
file.remove(dir(paste(getwd(),"/output/", sep = ""), # Clear output folder
                full.names = TRUE))    
source("functions_complete_analysis_iDE_Oct2017.R")  # Load custom functions
source("functions.R")
load_libraries(c("rio", "gmodels", "vcd", "gtools",  # Install & load libraries
                 "ca", "extracat", "iplots", 
                 "FactoMineR", "gplots", "factoextra",
                 "corrplot", "ggpubr", "rgl", "missMDA",
                 "pscl", "ltm", "Amelia", "ROCR"))
load(file = "iDE_Oct2017.Rdata")
data = subset(data, select = -c(IntndChngDich, IntndChng, IntndChng_Shltr,
                                IntndChng_Shwr, IntndChng_Sink, IntndChng_Pit,
                                IntndChng_WtrRes, IntndChng_Othr,
                                IntndChng_NAAlwysToi, RDefBefor))

###############################################################################
# ANALYZE DATA
###############################################################################
# Characterize data, focusing on NAs
print(summary(data))
# print(sapply(data, function(x) sum(is.na(x))))
# missmap(data, main = "Missing Values in Variables", legend = F)

# Remove rows missing IntndPitFull response
data = subset(data, !is.na(IntndPitFull))
# print(sapply(data, function(x) sum(is.na(x))))
# missmap(data[1:35], main = "Missing Values in Variables", legend = F)
# print(summary(data))

# Run 1-way frequency analysis on selected variable
# names(data)
# var1 = c(2:length(data))
# for (num in var1) {
#   print(num)
#   freqs.1way(data = data, metric1 = num, prov = prov)
# }

# Run 2-way frequency and correspondence analyses on selected variable pairs
# names(data)
# var1 = c(17, 38:44)
# var2 = c(2, 5, 6, 23, 24)
# pairs = expand.grid(var1, var2)
# for (num in 1:length(pairs[,1])) {
#   print(paste(pairs[num,1], "_", pairs[num,2]))
#   freqs.2way(data = data, metric1 = pairs[num,1], metric2 = pairs[num,2], 
#              prov = prov)
#   # correspondence(data = data, metric1 = pairs[num,1], metric2 = pairs[num,2], 
#   #    prov = prov)     # NOT WORKING
# }

# Run multiple correspondence analysis
# multiple.correspondence(data = data[2:length(data)],
#                         quali_sup = c(3:4, 7, 10, 12, 13, 18),
#                         quanti_sup = c())   # NOT WORKING

# Run generalized linear model
genlinmod(data = data, iter = 1)

###############################################################################
# ANALYZE DATA BY PROVINCE
###############################################################################
provinces = unique(data$Prov)
for (prov in provinces) {
  print(prov)
  
  # Subset data for this province only
  sub = subset(data, Prov == paste(prov))
  # print(summary(sub$Prov))
  # print(summary(sub))
  sub = droplevels(sub)
  
  # Characterize data for this province, focusing on NAs
  print(summary(sub))
  # print(sapply(sub, function(x) sum(is.na(x))))
  # missmap(sub, main = "Missing Values in Variables", legend = F)
  
  # Remove rows missing IntndPitFull response
  sub = subset(sub, !is.na(IntndPitFull))
  # print(sapply(sub, function(x) sum(is.na(x))))
  # missmap(sub[1:35], main = "Missing Values in Variables", legend = F)
  # print(summary(sub))
  
  # Run 1-way frequency analysis on selected variable
  # names(sub)
  # var1 = c(2:length(sub))
  # for (num in var1) {
  #   print(num)
  #   freqs.1way(data = sub, metric1 = num, prov = prov)
  # }
  
  # Run 2-way frequency and correspondence analyses on selected variable pairs
  # names(sub)
  # var1 = c(17, 38:44)
  # var2 = c(2, 5, 6, 23, 24)
  # pairs = expand.grid(var1, var2)
  # for (num in 1:length(pairs[,1])) {
  #   print(paste(pairs[num,1], "_", pairs[num,2]))
  #   freqs.2way(data = sub, metric1 = pairs[num,1], metric2 = pairs[num,2], 
  #              prov = prov)
  #   # correspondence(data = sub, metric1 = pairs[num,1], metric2 = pairs[num,2], 
  #   #    prov = prov)     # NOT WORKING
  # }
  
  # Run multiple correspondence analysis
  # multiple.correspondence(data = sub[2:length(sub)],
  #                         quali_sup = c(3:4, 7, 10, 12, 13, 18),
  #                         quanti_sup = c())   # NOT WORKING
  
  # Run generalized linear model
  genlinmod(data = sub, iter = 1)
  
}

##########################################################################
# CLEAN UP
##########################################################################
sink()
dev.off()
closeAllConnections()
file.remove(paste(getwd(),"/Output/dump.txt", sep = ""))
