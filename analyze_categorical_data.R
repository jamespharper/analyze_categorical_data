# Analysis of Surveys of Rural Cambodian Latrine Owners
# Written by James Harper, PE, ENV SP of the University of Colorado Boulder
# Started October 1, 2017
# Last updated February 13, 2018
# Useful References: https://www.statmethods.net/stats/frequencies.html, 
#                    www.mit.edu/~6.s085/notes/lecture6.pdf 

##########################################################################
# INITIALIZE
##########################################################################
rm(list = ls())                                                             # Clear global environment
cat("\014")                                                                 # Clear console window
file.remove(dir(paste(getwd(),"/Output/", sep = ""), full.names = TRUE))    # Clear output folder of all files
source("functions.R")                                                       # Load custom functions
load_libraries(c("rio", "gmodels", "vcd", "gtools", "ca", "extracat",       # Install and/or load libraries
                 "iplots", "FactoMineR"))

##########################################################################
# USER INPUT
##########################################################################
# Select file to import
file_to_import = "data_latowner_6monthpostconstruction.xlsx" 

# Select tests to run
Freqs_1way = 1        # Frequency analysis of individual variables
Freqs_2way = 0        # Frequency analysis of pairs of variables
Freqs_3way = 0        # Frequency analysis of triplets of variables
CA = 0                # Correspondence Analysis
MCA = 0               # Multiple Correspondence Analysis

# Define variables; set equal to 0 to ignore
var_interest = 0      # Variables of interest for two/three-way analyses
stratifiers = 0       # Stratifiers for three-way analysis
quali_sup = 0         # Qualitative supplementary variables for CA
quanti_sup = 0        # Quantitative supplementary variables for CA

##########################################################################
# CHECK FOR PROBLEMS WITH USER INPUT
##########################################################################
# Check if more than one test is selected


##########################################################################
# IMPORT AND CLEAN DATA
##########################################################################
# Import raw data
data = import(file_to_import)
# data = read.table("data_MCA_noneg.csv", header = TRUE, sep = ",")
# names(data)[1] = "RDefBefrNeiToi"

# Clean data
if (file_to_import == "data_latowner_6monthpostconstruction.xlsx") {
  data = data[,-c(1,4,7:10,15:24)]                                        # Remove unneeded data
}

# Convert categorical data from characters into factors
for (i in 1:length(names(data))) {
  if (is.character(data[i][[1]])) {
    data[i][[1]] = as.factor(data[i][[1]])
  }
}
# str(data)
# summary(data)

##########################################################################
# ANALYZE DATA
##########################################################################
# Run 1-way categorical analyses
if (Freqs_1way == 1) {
  print("Running one-way frequency analyses...")
  for (num in 1:length(data)) {
    print(num)
    categorical_analysis_1way(data, num)
  }
}

if (Freqs_2way == 1) {
  print("Running two-way frequency analyses...")
  
  # Create permutations of metrics to test for association
  metrics_2way = permutations(n = length(data), r = 2, v = 1:length(data), repeats.allowed = FALSE)
  if (var_interest[[1]] != 0) {
    temp = list()
    for (i in 1:length(var_interest)) {
      if (length(temp) == 0) {
        temp = metrics_2way[metrics_2way[,1] == var_interest[[i]],]
      } else {
        temp = rbind(temp, metrics_2way[metrics_2way[,1] == var_interest[[i]],])
      }
    }
    metrics_2way = temp
  }

  # Run 2-way categorical analyses
  for (num in 1:length(metrics_2way[,1])) {
    print(paste(metrics_2way[num,1],"_",metrics_2way[num,2]))
    categorical_analysis_2way(data, metrics_2way[num,1], metrics_2way[num,2])
  }
}

##########################################################################
# ANALYZE DATA - TEST FOR ASSOCIATIONS WITH STRATIFICATION
##########################################################################
if (Run_Tests_for_Associations_with_Stratification == 1) {
  print("Running tests for association with stratification...")
  
  # Run stratified association analyses
  if (Run_OneWay_Tests == 1) {
    
    # Loop through selected stratifiers
    for (stratifier in stratifiers) {
      print(paste("Running one-way tests stratified by ", names(data[stratifier]), sep = ""))
      
      # Create combinations of metrics to test for association
      strat_array = rep(stratifier, times = length(data))
      data_array = 1:length(data)
      combos = cbind(strat_array, data_array)
      combos = combos[-stratifier,]
      # if (var_interest != 0) {                                             # If a variable of interest has been selected,
      #   combos = combos[metrics_2way[,2] == var_interest,]     # Select only permutations with variable of interest first
      # }
      # print(combos)
      
      # Run 1-way categorical analyses stratified by this stratifier
      # (same as unstratified two-way categorical analyses between stratifier and all other variables)
      for (num in 1:length(combos[,1])) {
        print(paste(combos[num,1],"_",combos[num,2]))
        categorical_analysis_2way(data, combos[num,1], combos[num,2])
      }
    }
  }

  if (Run_TwoWay_Tests == 1) {
    
    # Loop through selected stratifiers
    for (stratifier in stratifiers) {
    
      print(paste("Running two-way tests stratified by ", names(data[stratifier]), sep = ""))
  
      # Create combinations of metrics to test for association
      metrics_2way = permutations(n = length(data) - 8, r = 2, v = 9:length(data), repeats.allowed = FALSE)
      if (var_interest[[1]] != 0) {
        temp = list()
        for (i in 1:length(var_interest)) {
          if (length(temp) == 0) {
            temp = metrics_2way[metrics_2way[,1] == var_interest[[i]],]
          } else {
            temp = rbind(temp, metrics_2way[metrics_2way[,1] == var_interest[[i]],])
          }
        }
        metrics_2way = temp
        # print(metrics_2way)
      }
      # metrics_2way = data.frame(metrics_2way)
      # metrics_2way = subset(metrics_2way, X1 != stratifier)
      # metrics_2way = subset(metrics_2way, X2 != stratifier)
  
      # Run 2-way categorical analyses stratified by this stratifier
      for (num in 1:length(metrics_2way[,1])) {
      # for (num in 1:1) {
        print(paste(metrics_2way[num,1], "_", metrics_2way[num,2], "_", stratifier))
        categorical_analysis_3way(data, metrics_2way[num,1], metrics_2way[num,2], stratifier)
      }
    }
  }
    
}

##########################################################################
# ANALYZE DATA - CORRESPONDENCE ANALYSIS
##########################################################################
if (Run_Correspondence_Analyses == 1) {
  print("Running correspondence analyses...")

  # Create combinations of metrics to test for correspondences
  metrics_2way = permutations(n = length(data), r = 2,
                              v = 1:length(data),
                              repeats.allowed = FALSE)
  head(metrics_2way)
  metrics_2way = metrics_2way[-c(1),]
  head(metrics_2way)

  # Run correspondence analyses
  for (num in 1:length(metrics_2way[,1])) {
    print(paste(metrics_2way[num,1],"_",metrics_2way[num,2]))
    correspondence_analysis_2way(data, metrics_2way[num,1], metrics_2way[num,2])
  }
}

if (Run_Multiple_Correspondence_Analysis == 1) {
  print("Running multiple correspondence analysis...")
  
  # Run multiple correspondence analysis
  multiple_correspondence_analysis(data, quali_sup, quanti_sup)
}

##########################################################################
# CLEAN UP
##########################################################################
sink()
dev.off()
closeAllConnections()
file.remove(paste(getwd(),"/Output/dump.txt", sep = ""))