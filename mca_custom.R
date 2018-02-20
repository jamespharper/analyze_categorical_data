###############################################################################
# INITIALIZE AND LOAD DATA
###############################################################################
rm(list = ls())                                      # Clear global environment
cat("\014")                                          # Clear console window
file.remove(dir(paste(getwd(),"/output/", sep = ""), # Clear output folder
                full.names = TRUE))    
source("functions.R")                                # Load custom functions
load_libraries(c("rio", "gmodels", "vcd", "gtools",  # Install & load libraries
                 "ca", "extracat", "iplots", 
                 "FactoMineR", "gplots", "factoextra",
                 "corrplot", "ggpubr", "rgl"))

###############################################################################
# LOAD, CLEAN AND CATEGORIZE DATA
###############################################################################
file_to_import_mca = paste(getwd(), "/data/data_mca.csv", sep = "")
data = import(file_to_import_freqs_mca)
data = read.table(file_to_import_mca, header = TRUE, sep = ",")
names(data)[1] = "IntndPitFull"
data(tea)
data(poison)
head(poison, 3)

# Convert categorical data from characters into factors
for (i in 1:length(names(data))) {
  if (is.character(data[i][[1]])) {
    data[i][[1]] = as.factor(data[i][[1]])
  }
}

# Categorize data
data.active = data[ , 1:10]
data.sup.quanti = data[ , 21:30]
data.sup.quali = data[ , 11:20]

###############################################################################
# PERFORM MULTIPLE CORRESPONDENCE ANALYSIS 
###############################################################################
# Summarize data
summary(data.active)
summary(data.sup.quanti)
summary(data.sup.quali)

# Perform multiple correspondence analysis
results = MCA(data.active, quanti.sup = data.sup.quanti, quali.sup = data.sup.quali)
results = MCA(tea, quanti.sup = 19, quali.sup = c(20:36))


# Create temporary vectors and name variables from data
name = "mca"
plot_name = name

# Print results from multiple correspondence analysis
summary(results, ncp = 3, nbelements = Inf)
dimdesc(results)

# Generate plots and store in variables
plot.MCA(results, invisible = c("ind", "quanti.sup", "quali.sup"))   # Variables
plot.MCA(results, invisible = c("var", "quanti.sup", "quali.sup"))   # Individuals
plot.MCA(results, invisible = c("var", "ind", "quali.sup"), cex = 0.7)
plot.MCA(results, invisible = c("var", "ind", "quanti.sup"), cex = 0.7)
plot(results, label = c("var","quali.sup"), cex = 0.7)
plot(results, label = c("var"), cex = 0.7)
plot(results, invisible = c("var","quali.sup"), cex = 0.7)
plot(results, invisible = c("ind","quali.sup"), autoLab = "y", cex = 0.7, title = "Active Categories")
plot(results, invisible = c("ind","quali.sup"), autoLab = "y", cex = 0.7, title = "Active Categories", selectMod = "contrib 20")
plot(results, invisible = c("ind","quali.sup"), cex = 0.7, title = "Active Categories")
plot(results, invisible = c("ind","var"), autoLab = "y", cex = 0.7, title = "Supplementary Categories")
plot(results, invisible = "ind", autoLab = "y", cex = 0.7, selectMod = "cos2 10")
plot(results, invisible = "ind", autoLab = "y", cex = 0.7, selectMod = "contrib 20")
plot(results, invisible = c("var","quali.sup"), autoLab = "y", cex = 0.7, select = "cos2 10")
plot(results, autoLab = "y", cex = 0.7, selectMod = "cos2 20", select = "cos2 10")
plot(results, choix = "var", xlim = c(0,0.6), ylim = c(0,0.6))
plot(results, choix = "var", xlim = c(0,0.6), ylim = c(0,0.6), invisible = c("ind","quali.sup"))
plot(results, invisible = c("var","quali.sup"), cex = 0.7, select = "contrib 20", axes = 3:4)
plot(results, invisible = c("ind"), cex = 0.7, select = "contrib 20", axes = 3:4)
plotellipses(results, keepvar = c(1:4))