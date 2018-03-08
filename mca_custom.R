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
                 "corrplot", "ggpubr", "rgl", "missMDA"))

###############################################################################
# LOAD, CLEAN AND CATEGORIZE DATA
###############################################################################
# Load data file
load(file = "iDE_Oct2017.RData")
# file_to_import_mca = paste(getwd(), "/data/data_mca_noNAs.xlsx", sep = "")
# data = import(file_to_import_mca)
# data = read.table(file_to_import_mca, header = TRUE, sep = ",")
# names(data)[1] = "IntndPitFull"
# data(tea)
# data(poison)
# head(poison, 3)

# Modify variable responses for MCA


# Categorize data as active or supplementary (qualitative or quantitative)
names(data)
data.active = subset(data, select = c("IntndPitFull"))    # c(1:8), c(1:3, 5:8)
data.sup.quali = ""    # data[ , 9:46]
data.sup.quanti = ""    # data[ , 47:56]

###############################################################################
# PERFORM MULTIPLE CORRESPONDENCE ANALYSIS 
###############################################################################
# Summarize data
summary(data.active)
summary(data.sup.quanti)
summary(data.sup.quali)

for (i in 1:length(data.active)) {
  plot(data.active[, i], main = colnames(data.active)[i],
       ylab = "Count", col = "steelblue")
}

# Impute data due to missing data (NOT WORKING)
require(missMDA)
data(vnf)
completed = imputeMCA(vnf, ncp = 2)
res.mca = MCA(vnf)
res.mca = MCA(vnf,tab.disj=completed$tab.disj)


# Perform multiple correspondence analysis
results = MCA(X = data.active, na.method = "NA")
# imputed = imputeMCA(don = data.active, ncp = 2)
# results = MCA(X = data.active, 
#               quali.sup = data.sup.quali,
#               quanti.sup = data.sup.quanti,
#               tab.disj = imputed$tab.disj)
# results = MCA(tea, quanti.sup = 19, quali.sup = c(20:36))

# Print results of multiple correspondence analysis
print(results)



# Tea example
data(tea)
res.mca = MCA(tea, quanti.sup=19, quali.sup=c(20:36))
plot.MCA(res.mca, invisible=c("var","quali.sup"), cex=0.7)
plot.MCA(res.mca, invisible=c("ind","quali.sup"), cex=0.7)
plot.MCA(res.mca, invisible=c("ind"))
plot.MCA(res.mca, invisible=c("ind", "var"))



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