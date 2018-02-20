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
file_to_import = paste(getwd(), 
                       "/data/data_latowner_6monthpostconstruction.xlsx", 
                       sep = "")
data = import(file_to_import)

# Convert categorical data from characters into factors
for (i in 1:length(names(data))) {
  if (is.character(data[i][[1]])) {
    data[i][[1]] = as.factor(data[i][[1]])
  }
}

###############################################################################
# Correspondence Analysis - Subsets of Districts and Intention When Pit Fills
###############################################################################
# Create temporary vectors and name variables from data
temp = subset(data, Prov == "SR", select = c(Dist, IntndPitFull))
temp = droplevels(temp)
A = temp$Dist
A = substring(A, 4)
B = temp$IntndPitFull
name = "ca_Dist-SR_IntndPitFull"
plot_name = "Dist-SR_IntndPitFull"

# Perform multiple correspondence analysis
results = MCA(data, quali.sup = quali_sup, quanti.sup = quanti_sup)

# Create file name and plot name variables
name = "mca"
plot_name = name

# Start sending text output to text file in folder
file1 = file(paste(getwd(), "/output/", name, ".txt", sep = ""))
sink(file1, append = TRUE)
sink(file1, append = TRUE, type = "message")

# Print results from multiple correspondence analysis
summary(results, ncp = 3, nbelements = Inf)
dimdesc(results)

# Stop saving text output to file
sink()
sink(type = "message")

# Generate plots and store in variables
plot1 = recordPlot(plot(results, label = c("var","quali.sup"), cex = 0.7))
plot2 = recordPlot(plot(results, invisible = c("var","quali.sup"), cex = 0.7))
plot3 = recordPlot(plot(results, invisible = c("ind","quali.sup"), autoLab = "y", cex = 0.7, title = "Active Categories"))
plot4 = recordPlot(plot(results, invisible = c("ind","quali.sup"), autoLab = "y", cex = 0.7, title = "Active Categories", selectMod = "contrib 20"))
plot5 = recordPlot(plot(results, invisible = c("ind","quali.sup"), cex = 0.7, title = "Active Categories"))
plot6 = recordPlot(plot(results, invisible = c("ind","var"), autoLab = "y", cex = 0.7, title = "Supplementary Categories"))
plot7 = recordPlot(plot(results, invisible = "ind", autoLab = "y", cex = 0.7, selectMod = "cos2 10"))
plot8 = recordPlot(plot(results, invisible = "ind", autoLab = "y", cex = 0.7, selectMod = "contrib 20"))
plot9 = recordPlot(plot(results, invisible = c("var","quali.sup"), autoLab = "y", cex = 0.7, select = "cos2 10"))
plot10 = recordPlot(plot(results, autoLab = "y", cex = 0.7, selectMod = "cos2 20", select = "cos2 10"))
plot11 = recordPlot(plot(results, choix = "var", xlim = c(0,0.6), ylim = c(0,0.6)))
plot12 = recordPlot(plot(results, choix = "var", xlim = c(0,0.6), ylim = c(0,0.6), invisible = c("ind","quali.sup")))
plot13 = recordPlot(plot(results, invisible = c("var","quali.sup"), cex = 0.7, select = "contrib 20", axes = 3:4))
plot14 = recordPlot(plot(results, invisible = c("ind"), cex = 0.7, select = "contrib 20", axes = 3:4))
plot15 = recordPlot(plotellipses(results, keepvar = c(1:4)))