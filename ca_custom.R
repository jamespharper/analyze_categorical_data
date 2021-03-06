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

# Add title to text file
print(paste("A = Dist-SR", sep = ""))
print(paste("B = IntndPitFull", sep = ""))

# Perform correspondence analyses
CrossTable(A, B)
freqs = table(A, B)
print(freqs)
print(prop.table(freqs, 1))
print(prop.table(freqs, 2))

freqs.ca = CA(freqs, graph = TRUE)       # FactoMineR package
fviz_ca_biplot(freqs.ca, repel = TRUE)
fviz_ca_biplot(freqs.ca, 
               map = "rowprincipal", arrow = c(TRUE, TRUE),
               repel = TRUE)
fviz_ca_biplot(freqs.ca, 
               map = "symbiplot", arrow = c(TRUE, TRUE),
               repel = TRUE)
fviz_ca_biplot(freqs.ca, 
               map = "colgab", arrow = c(TRUE, TRUE),
               repel = TRUE)
fviz_ca_biplot(freqs.ca, 
               map = "colgreen", arrow = c(TRUE, TRUE),
               repel = TRUE)
fviz_screeplot(freqs.ca, addlabels = TRUE, ylim = c(0, 50))
fviz_ca_row(freqs.ca, repel = TRUE)
fviz_ca_row(freqs.ca, col.row = "steelblue", shape.row = 15)
fviz_ca_row(freqs.ca, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)
row = get_ca_row(freqs.ca)
corrplot(row$cos2, is.corr = FALSE)
fviz_cos2(freqs.ca, choice = "row", axes = 1:2)
head(row$contrib)
corrplot(row$contrib, is.corr = FALSE)    
fviz_contrib(freqs.ca, choice = "row", axes = 1, top = 10)
fviz_contrib(freqs.ca, choice = "row", axes = 2, top = 10)
fviz_contrib(freqs.ca, choice = "row", axes = 1:2, top = 10)
fviz_ca_row(freqs.ca, col.row = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)
col = get_ca_col(freqs.ca)
fviz_ca_col(freqs.ca, col.col = "cos2", 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)
fviz_cos2(freqs.ca, choice = "col", axes = 1:2)
fviz_contrib(freqs.ca, choice = "col", axes = 1:2)

# I don't have much of this kind of data
data(children)
freqs.ca = CA(children, row.sup = 15:18, col.sup = 6:8,
              graph = FALSE)
fviz_ca_biplot(freqs.ca, repel = TRUE)


freqs.ca = ca(freqs)                      # ca package
print(freqs.ca)
print(summary(freqs.ca))
plot(freqs.ca, main = plot_name) # symmetric map
plot(freqs.ca, main = plot_name, lines = TRUE)
plot(freqs.ca, main = plot_name, arrows = c(TRUE, FALSE))
plot(freqs.ca, main = plot_name, mass = TRUE, contrib = "absolute", 
     map = "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map
plot(freqs.ca, dim = c(1,2), map = "symmetric", what = c("all", "all"),
     mass = c(TRUE, TRUE), contrib = c("relative", "relative"),
     col = c("blue", "red"), pch = c(16, 21, 17, 24),
     labels = c(2, 2), laboffset = c(0, 0),
     arrows = c(FALSE, FALSE), lines = c(FALSE, FALSE), lwd = 1,
     xlab = "_auto_", ylab = "_auto_")
plot3d.ca(freqs.ca, dim = c(1, 2, 3), map = "symmetric", what = c("all", "all"), 
          contrib = c("none", "none"), col = c("#6666FF","#FF6666"), 
          labcol  = c("#0000FF", "#FF0000"), pch = c(16, 1, 18, 9), 
          labels = c(2, 2), sf = 0.00002, arrows  = c(FALSE, FALSE))

###############################################################################
# Correspondence Analysis - Subsets of Districts and Intention When Pit Fills
###############################################################################




freqs_prov = subset(data,
                    Prov != "Takeo" & Prov != "Kampong Cham" &
                      Prov != "Kampong Speu" & Prov != "Phnom Penh" &
                      Yr != "2014",
                    select = c("Prov", "IntndChng_Shltr",
                               "IntndChng_Shwr", "IntndChng_HndWsh",
                               "IntndChng_WtrRes", "IntndChng_2pit",
                               "IntndChng_Othr", "IntndChng_NAAlwysToi"))
freqs_prov = droplevels(freqs_prov)
summary(freqs_prov, maxsum = 15)
