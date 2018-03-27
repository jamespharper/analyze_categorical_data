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
source("functions_complete_analysis_by_province.R")  # Load custom functions
source("functions.R")
load_libraries(c("rio", "gmodels", "vcd", "gtools",  # Install & load libraries
                 "ca", "extracat", "iplots", 
                 "FactoMineR", "gplots", "factoextra",
                 "corrplot", "ggpubr", "rgl", "missMDA",
                 "pscl", "ltm", "Amelia", "ROCR"))
load(file = "iDE_Oct2017.Rdata")
data = subset(data, select = -c())
provinces = unique(data$Prov)

###############################################################################
# ANALYZE DATA BY PROVINCE
###############################################################################
# Loop through all provinces
for (prov in provinces[1]) {
  print(prov)
  
  # Subset data for this province only
  sub = subset(data, Prov == paste(prov))
  # print(summary(sub$Prov))
  # print(summary(sub))
  sub = droplevels(sub)
  
  # Count and visualize NAs
  print(summary(sub))
  print(sapply(sub, function(x) sum(is.na(x))))
  missmap(sub, main = "Missing Values in Variables", legend = F)
  
  # Remove rows missing IntndPitFull response
  sub = subset(sub, !is.na(IntndPitFull))
  print(sapply(sub, function(x) sum(is.na(x))))
  missmap(sub[1:45], main = "Missing Values in Variables", legend = F)
  print(summary(sub))
  
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
  # genlinmod(data = sub, iter = 1)
  # iter = 1
  # iter = 1:iter
  # accuracy = rep(0, times = length(iter))
  # len = length(data[,1])
  # for (i in iter) {
  #   
  #   # Create training (70%) and testing (30%) sets using random sampling
  #   indices_all = 1:len
  #   indices_train = sort(sample(x = indices_all, size = round(0.7*len, 0), 
  #                               replace = F), decreasing = F)
  #   indices_test = indices_all[!(indices_all %in% indices_train)]
  #   # data.frame(indices_train[1:100], indices_test[1:100])
  #   train = data[indices_train,]; test = data[indices_test,]
  #   # print(summary(train)); print(summary(test))
  #   
  #   # Run glm model
  #   model = glm(formula = IntndPitFullDes ~ Dist + IDPoor + VillOD +
  #                 LivRP + VillOD + FreqNeiToi + AdltUseLat + ChldUseLat +
  #                 InfLatDump + Satis + Rec + SatisSup + RecSup + Yr + Mnth +
  #                 RDefBefor_BshFld + IntndChng_Shltr + IntndChng_Shwr +
  #                 IntndChng_Sink + IntndChng_WtrRes + Rain.mm,
  #               data = train,
  #               family = binomial(link = "logit"),
  #               na.action = na.omit)
  #   
  #   # Test predictive power of model on test data
  #   fitted.results = try(predict(object = model, newdata = test, 
  #                                type = "response"), silent = T)
  #   if (inherits(fitted.results, "try-error")) {
  #     next
  #   }
  #   fitted.results = ifelse(fitted.results > 0.5, 1, 0)
  #   # data.frame(test$IntndPitFullDes[!is.na(fitted.results)], 
  #   #            fitted.results[!is.na(fitted.results)])
  #   misclass.error = mean(fitted.results[!is.na(fitted.results)] != 
  #                           test$IntndPitFullDes[!is.na(fitted.results)])
  #   accuracy[i] = 1 - misclass.error
  #   
  #   # Analyze model fit
  #   # if (iter == 1) {
  #     print(summary(model))
  #     print(anova(model, test = "Chisq"))
  #     print(pR2(model))
  #   # }
  #   
  #   # Calculate AUC and plot ROC
  #   pr = prediction(fitted.results, test$IntndPitFullDes)
  #   prf = performance(pr, measure = "tpr", x.measure = "fpr")
  #   plot(prf)
  #   auc = performance(pr, measure = "auc")
  #   auc = auc@y.values[[1]]
  #   print(auc)
  #   
  # }
  # head(accuracy)
  # # hist(accuracy)
  # print(mean(accuracy[accuracy != 0]))
  
}

##########################################################################
# CLEAN UP
##########################################################################
sink()
dev.off()
closeAllConnections()
file.remove(paste(getwd(),"/Output/dump.txt", sep = ""))
