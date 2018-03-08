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

###############################################################################
# PERFORM GENERALIZED LINEAR MODELING
###############################################################################
#Examples
# counts <- c(18,17,15,20,10,20,25,13,12)
# outcome <- gl(3,1,9)
# treatment <- gl(3,3)
# print(d.AD <- data.frame(treatment, outcome, counts))
# glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
# anova(glm.D93)
# summary(glm.D93)
# anova(glm.D93, test = "Cp")
# anova(glm.D93, test = "Chisq")
# glm.D93a <- update(glm.D93, ~treatment*outcome) # equivalent to Pearson Chi-square
# anova(glm.D93, glm.D93a, test = "Rao")
# 
# data("Titanic")
# data = Titanic
# train <- data[1:800,]
# test <- data[801:889,]
# model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
# 
# data(mtcars)
# mtcars

names(data)
summary(data)
sapply(data, function(x) sum(is.na(x)))
data_sub = subset(data, !is.na(IntndPitFull))
sapply(data_sub, function(x) sum(is.na(x)))
model = glm(formula = IntndPitFull ~ Prov + CGend + IDPoorTyp,
            data = data_sub,
            family = binomial(link = "logit"),
            na.action = na.omit)
summary(model)
anova(model)
anova(model, test = "Chisq")




