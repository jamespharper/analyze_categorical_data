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
source("functions.R")
load_libraries(c("rio", "gmodels", "vcd", "gtools",  # Install & load libraries
                 "ca", "extracat", "iplots", 
                 "FactoMineR", "gplots", "factoextra",
                 "corrplot", "ggpubr", "rgl", "missMDA",
                 "pscl", "ltm", "Amelia", "ROCR", "extrafont"))
loadfonts(device = "win")
load(file = "iDE_Oct2017.Rdata")
data = subset(data, select = -c(IntndChngDich, IntndChng, IntndChng_Shltr,
                                IntndChng_Shwr, IntndChng_Sink, IntndChng_Pit,
                                IntndChng_WtrRes, IntndChng_Othr,
                                IntndChng_NAAlwysToi, RDefBefor))

# Characterize data, focusing on NAs
summary(data)
names(data)
# print(sapply(data, function(x) sum(is.na(x))))
# missmap(data, main = "Missing Values in Variables", legend = F)

# Remove rows missing IntndPitFull response
data = subset(data, !is.na(IntndPitFull))
# print(sapply(data, function(x) sum(is.na(x))))
# missmap(data[1:35], main = "Missing Values in Variables", legend = F)
# print(summary(data))

###############################################################################
# ANALYZED DATA
###############################################################################

# Run 1-way frequency analysis on selected variable
names(data)
var1 = c(2:length(data))
for (num in var1) {
  print(num)
  freqs.1way(data = data, metric1 = num)
}

# Run 2-way frequency and correspondence analyses on selected variable pairs
names(data)
var1 = c(28)
var2 = c(1:2, 6:13, 15:27, 35:42)
pairs = expand.grid(var1, var2)
for (num in 1:length(pairs[,1])) {
  print(paste(pairs[num,1], "_", pairs[num,2]))
  freqs.2way(data = data, metric1 = pairs[num,1], metric2 = pairs[num,2])
  # correspondence(data = data, metric1 = pairs[num,1], metric2 = pairs[num,2],
  #    prov = prov)     # NOT WORKING
}

# Run multiple correspondence analysis
# multiple.correspondence(data = data[2:length(data)],
#                         quali_sup = c(3:4, 7, 10, 12, 13, 18),
#                         quanti_sup = c())   # NOT WORKING

# Run generalized linear model
data.sub = subset(data, select = -c(Comm, Vill))
genlinmod(data = data.sub, iter = 1)
iter = 1
iter = 1:iter
accuracy = rep(0, times = length(iter))
len = length(data.sub[,1])
# for (i in iter) {

# Create training (50%) and testing (50%) sets using random sampling
indices_all = 1:len
continue = 0
while (continue == 0) {
  indices_train = sort(sample(x = indices_all, size = round(0.95*len, 0),
                              replace = F), decreasing = F)
  indices_test = indices_all[!(indices_all %in% indices_train)]
  # data.frame(indices_train[1:100], indices_test[1:100])
  train = data.sub[indices_train,]; test = data.sub[indices_test,]
  # print(summary(train)); print(summary(test))
  
  # Check if each variable in training and testing sets has two factors
  continue = 1
  for (var in 1:(length(data.sub) - 1)) {
    if (var == 33 | var == 41 | var == 42 | var == 43 ) {next}
    if (nlevels(train[,var]) < 2) { print(paste("Train", var)); continue = 0 }
    if (nlevels(test[,var]) < 2) { print(paste("Test", var)); continue = 0 }
  }
}

# Run glm model
model = glm(formula = IntndPitFullDes ~ Prov + CGend + IDPoor + LivRP + 
              VillOD + FreqNeiToi + 
              Satis + Rec + SatisSup + RecSup + Yr + Mnth +
              RDefBefor_BshFld + Rain.mm,
            data = train,
            family = binomial(link = "logit"),
            na.action = na.omit)
# Removed due to low freqs: RDefBefor_NeiToi, RDefBefor_RivPnd
# Removed due to low info: AdltUseLat, ChldUseLat, InfLatDump

# Analyze model fit
# if (iter == 1) {
# print(summary(model))
print(anova(model, test = "Chisq"))
print(pR2(model))











# By year
data.sub = subset(data, Yr != 2014)
data.sub = droplevels(data.sub)
freqs_2way_IntndPitFull_Yr = freqs.2way(data.sub, "IntndPitFull", "Yr", 1)
freqs_2way_IntndPitFullDK_Yr = freqs.2way(data.sub, "IntndPitFullDK", "Yr", 1)
freqs_2way_IntndPitFullEmpSlf_Yr = freqs.2way(data.sub, "IntndPitFullEmpSlf", "Yr", 1)
freqs_2way_IntndPitFullPit_Yr = freqs.2way(data.sub, "IntndPitFullPit", "Yr", 1)
freqs_2way_IntndPitFullOthr_Yr = freqs.2way(data.sub, "IntndPitFullOthr", "Yr", 1)
freqs_2way_IntndPitFullPay_Yr = freqs.2way(data.sub, "IntndPitFullPay", "Yr", 1)
freqs_2way_IntndPitFullStop_Yr = freqs.2way(data.sub, "IntndPitFullStop", "Yr", 1)
crosstable_IntndPitFull_Yr = CrossTable(freqs_2way_IntndPitFull_Yr[[2]])
print(crosstable_IntndPitFull_Yr)
barplot(crosstable_IntndPitFull_Yr$prop.col, beside = T,
        col = c(1:6), family = "serif", axes = T,
        legend.text = c("Don't know", "Empty myself", "Install new pit",
                        "Other", "Pay someone", "Stop using latrine"), 
        args.legend = list(x = "topright", bty = "n"))
df = data.frame(IntndPitFull = crosstable_IntndPitFull_Yr$prop.col)
ggplot(df, aes(x = IntndPitFull.B, y = IntndPitFull.Freq, group = IntndPitFull.A)) + 
  geom_line(aes(color = IntndPitFull.A))
# NOT SURE WHY SCRIPTS BELOW DON'T GIVE p and v
# data_sub = subset(data, IntndPitFull == "Pay", select = c("IntndPitFull", "Yr"))
# data_sub = droplevels(data_sub)
# summary(data_sub)
# assocstats(table(data_sub$IntndPitFull, data_sub$Yr))

# Analyze FSM intentions by month only in 2017
data.sub = subset(data, YrMnth == 201610 | YrMnth == 201611 |
                    YrMnth == 201612 | YrMnth == 201701 |
                    YrMnth == 201702 | YrMnth == 201703 |
                    YrMnth == 201704 | YrMnth == 201705 |
                    YrMnth == 201706 | YrMnth == 201707 |
                    YrMnth == 201708 | YrMnth == 201709, 
                  select = c(YrMnth, IntndPitFull, Mnth, Prov))
summary(data.sub, maxsum = 100)
length(data.sub$IntndPitFull)
freqs.2way.IntndPitFull.Mnth = freqs.2way(data = data.sub, 
                                          metric1 = "IntndPitFull",
                                          metric2 = "Mnth", return = 1)
# THE FOLLOWING COMMENTED CODE WEIGHTS THE COUNTS OF EACH FSM INTENTION PER 
# MONTH BY THE NUMBER OF SURVEYS IN THAT MONTH.  BUT, CROSSTABLE() IS ALREADY
# DOING THIS; THUS, THE FOLLOWING CODE IS COMMENTED OUT AND UNNECESSARY.
# summary(data.sub$YrMnth)
# summary(data.sub$Mnth)
# avg.n.survey.mnth = mean(summary(data.sub$Mnth))
# freqs.2way.IntndPitFull.Mnth[[2]]
# for (i in 1:length(summary(data.sub$YrMnth))) {
#   # print(i)
#   freqs.2way.IntndPitFull.Mnth[[2]][,i] = 
#     round(freqs.2way.IntndPitFull.Mnth[[2]][,i] * avg.n.survey.mnth / 
#             summary(data.sub$Mnth)[i], digits = 0)
#   print(sum(freqs.2way.IntndPitFull.Mnth[[2]][,i]))
# }
# freqs.2way.IntndPitFull.Mnth[[2]]
crosstable.IntndPitFull.Mnth = CrossTable(freqs.2way.IntndPitFull.Mnth[[2]])
print(crosstable.IntndPitFull.Mnth)
# BAD BARPLOT USING BUILT-IN PLOT FUNCTION
# barplot(crosstable.IntndPitFull.Mnth$prop.col, beside = F,
#         col = c(1:6), family = "serif", axes = T,
#         legend.text = c("Don't know", "Empty myself", "Install new pit",
#                         "Other", "Pay someone", "Stop using latrine"),
#         args.legend = list(x = "topright", bty = "n"))
prop.col = crosstable.IntndPitFull.Mnth$prop.col
typeof(prop.col)
order = c("Pay", "Pit", "EmpSlf", "DK", "Stop", "Othr")
prop.col = prop.col[match(order, row.names(prop.col)),,drop = FALSE]
df = data.frame(IntndPitFull = prop.col)
names(df) = c("IntndPitFull", "Mnth", "Prop")
df$Desir = rep(x = c("Des", "Des", "Undes", "Undes", "Undes", "Undes"), times = 12)
ggplot(df, aes(x = Mnth, y = Prop, fill = IntndPitFull)) +
  geom_col() + 
  coord_flip()
windowsFonts()
title = "                       Desirable                                             Undesirable"
data.sub.des = subset(df, Desir == "Des")
data.sub.undes = subset(df, Desir == "Undes")
perc_des = round(data.sub.des$Prop[data.sub.des$IntndPitFull == "Pay"] +
                   data.sub.des$Prop[data.sub.des$IntndPitFull == "Pit"], 2)*100
perc_undes = 100 - perc_des
ggplot(df, aes(x = Mnth)) + 
  geom_col(data = data.sub.des, aes(y = -Prop, fill = IntndPitFull)) + 
  geom_col(data = data.sub.undes, aes(y = Prop, fill = IntndPitFull)) + 
  coord_flip() + 
  geom_hline(yintercept = 0, colour = "white", lwd = 1) + 
  scale_x_discrete(name = "Month", 
                   breaks = unique(df$Mnth),
                   limits = rev(unique(df$Mnth)),
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(name = "Percentage of rural latrine owners", 
                     breaks = round(seq(-0.8, 0.8, 0.1), 1), 
                     limits = c(-0.8, 0.8),
                     labels = paste(as.character(abs(seq(-80, 80, 10))), "%", sep = "")) +
  scale_fill_discrete(name = "Intention when Pit Fills: ",
                      breaks = order,
                      labels = c("Pay professional",
                                 "Install a new pit",
                                 "Self-empty",
                                 "Don't know",
                                 "Stop using latrine",
                                 "Other")) +
  ggtitle(title) + 
  theme_bw() +
  theme(text = element_text(family = "serif", size = 14), 
        legend.position = "bottom",
        legend.direction = "horizontal",
        panel.background = element_blank()) +
  annotate(geom = "text", size = 4, family = "serif",
           x = unique(df$Mnth), y = -perc_des/100 - 0.04, 
           label = paste(as.character(perc_des), "%", sep = "")) + 
  annotate(geom = "text", size = 4, family = "serif",
           x = unique(df$Mnth), y = perc_undes/100 + 0.05, 
           label = paste(as.character(perc_undes), "%", sep = ""))
ggplot(df, aes(x = Mnth, y = Prop, group = IntndPitFull)) + 
  geom_line(aes(color = IntndPitFull), size = 2) +
  guides(linetype = guide_legend(override.aes = list(alpha = 1)))
mean(perc_des)
sd(perc_des)

# Plot FSM intentions by YrMnth
data.sub = subset(data, select = c(YrMnth, IntndPitFull))
freqs.2way.IntndPitFull.YrMnth = freqs.2way(data = data.sub, 
                                            metric1 = "IntndPitFull",
                                            metric2 = "YrMnth", return = 1)
crosstable.IntndPitFull.YrMnth = CrossTable(freqs.2way.IntndPitFull.YrMnth[[2]])
print(crosstable.IntndPitFull.YrMnth)
prop.col = crosstable.IntndPitFull.YrMnth$prop.col
df = data.frame(IntndPitFull = prop.col)
names(df) = c("IntndPitFull", "YrMnth", "Prop")
ggplot(df, aes(x = YrMnth, y = Prop, group = IntndPitFull)) + 
  geom_line(aes(color = IntndPitFull))
counts = crosstable.IntndPitFull.YrMnth$t
df = data.frame(IntndPitFull = counts)
names(df) = c("IntndPitFull", "YrMnth", "Count")
ggplot(df, aes(x = YrMnth, y = Count, group = IntndPitFull)) + 
  geom_line(aes(color = IntndPitFull), size = 1)

# By IDPoor status
freqs_2way_IntndPitFull_IDPoor = freqs.2way(data, "IntndPitFull", "IDPoor", 1)
freqs_2way_IntndPitFullDK_IDPoor = freqs.2way(data, "IntndPitFullDK", "IDPoor", 1)
freqs_2way_IntndPitFullEmpSlf_IDPoor = freqs.2way(data, "IntndPitFullEmpSlf", "IDPoor", 1)
freqs_2way_IntndPitFullPit_IDPoor = freqs.2way(data, "IntndPitFullPit", "IDPoor", 1)
freqs_2way_IntndPitFullOthr_IDPoor = freqs.2way(data, "IntndPitFullOthr", "IDPoor", 1)
freqs_2way_IntndPitFullPay_IDPoor = freqs.2way(data, "IntndPitFullPay", "IDPoor", 1)
freqs_2way_IntndPitFullStop_IDPoor = freqs.2way(data, "IntndPitFullStop", "IDPoor", 1)
crosstable_IntndPitFull_IDPoor = CrossTable(freqs_2way_IntndPitFull_IDPoor[[2]])
print(crosstable_IntndPitFull_IDPoor)
barplot(crosstable_IntndPitFull_IDPoor$prop.col, beside = T,
        col = c(1:6), family = "serif", axes = T,
        legend.text = c("Don't know", "Empty myself", "Install new pit",
                        "Other", "Pay someone", "Stop using latrine"), 
        args.legend = list(x = "topright", bty = "n"))
df = data.frame(IntndPitFull = crosstable_IntndPitFull_IDPoor$prop.col)
ggplot(df, aes(x = IntndPitFull.B, y = IntndPitFull.Freq, group = IntndPitFull.A)) + 
  geom_line(aes(color = IntndPitFull.A))

# By gender
freqs_2way_IntndPitFull_CGend = freqs.2way(data, "IntndPitFull", "CGend", 1)
freqs_2way_IntndPitFullDK_CGend = freqs.2way(data, "IntndPitFullDK", "CGend", 1)
freqs_2way_IntndPitFullEmpSlf_CGend = freqs.2way(data, "IntndPitFullEmpSlf", "CGend", 1)
freqs_2way_IntndPitFullPit_CGend = freqs.2way(data, "IntndPitFullPit", "CGend", 1)
freqs_2way_IntndPitFullOthr_CGend = freqs.2way(data, "IntndPitFullOthr", "CGend", 1)
freqs_2way_IntndPitFullPay_CGend = freqs.2way(data, "IntndPitFullPay", "CGend", 1)
freqs_2way_IntndPitFullStop_CGend = freqs.2way(data, "IntndPitFullStop", "CGend", 1)
crosstable_IntndPitFull_CGend = CrossTable(freqs_2way_IntndPitFull_CGend[[2]])
print(crosstable_IntndPitFull_CGend)
barplot(crosstable_IntndPitFull_CGend$prop.col, beside = T,
        col = c(1:6), family = "serif", axes = T,
        legend.text = c("Don't know", "Empty myself", "Install new pit",
                        "Other", "Pay someone", "Stop using latrine"), 
        args.legend = list(x = "topright", bty = "n"))
df = data.frame(IntndPitFull = crosstable_IntndPitFull_CGend$prop.col)
ggplot(df, aes(x = IntndPitFull.B, y = IntndPitFull.Freq, group = IntndPitFull.A)) + 
  geom_line(aes(color = IntndPitFull.A))

# By province
data.sub = subset(data, select = c(Prov, IntndPitFull))
data.sub = subset(data.sub, Prov != "Kampong Cham" &
                    Prov != "Kampong Speu" &
                    Prov != "Takeo")
data.sub = droplevels(data.sub)
summary(data.sub, maxsum = 100)
length(data.sub$IntndPitFull)
freqs.2way.IntndPitFull.Prov = freqs.2way(data.sub, "IntndPitFull", "Prov", 1)
crosstable.IntndPitFull.Prov = CrossTable(freqs.2way.IntndPitFull.Prov[[2]])
print(crosstable.IntndPitFull.Prov)
prop.col = crosstable.IntndPitFull.Prov$prop.col
typeof(prop.col)
order = c("Pay", "Pit", "EmpSlf", "DK", "Stop", "Othr")
prop.col = prop.col[match(order, row.names(prop.col)),,drop = FALSE]
order2 = c("Oddar Meanchey", "Banteay Meanchey", "Siem Reap", "Kampong Thom", "Kandal", "Prey Veng", 
           "Svay Rieng")
prop.col = prop.col[,match(order2, colnames(prop.col)),drop = FALSE]
df = data.frame(IntndPitFull = prop.col)
names(df) = c("IntndPitFull", "Prov", "Prop")
df$Desir = rep(x = c("Des", "Des", "Undes", "Undes", "Undes", "Undes"), times = 7)
ggplot(df, aes(x = Prov, y = Prop, fill = IntndPitFull)) +
  geom_col() + 
  coord_flip()
title = "                     Desirable                                    Undesirable"
data.sub.des = subset(df, Desir == "Des")
data.sub.undes = subset(df, Desir == "Undes")
perc_des = round(data.sub.des$Prop[data.sub.des$IntndPitFull == "Pay"] +
                   data.sub.des$Prop[data.sub.des$IntndPitFull == "Pit"], 2)*100
perc_undes = 100 - perc_des
ggplot(df, aes(x = Prov)) + 
  geom_col(data = data.sub.des, aes(y = -Prop, fill = IntndPitFull)) + 
  geom_col(data = data.sub.undes, aes(y = Prop, fill = IntndPitFull)) + 
  coord_flip() + 
  geom_hline(yintercept = 0, colour = "white", lwd = 1) + 
  scale_x_discrete(name = "Province", 
                   breaks = unique(df$Prov),
                   limits = rev(unique(df$Prov))) +
  scale_y_continuous(name = "Percentage of rural latrine owners", 
                     breaks = round(seq(-0.8, 0.8, 0.1), 1), 
                     limits = c(-0.8, 0.8),
                     labels = paste(as.character(abs(seq(-80, 80, 10))), "%", sep = "")) +
  scale_fill_discrete(name = "Intention when Pit Fills: ",
                      breaks = order,
                      labels = c("Pay professional",
                                 "Install a new pit",
                                 "Self-empty",
                                 "Don't know",
                                 "Stop using latrine",
                                 "Other")) +
  ggtitle(title) + 
  theme_bw() +
  theme(text = element_text(family = "serif", size = 14), 
        legend.position = "bottom",
        legend.direction = "horizontal",
        panel.background = element_blank()) +
  annotate(geom = "text", size = 4, family = "serif",
           x = unique(df$Prov), y = -perc_des/100 - 0.04, 
           label = paste(as.character(perc_des), "%", sep = "")) + 
  annotate(geom = "text", size = 4, family = "serif",
           x = unique(df$Prov), y = perc_undes/100 + 0.05, 
           label = paste(as.character(perc_undes), "%", sep = ""))
ggplot(df, aes(x = Prov, y = Prop, group = IntndPitFull)) + 
  geom_line(aes(color = IntndPitFull), size = 2) +
  guides(linetype = guide_legend(override.aes = list(alpha = 1)))
mean(perc_des)
sd(perc_des)
windowsFonts()
correspondence(data = data.sub, metric1 = "IntndPitFull", metric2 = "Prov")

# By rainfall
summary(as.factor(data$Rain.mm))
length(data)
# missmap(data[45], main = "Missing values vs observed", legend = F)
biserial.cor(data$Rain.mm, data$IntndPitFullDes, use = "complete.obs")
ggplot(data = data, aes(x = as.numeric(Mnth), y = Rain.mm, colour = Prov)) +
  geom_line(aes(colour = Prov), size = 1.5) +
  theme_minimal() +
  labs(x = "Month", y = "Rainfall (mm)", colour = "Provinces") +
  scale_x_continuous(breaks = c(1:12)) +
  coord_cartesian(xlim = c(1, 12), ylim = c(0, max(data$Rain.mm) + 30))
data.sub = subset(data, select = c(Rain.mm, IntndPitFullDes))
model = glm(formula = IntndPitFullDes ~ Rain.mm,
            data = data.sub,
            family = binomial(link = "logit"),
            na.action = na.omit)
plot(data.sub$IntndPitFullDes, data.sub$Rain.mm)
biserial.cor(data.sub$Rain.mm, data.sub$IntndPitFullDes, use = "complete.obs")
anova(model, test = "Chisq")
summary(data.sub)
plot(data$IntndPitFullDes, data$Rain.mm)
df = data.frame(Prov = factor(), PtBisCor = double(), P = double())
levels(df$Prov) = unique(data$Prov)
# for (i in 1:1) {
for (i in 1:length(unique(data$Prov))) {
  data.sub = subset(data, Prov == unique(data$Prov)[i],
                    select = c(Rain.mm, IntndPitFullDes))
  model = glm(formula = IntndPitFullDes ~ Rain.mm,
              data = data.sub,
              family = binomial(link = "logit"),
              na.action = na.omit)
  plot(data.sub$IntndPitFullDes, data.sub$Rain.mm, main = unique(data$Prov)[i])
  prov = unique(data$Prov)[i]
  ptbiscor = biserial.cor(data.sub$Rain.mm, data.sub$IntndPitFullDes, 
                          use = "complete.obs")
  p = anova(model, test = "Chisq")$`Pr(>Chi)`[2]
  df = rbind(df, data.frame(Prov = prov, PtBisCor = ptbiscor, P = p))
}
df
summary(data$Prov)
mean(df$P[!is.na(df$P)])

###############################################################################
# ANALYZE DATA AGGREGATED BY PROVINCE
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
