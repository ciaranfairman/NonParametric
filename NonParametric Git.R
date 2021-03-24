library(gtsummary)
library("pwr")
library(dplyr)
library("clinfun")
library("pgirmess")
library(gridExtra)
library("broom")
library(scales)
library(reshape2)
library("psych")
library("ggplot2")
library("pastecs")
library("WRS2")
library(car)
library(tidyverse)

getwd()
setwd("~/Dropbox/DataSet/Section2")

Drug <- read.delim("Drug.dat", header = TRUE)

#Check Normality

#histogram using ggplot with normal curve (stat_function)
#Pre

HIST_Sun <- ggplot(Drug, aes(x = sundayBDI)) +
  geom_histogram(colour = "light blue", fill = "grey", bins = 30) +
  labs(x = "group", y = "count",
       title = "Drug Count",
       caption = "Figure 1. Histogram for drugs") +
  stat_function(fun = dnorm, args = list(mean = mean(Drug$sundayBDI, na.rm = TRUE),
                                         sd = sd(Drug$sundayBDI)), colour = "pink", size = 1) +
  theme_classic()
HIST_Sun



HIST_Wed <- ggplot(Drug, aes(x = wedsBDI)) +
  geom_histogram(colour = "light blue", fill = "grey", bins = 30) +
  labs(x = "group", y = "count",
       title = "Drug Count",
       caption = "Figure 1. Histogram for drugs") +
  stat_function(fun = dnorm, args = list(mean = mean(Drug$wedsBDI, na.rm = TRUE),
                                         sd = sd(Drug$wedsBDI)), colour = "pink", size = 1) +
  theme_classic()
HIST_Wed

#Checking normality with numbers

by(cbind(data=Drug$sundayBDI, data=Drug$wedsBDI), Drug$drug, stat.desc, basic = FALSE, norm = TRUE)


#Check Homogeneity of Variance
leveneTest(Drug$sundayBDI, Drug$drug, center = mean)
leveneTest(Drug$wedsBDI, Drug$drug, center = mean)


#wilcoxon
SunWilcox <- wilcox.test(sundayBDI ~ drug, data = Drug)
SunWilcox

WedWilcox <- wilcox.test(wedsBDI ~ drug, data = Drug)
WedWilcox

#NEED TO DOUBLE CHECK COHEN'S D
effsize::cohen.d()


#Boxplot
library(scales)
ggplot(Drug, aes(x= factor(drug,
                           labels = c("Ecstacy",
                                      "Alcohol")),
                 y = sundayBDI,
                 color = drug)) +
  geom_boxplot(size = 1,
               outlier.shape = 1,
               outlier.color = "black",
               outlier.size = 3) +
  geom_jitter(alpha = 0.7,
              width = .15) +
  labs(title = "Sunday Drug use",
       x = "Drug",
       y = "Use") +
  theme_minimal() +
  theme(legend.position = "none") 


#Effects Size
rFromWilcox <- function(wilcoxModel, N){
  z <- qnorm(wilcoxModel$p.value/2)
  r<- z/sqrt(N)
  cat(wilcoxModel$data.name, "Effect Size, r = ", r)
}

rFromWilcox(SunWilcox, 20)
rFromWilcox(WedWilcox, 20)











##Compute Variable##
BDIchange <- Drug$sundayBDI-Drug$wedsBDI
Drug$BDIChange <- BDIchange

mutate(Drug, ChangeBDI = Drug$sundayBDI-Drug$wedsBDI)

by(cbind(data=Drug$BDIChange), Drug$drug, stat.desc, basic = FALSE, norm = TRUE)
?by



#Subsets for Wilcoxon Signed Rank

EOnlyu <-(subset(Drug, Drug == "Ecstasy"))
AlcoholOnly <-(subset(Drug, Drug == "Alcohol"))

AlcoholWilcox <-wilcox.test(AlcoholOnly$wedsBDI, AlcoholOnly$sundayBDI, paired = TRUE, correct = FALSE)
AlcoholWilcox

EcstacyWilcox <-wilcox.test(EOnlyu$wedsBDI, EOnlyu$sundayBDI, paired = TRUE, correct = FALSE)
EcstacyWilcox

#convert wide to long
EVisual <- EOnlyu[, c(1:3)]

Elong <- melt(EVisual, id.vars = "drug")

ggplot(Elong, aes(x= factor(variable,
                            labels = c("SundayBDI",
                                       "WedBDI")),
                  y = value,
                  color = variable)) +
  geom_boxplot(size = 1,
               outlier.shape = 1,
               outlier.color = "black",
               outlier.size = 3) +
  geom_jitter(alpha = 0.7,
              width = .15) +
  labs(title = "EcstacyBDI",
       x = "Day",
       y = "Depression") +
  theme_minimal() +
  theme(legend.position = "none") 

#AlcoholOnly

AlcholBDI <- AlcoholOnly[, c(1:3)]

Alclong <- melt(AlcholBDI, id.vars = "drug")

ggplot(Alclong, aes(x= factor(variable,
                              labels = c("SundayBDI",
                                         "WedBDI")),
                    y = value,
                    color = variable)) +
  geom_boxplot(size = 1,
               outlier.shape = 1,
               outlier.color = "black",
               outlier.size = 3) +
  geom_jitter(alpha = 0.7,
              width = .15) +
  labs(title = "AlchoholBDI",
       x = "Day",
       y = "Depression") +
  theme_minimal() +
  theme(legend.position = "none")           

rFromWilcox(AlcoholWilcox, 20)

rFromWilcox(EcstacyWilcox, 20)


#Create table (GT Summary)

Summary <- Drug %>% select(drug, sundayBDI, wedsBDI)
Summary %>% tbl_summary()

#split summary table by drug
Summary %>% tbl_summary(by = drug,
                        statistic = list(all_continuous() ~ "{mean} ({sd})",
                                         all_categorical() ~ "{n} / {N} ({p}%)"),
                        label = sundayBDI ~ "Depression on Sunday") 
