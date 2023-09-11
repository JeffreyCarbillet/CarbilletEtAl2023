###############################################################
# Saliva and plasma corticosterone in Common gulls            #
# April 2023                                                  #
# J. Carbillet                                                #
#                                                             #
###############################################################

#############
# Workspace #
#############

setwd("C:/Users/jeffreym/Documents/Donnees chevreuils")

###########
# library #
###########

library(BayesFactor)
library(bayestestR)
library(MuMIn)

# Load dataset
rm(list = ls())
DataManip=read.csv("testcort.csv",header=T, dec=".")
summary(DataManip)
dim(DataManip) # 24 observations of 5 variables
attach(DataManip)

# Pearson correlation test
plot(DataManip$mean.saliva,DataManip$mean.plasma)
cor.test(DataManip$mean.saliva,DataManip$mean.plasma)

# Pearson correlation test without the 4 highest values
plot(Datatest$mean.saliva,Datatest$mean.plasma)
cor.test(Datatest$mean.saliva,Datatest$mean.plasma)

# Bayesian correlation test
result <- correlationBF(DataManip$mean.saliva,DataManip$mean.plasma,iterations = 10000)
describe_posterior(result)

# Plot
plot(DataManip$mean.saliva, DataManip$mean.plasma, pch = 16,
     ylab="Plasma corticosterone (ng/mL)", 
     xlab="Salivary corticosterone (ng/mL)")
abline(lm(DataManip$mean.plasma ~ DataManip$mean.saliva), col = 4, lwd = 3)

# Text
coef <- round(coef(lm(DataManip$mean.plasma ~ DataManip$mean.saliva)), 2)
text(1.5, 12.5,  paste("Y = ", coef[2], "x", "+", coef[1]))
text(4.5, 3.5, paste("R = 0.58",";", "P < 0.003"))
text(4.5, 2.5, paste("n = 24"))

## Paired T-test to compare salivary and plasmatic corticosterone levels
t.test(DataManip$mean.saliva, DataManip$mean.plasma, 
       paired = TRUE, alternative = "two.sided")

# Regression model - 20 observations (4 individual without known age)
datacsna=na.omit(DataManip)
lm1=lm(log(mean.plasma)~mean.saliva+age+sex+head.size+gland.size+trapping.time,
       data=datacsna)
anova(lm1)
summary(lm1)

# Model selection
options(na.action = na.fail)
dd <- dredge(lm1)
d=subset(dd, delta < 2)
d

# Final model
lm2=lm(log(mean.plasma)~mean.saliva,data=DataManip)
anova(lm2)
summary(lm2) # R=0.22
confint(lm2)

# Data on the percentage difference between plasma and saliva
summary(DataManip$salivary.percentage.of.plasma.cort)
sd(DataManip$salivary.percentage.of.plasma.cort)