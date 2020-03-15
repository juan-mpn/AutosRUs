# Challenge 15 AutosRU's Statistics
# Juan M. Pacheco
# 3/14/2020
# Linear Regresion
# R Script
library(jsonlite)
library(tidyverse)

# Created a copy of the MechaCar_mpg and renamed the columns with shorter name
mpgm <- read.csv(file='MechaCar_mpg2.csv',check.names=F,stringsAsFactors = F)

mpg_filt <- mpgm[,c("mpg","length")] #filter columns from mtcars dataset
mpg_filt$length <- factor(mpg_filt$length) #convert numeric column to factor 

aov(mpg ~ length,data=mpg_filt) # compare means across multiple levels
summary(aov(mpg ~ length,data=mpg_filt))

used_matrix <- as.matrix(mpgm[,c("weight","mpg","clearance","length","angle","AWD")]) #convert data frame into numeric matrix
cor(used_matrix)

plt <- ggplot(mpgm,aes(x=mpg,y=length,color=mpg)) #import dataset into ggplot2
plt + geom_point() + labs(x="mpg", y="length", color="mpg") #add scatter plot with labels

lm(mpg ~ length + AWD + clearance + weight + angle,data=mpgm) #generate multiple linear regression model

summary(lm(mpg ~ length + AWD + clearance + weight + angle,data=mpgm)) #generate summary statistics

model <- lm(mpg ~ length,mpgm) #create linear model
yvals <- model$coefficients['length']*mpgm$length + model$coefficients['(Intercept)'] #determine y-axis values from linear model

plt <- ggplot(mpgm,aes(x=length,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model

# ------------------ Suspension_Coil.csv ---------------------------------------
mcoil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

plt <- ggplot(mcoil,aes(x=PSI)) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot


summarize_demo <- mcoil %>% group_by(Manufacturing_Lot) %>% summarize(Min_PSI=min(PSI),Mean_PSI=mean(PSI),Maximum_PSI=max(PSI),
                                                                      Med_PSI=median(PSI),StandDev_PSI=sd(PSI),
                                                                      Variance=var(PSI),
                                                                      Num_Vehicles=n(), ) #create summary table with multiple columns

mcoilL1 <- mcoil %>% filter(Manufacturing_Lot=="Lot1") # 
mcoilL2 <- mcoil %>% filter(Manufacturing_Lot=="Lot2")
mcoilL3 <- mcoil %>% filter(Manufacturing_Lot=="Lot3") # 
t.test(mcoilL1$PSI,mcoilL2$PSI,paired = T) 
t.test(mcoilL1$PSI,mcoilL3$PSI,paired = T) 

