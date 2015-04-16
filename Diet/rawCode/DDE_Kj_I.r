####################################################################################################
#                   DIET KJ ANALYSIS - COMMON DOLPHIN DIET (HAKE)
#              
#                    created: (camilo.saavedra@vi.ieo.es) 16/12/2013
####################################################################################################


# Set working directory (to source file location)

# Charge libraries
library(boot)
library(reshape)
library(ggplot2)

# Reading data
ddeSp <- read.csv("../../RData/dietCD.csv") # Galician diet
dietCDPt <- read.csv("../../RData/dietCDPt.csv") # Potugal diet

# Total energy content in Galicia
sum <- colSums(ddeSp[,-1])
tot <- sum(sum[-length(sum)])
prop <- (sum/tot) * 100
gr <- prop[-length(sum)] * 10
energCont <- c(0, 8.5, 8.7, 6.1, 6.6, 4.5, 6.2, 3.7, 0, 6.0, 8.1, 6.5, 5.4, 0, 5.3, 0, 
               5.6, 7.9, 5.2, 7.3, 0, 5.4, 5, 5.7, 3.8, 4.8, 4.8, 4.4, 4.7, 4.4)
TotalfoodGal <- sum(gr * energCont)

# Oherfood energy content in Galicia
sum <- colSums(ddeSp[,-1])
totNOhkesar <- sum(sum[-c(3,8,length(sum))])
propNOhkesar <- (sum[-c(3,8,length(sum))]/totNOhkesar[-c(3,8,length(sum))]) * 100
grNOhkesar <- propNOhkesar * 10
energCont <- c(0, 8.5, 6.1, 6.6, 4.5, 6.2, 0, 6.0, 8.1, 6.5, 5.4, 0, 5.3, 0, 
  5.6, 7.9, 5.2, 7.3, 0, 5.4, 5, 5.7, 3.8, 4.8, 4.8, 4.4, 4.7, 4.4)
OtherfoodGal <- sum(grNOhkesar * energCont)


# Total energy content in Galicia
sum <- colSums(dietCDPt[,-1])
tot <- sum(sum[-length(sum)])
prop <- (sum/tot) * 100
gr <- prop[-length(sum)] * 10
energCont <- c(0, 8.5, 8.7, 6.1, 6.6, 4.5, 6.2, 3.7, 0, 6.0, 8.1, 6.5, 5.4, 0, 5.3, 0, 
               5.6, 7.9, 5.2, 7.3, 0, 5.4, 5, 5.7, 3.8, 4.8, 4.8, 4.4, 4.7, 4.4)
TotalfoodPt <- sum(gr * energCont)

# Oherfood energy content in Portugal
sum <- colSums(dietCDPt[,-1])
totNOhkesar <- sum(sum[-c(3,8,length(sum))])
propNOhkesar <- (sum[-c(3,8,length(sum))]/totNOhkesar[-c(3,8,length(sum))]) * 100
grNOhkesar <- propNOhkesar * 10
energCont <- c(0, 8.5, 6.1, 6.6, 4.5, 6.2, 0, 6.0, 8.1, 6.5, 5.4, 0, 5.3, 0, 
               5.6, 7.9, 5.2, 7.3, 0, 5.4, 5, 5.7, 3.8, 4.8, 4.8, 4.4, 4.7, 4.4)
OtherfoodPt <- sum(grNOhkesar * energCont)


OtherFood <- (OtherfoodGal + OtherfoodPt)/2

# Daily food intake estimation
KjGal <- read.csv("../../RData/KjCDGal.csv")
KjPt <- read.csv("../../RData/KjCDPt.csv")
# Kj without hake and sardine
NOhkesarGal <- sum(KjGal[-c(1,6),4] * KjGal[-c(1,6),5] * 10)
NOhkesarPt <- sum(KjPt[-c(1,6),4] * KjPt[-c(1,6),5] * 10)
OtherfoodMean <- (NOhkesarGal + NOhkesarPt)/2
OtherfoodMean/5.7

KjGal[-c(1,6),5]*1000



