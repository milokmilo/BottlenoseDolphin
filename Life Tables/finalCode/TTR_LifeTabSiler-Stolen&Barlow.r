####################################################################################################
#                   SILER LIFE TABLES FOR BOTTLENOSE DOLPHIN
#          Siler life table from the predicted proportion of strandings
#                     using data from Stolen and Barlow, 2003
#                created: (camilo.saavedra@vi.ieo.es) 04/04/2015
####################################################################################################

# IMPORTANT: Set working directory (to source file location)


# inputs
# "../../RObjects/TTR_SilerA-S&B.RData"
# "../../RObjects/TTR_SilerM-S&B.RData"
# "../../RObjects/TTR_SilerF-S&B.RData"

# outputs
# ../../RObjects/lifeSilerA-S&B.RData
# ../../RObjects/lifeSilerM-S&B.RData
# ../../RObjects/lifeSilerF-S&B.RData


# Charge libraries
library(ggplot2)
library(reshape)

## Reading life tables data (following Stolen and Berlow, 1993) ##
load("../../RObjects/TTR_SilerA-S&B.RData")
load("../../RObjects/TTR_SilerM-S&B.RData")
load("../../RObjects/TTR_SilerF-S&B.RData")


## PREDICTING SILER LIFE TABLE (from Siler models fitted to Stolen and Barlow, 2003) ##

## All

lifeSiler <- data.frame(age=c(0,seq(1:36)), qx=round(get(paste("Siler","A",sep=""))$BotSiler,3))
n <- 1000
# nx and dx - survivors and deaths at age
for (j in 1:nrow(lifeSiler)){
  if (j == 1) {dx <- lifeSiler$qx[1]*n; nx <- n
  } else {
    nx <- c(nx, nx[j-1]- dx[j-1])
    dx <- c(dx, lifeSiler$qx[j]*nx[j])
  }
}
lifeSiler <- data.frame(lifeSiler, nx=round(nx,5), dx=round(dx,5))
# lx - Survivorship-at-age percent 
lifeSiler$lx <- lifeSiler$nx/n 
# ex - Life expentancy at age ex = ∑lx/lx
ex <- vector("numeric")
for (j in 1:nrow(lifeSiler)) {
  e <- round(sum(lifeSiler$lx[j:nrow(lifeSiler)])/lifeSiler$lx[j],3)
  ex <- c(ex,e) }
lifeSiler$ex <- ex
# Z - Total mortality-at-age -L(nt/no)/t
Z <- c(NA)
for (j in 1:nrow(lifeSiler)){
  if (j == 1) {Z <- vector("numeric")}
  z <- round(-log(lifeSiler$nx[j+1]/lifeSiler$nx[j])/1,2)
  # Correction for the last mortality
  if (j == nrow(lifeSiler)) {z <- NA}
  Z <- c(Z,z)
}
lifeSiler$Z <- Z
lifeSiler$dxProp <- lifeSiler$dx/n
lifeSiler <- lifeSiler[-nrow(lifeSiler),]
assign(paste("lifeSiler","A",sep=""), lifeSiler)
rm(lifeSiler)


## Males

lifeSiler <- data.frame(age=c(0,seq(1:40)), qx=round(get(paste("Siler","M",sep=""))$BotSiler,3))
n <- 1000
# nx and dx - survivors and deaths at age
for (j in 1:nrow(lifeSiler)){
  if (j == 1) {dx <- lifeSiler$qx[1]*n; nx <- n
  } else {
    nx <- c(nx, nx[j-1]- dx[j-1])
    dx <- c(dx, lifeSiler$qx[j]*nx[j])
  }
}
lifeSiler <- data.frame(lifeSiler, nx=round(nx,5), dx=round(dx,5))
# lx - Survivorship-at-age percent 
lifeSiler$lx <- lifeSiler$nx/n 
# ex - Life expentancy at age ex = ∑lx/lx
ex <- vector("numeric")
for (j in 1:nrow(lifeSiler)) {
  e <- round(sum(lifeSiler$lx[j:nrow(lifeSiler)])/lifeSiler$lx[j],3)
  ex <- c(ex,e) }
lifeSiler$ex <- ex
# Z - Total mortality-at-age -L(nt/no)/t
Z <- c(NA)
for (j in 1:nrow(lifeSiler)){
  if (j == 1) {Z <- vector("numeric")}
  z <- round(-log(lifeSiler$nx[j+1]/lifeSiler$nx[j])/1,2)
  # Correction for the last mortality
  if (j == nrow(lifeSiler)) {z <- NA}
  Z <- c(Z,z)
}
lifeSiler$Z <- Z
lifeSiler$dxProp <- lifeSiler$dx/n
lifeSiler <- lifeSiler[-nrow(lifeSiler),]
assign(paste("lifeSiler","M",sep=""), lifeSiler)
rm(lifeSiler)


## Females


lifeSiler <- data.frame(age=c(0,seq(1:36)), qx=round(get(paste("Siler","F",sep=""))$BotSiler,3))
n <- 1000
# nx and dx - survivors and deaths at age
for (j in 1:nrow(lifeSiler)){
  if (j == 1) {dx <- lifeSiler$qx[1]*n; nx <- n
  } else {
    nx <- c(nx, nx[j-1]- dx[j-1])
    dx <- c(dx, lifeSiler$qx[j]*nx[j])
  }
}
lifeSiler <- data.frame(lifeSiler, nx=round(nx,5), dx=round(dx,5))
# lx - Survivorship-at-age percent 
lifeSiler$lx <- lifeSiler$nx/n 
# ex - Life expentancy at age ex = ∑lx/lx
ex <- vector("numeric")
for (j in 1:nrow(lifeSiler)) {
  e <- round(sum(lifeSiler$lx[j:nrow(lifeSiler)])/lifeSiler$lx[j],3)
  ex <- c(ex,e) }
lifeSiler$ex <- ex
# Z - Total mortality-at-age -L(nt/no)/t
Z <- c(NA)
for (j in 1:nrow(lifeSiler)){
  if (j == 1) {Z <- vector("numeric")}
  z <- round(-log(lifeSiler$nx[j+1]/lifeSiler$nx[j])/1,2)
  # Correction for the last mortality
  if (j == nrow(lifeSiler)) {z <- NA}
  Z <- c(Z,z)
}
lifeSiler$Z <- Z
lifeSiler$dxProp <- lifeSiler$dx/n
lifeSiler <- lifeSiler[-nrow(lifeSiler),]
assign(paste("lifeSiler","F",sep=""), lifeSiler)
rm(lifeSiler)



# save life tables
lifeSilers <- ls(pattern="^lifeSiler[A|M|F]")
for (i in 1:length(lifeSilers)){
  save(list=lifeSilers[i],
    file=paste("../../RObjects/", lifeSilers[i], "-S&B", ".RData", sep=""))
}



