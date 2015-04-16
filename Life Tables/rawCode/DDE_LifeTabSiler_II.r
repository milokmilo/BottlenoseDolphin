####################################################################################################
#                                 SILER LIFE TABLES 
#
#                   created:  (camilo.saavedra@vi.ieo.es) 22/10/2013
####################################################################################################

# IMPORTANT: Set working directory (to source file location)

# Call libraries 
library(ggplot2)

# Read data 
#CEMMA <- read.csv("../../data/RData/CEMMA.csv")
#ddePop <- CEMMA[CEMMA$sp == "DDE",c("age")]
#ddePop <- ddePop[complete.cases(ddePop)]
# Changing age values (0.5 and 1.5 for 0 and 1)
#ddePop[ddePop == 0.5] <- 0
#ddePop[ddePop == 1.5] <- 1
# Calculating stranding frequencies by age 
#Str <- table(ddePop)
#Str <-data.frame(age = as.numeric(rownames(Str)), n = Str[])
#ages <- data.frame(age=c(0,seq(1:29))) # Creates a dataframe with all the ages
#Str <- merge(ages, Str, by="age", all.x=TRUE) # Merges dataframes
#Str[is.na(Str)] <- 0 # Replaces to zeros
#Str

## LIFE TABLE (original strandisngs data) ##

#lifeTab <- data.frame(ageCl=Str$age, M=Str$n) # mortality table
#n <- 1000
# S - Creating survivorship column
#d <- sum(lifeTab$M) # sum of deaths
#for (j in 1:nrow(lifeTab)){
#  lifeTab$S[[j]] <- d-sum(lifeTab$M[1:j]) } 
# N - number
#lifeTab$N <- lifeTab$M + lifeTab$S
# nx - Standardizing survivors [(N/∑M) * 1000]
#lifeTab$nx <- round((lifeTab$N/d)*n,1) 
# dx - Dolphins death-at-age [nx - n(x+1)]
#for (j in 1:nrow(lifeTab)) {
#  if (j == 1) {dx <- vector("numeric")}
#  d <- round(lifeTab$nx[j]-lifeTab$nx[j+1],3)
#  if (j == nrow(lifeTab)) {d <- round(lifeTab$nx[j]-0,3)} 
#  dx <- c(dx,d)}
#lifeTab$dx <- dx
# qx - Death-at-age probability [dx / nx]
#for (j in 1:nrow(lifeTab)) {
#  if (j == 1) {qx <- vector("numeric")}
#  q <- round(lifeTab$dx[j]/lifeTab$nx[j],3)
#  qx <- c(qx,q) }
#lifeTab$qx <- qx 

## Readind life table data (original strandings data) ##
load("../../data/RObjects/lifeTables/LifeTabn.RData")


## FITING SILER MODEL ##

# Selecting ages 2,3 (rows 3,4)
for (i in c(3,4)){
  young <- lifeTab[i:6, c("ageCl", "qx")]
  adult <- lifeTab[-c(1:2), c("ageCl", "qx")]
  adult <- adult[adult$qx!=0,]
  old <- lifeTab[5:30, c("ageCl", "qx")]
  old <- old[old$qx!=0,]
  ag <- c(0,seq(1:29))
  # Adult mortality
  a2 <- min(adult$qx)
  # Younger mortality
  young$qx <- (young$qx) - a2
  exp <- nls(qx ~ a*exp(b*ageCl), data = young, start = list(a = 10, b = 1))
  a1 <- coef(exp)[[1]]
  b1 <- coef(exp)[[2]]
  fit <- a1 * exp(b1 * ag)
  # Senescence mortality
  old$qx <- (old$qx) - a2
  expS <- nls(qx ~ a*exp(b*ageCl), data = old, start = list(a = 1, b = 0.5))
  a3 <- coef(expS)[[1]]
  b3 <- coef(expS)[[2]]
  fitS <- a3 * exp(b3 * ag)
  # Model
  Siler <- a1*exp(b1*ag)+a2+a3*exp(b3*ag)
  # plot
  t <- lifeTab[,c("ageCl", "qx")]
  assign(paste("Siler0", i-2, sep=""), cbind(t,fit, fitS, Siler))
}

## Plotting the two models ##
plot(lifeTab[,c("ageCl", "qx")], xlim=c(0,30), ylim=c(0,1))
lines(Siler01$ageCl, Siler01$fit+a2, col=2, lty=1)
lines(Siler02$ageCl, Siler02$fit+a2, col=2, lty=2)
abline(h=a2, col=4)
lines(Siler01$ageCl, Siler01$fitS+a2, col=3)
lines (Siler01$ageCl, Siler01$Siler, col=1, lty=1)
lines (Siler02$ageCl, Siler02$Siler, col=1, lty=2)


## PREDICTED SILER LIFE TABLE (Predicted strandings amounts from Siler mortality) ##

for (i in c(3,4)){
  lifeSiler <- data.frame(ageCl=c(0,seq(1:29)), qxS=round(get(paste("Siler0",i-2,sep=""))$Siler,3))
  n <- 1000
  # nx and dx - survivors and deaths at age
  for (j in 1:nrow(lifeSiler)){
    if (j == 1) {dxS <- lifeSiler$qxS[1]*n; nxS <- n
    } else {
    nxS <- c(nxS, nxS[j-1]- dxS[j-1])
    dxS <- c(dxS, lifeSiler$qxS[j]*nxS[j])
    }
  }
  lifeSiler <- data.frame(lifeSiler, nxS=round(nxS,5), dxS=round(dxS,5))
  # lx - Survivorship-at-age percent 
  lifeSiler$lxS <- lifeSiler$nxS/n 
  # ex - Life expentancy at age ex = ∑lx/lx
  exS <- vector("numeric")
  for (j in 1:nrow(lifeSiler)) {
    eS <- round(sum(lifeSiler$lxS[j:nrow(lifeSiler)])/lifeSiler$lxS[j],3)
    exS <- c(exS,eS) }
  lifeSiler$exS <- exS
  # Z - Total mortality-at-age -L(nt/no)/t
  ZS <- c(NA)
  for (j in 1:nrow(lifeSiler)){
    if (j == 1) {ZS <- vector("numeric")}
    zS <- round(-log(lifeSiler$nxS[j+1]/lifeSiler$nxS[j])/1,2)
    # Correction for the last mortality
    if (j == nrow(lifeSiler)) {zS <- NA}
    ZS <- c(ZS,zS)
  }
  lifeSiler$ZS <- ZS
  assign(paste("lifeSiler0",i-2,sep=""), lifeSiler)
  rm(lifeSiler)
}

lifeSilers <- ls(pattern="^lifeSiler")
#for (i in 1:length(lifeTabs)){
#  print(get(lifeTabs[i]))
#}

# save life tables
for (i in 1:length(lifeSilers)){
  assign("lifeSiler", get(lifeSilers[i]))
  save(file=paste("../../data/RObjects/lifeTables/", lifeSilers[i], ".RData", sep=""), lifeSiler)
}


## Plotting Z total mortality ##
plot(lifeSiler01$age, lifeSiler01$Z, type= "l", col=2)
lines(lifeSiler02$Z, lty=2, col=2)

