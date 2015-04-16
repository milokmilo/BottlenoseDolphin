####################################################################################################
#                               BY CATCH SILER LIFE TABLES 
#             Observed life tables with both total, bycatch and no bycatch data.
#         Siler mortality model and Siler life tables with both types of strandings
#                   created: (camilo.saavedra@vi.ieo.es) 11/11/2013
####################################################################################################

# inputs
# "../../RObjects/byCatch.RData"

# outputs
# "../../RObjects/lifeTabNByC.RData"
# "../../RObjects/NByCSiler00.RData"
# "../../RObjects/lifeSilerNByC.RData"
# "../plots/DDE_SilerNByC.png"


# IMPORTANT: Set working directory (to source file location)

## Loading data (original strandings data) ##
load(file="../../RObjects/byCatch.RData")

######################################## LIFE TABLE ################################################

# age: real age of the dolphin
# M: Stranded at age x
# S: Survivors at the start of the age x
# nx: Standardized survivors (by n=1000) [(S/∑M) * 1000]
# dx: Death-at-age (standarized stranded dolphins) [nx - n(x+1)]
# qx: Death-at-age probability (probability of death by age) [dx / nx]
# lx: Survivors-at-age percent (how many survivors remain in each age) [nx / n]
# ex: Life expentancy at age (average years of life in each age) [∑ly/lx]
# Z: Total mortality (Natural mortality-M + Fishing mortality-F) [(-log(S)/(S-1))/t] 
#--------------------------------------------------------------------------------------------------#

### ACTUAL STRANDINGS (TOTAL, BYCATCH AND NO BYCATCH) ###

for (i in 3){ # 2:length(byCatch)
  lifeTab <- data.frame(ageCl=byCatch[,1], M=byCatch[,i]) # mortality table
  #lifeTab <- rbind(c(NA, 0, 0), lifeTab) 
  n <- 1000
  # S - Creating survivorship column
  d <- sum(lifeTab$M) # sum of deaths
  for (j in 1:nrow(lifeTab)){
    lifeTab$S[[j]] <- d-sum(lifeTab$M[1:j-1])} 
  # nx - Standardizing survivors [(N/∑M) * 1000]
  lifeTab$nx <- round((lifeTab$S/d)*n,1) 
  # dx - Dolphins death-at-age [nx - n(x+1)]
  for (j in 1:nrow(lifeTab)) {
    if (j == 1) {dx <- vector("numeric")}
    d <- round(lifeTab$nx[j]-lifeTab$nx[j+1],3)
    if (j == nrow(lifeTab)) {d <- round(lifeTab$nx[j]-0,3)} 
    dx <- c(dx,d)}
  lifeTab$dx <- dx
  # qx - Death-at-age probability [dx / nx]
  for (j in 1:nrow(lifeTab)) {
    if (j == 1) {qx <- vector("numeric")}
    q <- round(lifeTab$dx[j]/lifeTab$nx[j],3)
    qx <- c(qx,q) }
  lifeTab$qx <- qx 
  # lx - Survivorship-at-age percent [nx / n]
  lifeTab$lx <- lifeTab$nx/n 
  # ex - Life expentancy at age ex = ∑ly/lx
  ex <- vector("numeric")
  for (j in 1:nrow(lifeTab)) {
    e <- round(sum(lifeTab$lx[j:nrow(lifeTab)])/lifeTab$lx[j],3)
    ex <- c(ex,e) }
  lifeTab$ex <- ex
  # Z - Total mortality-at-age -L(nt/no)/t
  Z <- c(NA)
  for (j in 1:nrow(lifeTab)){
    if (j == 1) {Z <- vector("numeric")}
    z <- round(-log(lifeTab$nx[j+1]/lifeTab$nx[j])/1,2)
    if (j == nrow(lifeTab)) {z <- 1.00}
    Z <- c(Z,z)
  }
  lifeTab$Z <- Z
  assign(paste("lifeTab", names(byCatch)[i], sep=""),lifeTab)
  rm("lifeTab")
}

# Printing life tables 
lifeTabs <- ls(pattern="^lifeTab[A-Z](.+)")
for (i in 1:length(lifeTabs)){
  print(get(lifeTabs[i]))
}


# Saving life tables
for (i in 1:length(lifeTabs)){
  assign("lifeTabNByC", get(lifeTabs[i]))
  save(file=paste("../../RObjects/", lifeTabs[i], ".RData", sep=""), lifeTabNByC)
}


###################################### FITING SILER MODEL ##########################################

# Selecting ages 2,3 (rows 3,4)
for (j in 1:length(lifeTabs)){
    for (i in c(2)){
      young <- get(lifeTabs[j])[i:7, c("ageCl", "qx")]
      #  young <- young[young$ageCl!=3 & young$ageCl!=4 ,] # el problema no es que sea alta al principio, es que la media total está muy elevada
      adult <- get(lifeTabs[j])[-c(1:2), c("ageCl", "qx")]
      adult <- adult[!is.na(adult$qx),]
      adult <- adult[adult$qx!=0,]
      old <- get(lifeTabs[j])[12:30, c("ageCl", "qx")]
      old <- old[!is.na(old$qx),]
      #old <- old[old$qx!=0,]
      ag <- c(0,seq(1:30))
# Contanst mortality
      assign(paste(substr(lifeTabs[j], 8, nchar(lifeTabs[j])), "a2", sep=""), min(adult$qx))
      #a2 <- min(adult$qx)
# Juveninle mortality
      young$qx <- (young$qx) - get(paste(substr(lifeTabs[j], 8, nchar(lifeTabs[j])), "a2", sep=""))
      young <- rbind(young,data.frame(ageCl=young$ageCl[nrow(young)]:29, qx=rep(0,30-young$ageCl[nrow(young)])))
      exp <- nls(qx ~ a*exp(b*ageCl), data = young, start = list(a = 0.1, b = -0.1),control= nls.control(maxiter = 1e+02))
      a1 <- coef(exp)[[1]]
      b1 <- coef(exp)[[2]]
      fitY <- a1 * exp(b1 * ag)
# Senescence mortality
      old$qx <- (old$qx) - get(paste(substr(lifeTabs[j], 8, nchar(lifeTabs[j])), "a2", sep=""))
      old$qx[old$qx<0] <- 0
      old <- rbind(data.frame(ageCl=0:(old$ageCl[1]-1), qx=rep(0,old$ageCl[1])),old)
      #old <- old[1:11,]
      expS <- nls(qx ~ a*exp(b*ageCl), data = old, start = list(a=0.1,  b = 0.1),control= nls.control(maxiter = 1e+09))
      a3 <- coef(expS)[[1]]
      b3 <- coef(expS)[[2]]
      fitS <- a3 * exp(b3 * ag)
# Siler Model
      Siler <- a1*exp(b1*ag)+get(paste(substr(lifeTabs[j], 8, nchar(lifeTabs[j])), "a2", sep=""))+a3*exp(b3*ag)
# Data to the Siler plot
      t <- rbind(get(lifeTabs[j])[,c("ageCl", "qx")], c(30,NA))
      assign(paste(substr(lifeTabs[j], 8, nchar(lifeTabs[j])), "Siler0", i-2, sep=""), cbind(t,fitY, fitS, Siler))
    }
}

# Saving Siler mortalities for proofs.r
save(file="../../RObjects/NByCSiler00.RData", NByCSiler00)

## Plotting Siler model without by catch ##
SilMod <- ls(pattern="(.+)Siler(.+)")
png("../plots/DDE_SilerNByC.png", width=600, height=400)
for (i in 1){ # :length(SilMod)
  plot(get(lifeTabs[i])[,c("ageCl", "qx")], xlim=c(0,30), xlab="age", ylab="survivorship and mortality", 
       main="Mortality and Survivorship -Siler model- ")
  abline(h=get(paste(substr(SilMod[i], 1, nchar(SilMod[i])-7), "a2", sep="")), col=4, lty=1)
  lines(get(SilMod[i])$ageCl, get(SilMod[i])$fitY+
          get(paste(substr(SilMod[i], 1, nchar(SilMod[i])-7), "a2", sep="")), col=2, lty=1)
  lines(get(SilMod[i])$ageCl, get(SilMod[i])$fitS+
          get(paste(substr(SilMod[i], 1, nchar(SilMod[i])-7), "a2", sep="")), col=3, lty=1)
  lines (get(SilMod[i])$ageCl, get(SilMod[i])$Siler, col=1, lty=1, lwd=2)
}
legend(10,1,legend=c("total mortality","young Siler mortality","constant Siler mortality", "old Siler mortality"), 
       lwd=c(3,1,1,1), lty=c(1,1,1,1), col=c("black", "red", "blue", "green"))
dev.off()


## PREDICTED SILER LIFE TABLE (Predicted strandings amounts from Siler mortality) ##

for (i in c(2)){
  lifeSiler <- data.frame(ageCl=c(0,seq(1:30)), qxS=round(get(paste("NByCSiler0",i-2,sep=""))$Siler,3))
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
  lifeSiler <- lifeSiler[-nrow(lifeSiler),]
  assign(paste("lifeSiler0",i-1,sep=""), lifeSiler)
  rm(lifeSiler)
}

lifeSilers <- ls(pattern="^lifeSiler[0-1]")
lifeSilerNByC <- get(lifeSilers[1]) 
#for (i in 1:length(lifeTabs)){
#  print(get(lifeTabs[i]))
#}

# Saving life tables
#for (i in 1:length(lifeSilers)){
#  assign("lifeSiler", get(lifeSilers[i]))
  save(file=paste("../../RObjects/lifeSilerNByC.RData", sep=""), lifeSilerNByC)
#}



# Ploting all the mortalities #

#load("../../RObjects/lifeSiler01.RData")

#ages <- 0:29
#plot(ages, lifeSiler$ZS, type="l",ylim=c(0,1)) # Total mortality
#lines(ages, lifeSilerNByC$ZS, lty=2) # Natural mortality
#lines(ages, lifeSiler$ZS - lifeSilerNByC$ZS, lty=3) # Fishing mortality





