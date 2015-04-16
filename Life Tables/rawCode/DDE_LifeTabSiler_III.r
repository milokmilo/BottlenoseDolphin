####################################################################################################
#                                 SILER LIFE TABLES 
#
#                   created: (camilo.saavedra@vi.ieo.es) 24/10/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 09/11/2013
####################################################################################################

# inputs
# "../../RObjects/lifeTables/LifeTabn.RData"

# outputs
# "../plots/DDE_Siler.png"
# "../../RObjects/lifeTables/Siler0.RData"
# "../../RObjects/lifeTables/Siler1.RData"
# "../../RObjects/lifeTables/Siler2.RData"
# "../../RObjects/lifeTables/Siler3.RData"

# IMPORTANT: Set working directory (to source file location)

## Reading life table data (original strandings data) ##
load("../../RObjects/lifeTables/LifeTabn.RData")


###################################### FITING SILER MODEL ##########################################

# zero values included in old clases to fit hazard Type II

# Selecting ages 2,3 (rows 3,4)
for (i in c(1,2,3,4)){
  young <- lifeTab[i:9, c("ageCl", "qx")]
#  young <- young[young$ageCl!=3 & young$ageCl!=4 ,] # el problema no es que sea alta al principio, es que la media total está muy elevada
  adult <- lifeTab[-c(1:2), c("ageCl", "qx")]
  adult <- adult[adult$qx!=0,]
  old <- lifeTab[7:30, c("ageCl", "qx")]
#  old <- old[old$qx!=0,]
  ag <- c(0,seq(1:30))
  # Adult mortality
  a2 <- min(adult$qx)
  # Younger mortality
  young$qx <- (young$qx) - a2
#  young <- rbind(young,data.frame(ageCl=young$ageCl[nrow(young)]:29, qx=rep(0,30-young$ageCl[nrow(young)])))
  exp <- nls(qx ~ a*exp(b*ageCl), data = young, start = list(a = 0.1, b = -0.5),control= nls.control(maxiter = 1e+03))
  a1 <- coef(exp)[[1]]
  b1 <- coef(exp)[[2]]
  fitY <- a1 * exp(b1 * ag)
  # Senescence mortality
  old$qx <- (old$qx) - a2
  old$qx[old$qx<0] <- 0
#  old <- rbind(data.frame(ageCl=0:(old$ageCl[1]-1), qx=rep(0,old$ageCl[1])),old)
  expS <- nls(qx ~ a*exp(b*ageCl), data = old, start = list(a = 0.5, b = 0.1),control= nls.control(maxiter = 1e+09))
  a3 <- coef(expS)[[1]]
  b3 <- coef(expS)[[2]]
  fitS <- a3 * exp(b3 * ag)
  # Model
  Siler <- a1*exp(b1*ag)+a2+a3*exp(b3*ag)
  # plot
  t <- rbind(lifeTab[,c("ageCl", "qx")], c(30,NA))
  assign(paste("Siler", i-1, sep=""), cbind(t,fitY, fitS, Siler))
}


## Plotting Siler model (removing two ages) ##
png("../plots/DDE_Siler.png", width=600, height=400)
plot(lifeTab[,c("ageCl", "qx")], xlim=c(0,30), xlab="age", ylab="survivorship and mortality", 
     main="Mortality and Survivorship -Siler model (removing two ages) - ")
points(lifeTab[-c(1,2),c("ageCl")], lifeTab[-c(1,2),c("qx")], pch=16)
abline(h=a2, col=4, lty=1)
#lines(Siler1$ageCl, Siler1$fitY+a2, col=2, lty=1)
lines(Siler2$ageCl, Siler2$fitY+a2, col=2, lty=1)
#lines(Siler1$ageCl, Siler1$fitS+a2, col=3, lty=1)
lines(Siler2$ageCl, Siler2$fitS+a2, col=3, lty=1)
#lines (Siler1$ageCl, Siler1$Siler, col=1, lty=1, lwd=3)
lines (Siler2$ageCl, Siler2$Siler, col=1, lty=1, lwd=3)
#lines (lifeSiler01$ageCl, lifeSiler01$lx, col=1, lty=3, lwd=3)
legend(10,1,legend=c("Survivorship","Mortality","young Siler mortality","constant Siler mortality", "old Siler mortality"), 
       lwd=c(3,3,1,1,1), lty=c(2,1,1,1,1), col=c("black", "black", "red", "blue", "green"))
dev.off()


# Saving data
sil <- ls(pattern="^Siler[0-9]")
for (i in 1:length(sil)){
  assign("siler", get(sil[i]))
  save(file=paste("../../RObjects/lifeTables/", sil[i], ".RData", sep=""), siler)
}


## PREDICTED SILER LIFE TABLE (Predicted strandings amounts from Siler mortality) ##

for (i in c(1,2,3,4)){
  lifeSiler <- data.frame(ageCl=c(0,seq(1:30)), qxS=round(get(paste("Siler",i-1,sep=""))$Siler,3))
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
  assign(paste("lifeSiler",i-1,sep=""), lifeSiler)
  rm(lifeSiler)
}

# save life tables
lifeSilers <- ls(pattern="^lifeSiler[0-9]")
for (i in 1:length(lifeSilers)){
  assign("lifeSiler", get(lifeSilers[i]))
  save(file=paste("../../RObjects/lifeTables/", lifeSilers[i], ".RData", sep=""), lifeSiler)
}



