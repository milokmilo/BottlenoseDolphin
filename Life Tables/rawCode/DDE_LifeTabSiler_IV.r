####################################################################################################
#                                 SILER LIFE TABLES 
#
#                   created: (camilo.saavedra@vi.ieo.es) 24/10/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 09/11/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 18/11/2013
####################################################################################################

# inputs
# "../../RObjects/lifeTables/LifeTabn.RData"

# outputs
# "../plots/DDE_Siler.png"
# "../../RObjects/lifeTables/Siler0.RData"
# "../../RObjects/lifeTables/Siler1.RData"
# "../../RObjects/lifeTables/Siler2.RData"
# "../../RObjects/lifeTables/Siler3.RData"
# "../../RObjects/lifeTables/lifeSiler0.RData"
# "../../RObjects/lifeTables/lifeSiler1.RData"
# "../../RObjects/lifeTables/lifeSiler2.RData"
# "../../RObjects/lifeTables/lifeSiler3.RData"

# IMPORTANT: Set working directory (to source file location)

## Reading life table data (original strandings data) ##
load("../../RObjects/lifeTables/lifeN.RData")


###################################### FITING SILER MODEL ##########################################

# zero values included in old clases to fit hazard Type II

# Selecting ages 2,3 (rows 3,4)
for (i in c(1,2,3,4)){
  young <- lifeN[i:9, c("age", "qx")]
#  young <- young[young$age!=3 & young$age!=4 ,] # el problema no es que sea alta al principio, es que la media total está muy elevada
  adult <- lifeN[-c(1:2), c("age", "qx")]
  adult <- adult[adult$qx!=0,]
  old <- lifeN[7:30, c("age", "qx")]
#  old <- old[old$qx!=0,]
  ag <- c(0,seq(1:30))
  # Adult mortality
  a2 <- min(adult$qx)
  # Younger mortality
  young$qx <- (young$qx) - a2
#  young <- rbind(young,data.frame(age=young$age[nrow(young)]:29, qx=rep(0,30-young$age[nrow(young)])))
  exp <- nls(qx ~ a*exp(b*age), data = young, start = list(a = 0.1, b = -0.5),control= nls.control(maxiter = 1e+03))
  a1 <- coef(exp)[[1]]
  b1 <- coef(exp)[[2]]
  fitY <- a1 * exp(b1 * ag)
  # Senescence mortality
  old$qx <- (old$qx) - a2
  old$qx[old$qx<0] <- 0
#  old <- rbind(data.frame(age=0:(old$age[1]-1), qx=rep(0,old$age[1])),old)
  expS <- nls(qx ~ a*exp(b*age), data = old, start = list(a = 0.5, b = 0.1),control= nls.control(maxiter = 1e+09))
  a3 <- coef(expS)[[1]]
  b3 <- coef(expS)[[2]]
  fitS <- a3 * exp(b3 * ag)
  # Model
  Siler <- a1*exp(b1*ag)+a2+a3*exp(b3*ag)
  # plot
  t <- rbind(lifeN[,c("age", "qx")], c(30,NA))
  assign(paste("Siler", i-1, sep=""), cbind(t,fitY, fitS, Siler))
  assign(paste("Sparams", i-1, sep=""),  c(a1,b1,a2,a3,b3))
}

# Saving data
sil <- ls(pattern="^Siler[0-9]")
for (i in 1:length(sil)){
  save(list=sil[i], 
       file=paste("../../RObjects/lifeTables/", sil[i], ".RData", sep=""))
}
silPar <- ls(pattern="^Sparams[0-9]")
for (i in 1:length(silPar)){
  save(list=silPar[i], 
       file=paste("../../RObjects/lifeTables/", silPar[i], ".RData", sep=""))
}


## Plotting Siler model (removing two ages) ##
png("../plots/DDE_Siler.png", width=600, height=400)
plot(lifeN[,c("age", "qx")], xlim=c(0,30), xlab="age", ylab="survivorship and mortality", 
     main="Mortality and Survivorship -Siler model (removing two ages) - ")
points(lifeN[-c(1,2),c("age")], lifeN[-c(1,2),c("qx")], pch=16)
abline(h=a2, col=4, lty=1)
#lines(Siler1$age, Siler1$fitY+a2, col=2, lty=1)
lines(Siler2$age, Siler2$fitY+a2, col=2, lty=1)
#lines(Siler1$age, Siler1$fitS+a2, col=3, lty=1)
lines(Siler2$age, Siler2$fitS+a2, col=3, lty=1)
#lines (Siler1$age, Siler1$Siler, col=1, lty=1, lwd=3)
lines (Siler2$age, Siler2$Siler, col=1, lty=1, lwd=3)
#lines (lifeSiler01$age, lifeSiler01$lx, col=1, lty=3, lwd=3)
legend(10,1,legend=c("Survivorship","Mortality","young Siler mortality","constant Siler mortality", "old Siler mortality"), 
       lwd=c(3,3,1,1,1), lty=c(2,1,1,1,1), col=c("black", "black", "red", "blue", "green"))
dev.off()





## PREDICTED SILER LIFE TABLE (Predicted strandings amounts from Siler mortality) ##

for (i in c(1,2,3,4)){
  lifeSiler <- data.frame(age=c(0,seq(1:30)), qx=round(get(paste("Siler",i-1,sep=""))$Siler,3))
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
  assign(paste("lifeSiler",i-1,sep=""), lifeSiler)
  rm(lifeSiler)
}

# save life tables
lifeSilers <- ls(pattern="^lifeSiler[0-9]")
for (i in 1:length(lifeSilers)){
  save(list=lifeSilers[i],
    file=paste("../../RObjects/lifeTables/", lifeSilers[i], ".RData", sep=""))
}



