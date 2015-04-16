####################################################################################################
#                                 SILER LIFE TABLES 
#          Siler life table from the predicted proportion of strandings
#                   created: (camilo.saavedra@vi.ieo.es) 24/10/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 09/11/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 18/11/2013
####################################################################################################

# inputs
# "../../RObjects/lifeN.RData"

# outputs
# "../plots/DDE_Siler.png"
# "../../RObjects/Siler0.RData"
# "../../RObjects/Siler1.RData"
# "../../RObjects/Siler2.RData"
# "../../RObjects/Siler3.RData"
# "../../RObjects/lifeSiler0.RData"
# "../../RObjects/lifeSiler1.RData"
# "../../RObjects/lifeSiler2.RData"
# "../../RObjects/lifeSiler3.RData"
# "../../RObjects/Sparams0.RData"
# "../../RObjects/Sparams1.RData"
# "../../RObjects/Sparams2.RData"
# "../../RObjects/Sparams3.RData"

# IMPORTANT: Set working directory (to source file location)

# Charge libraries
library(ggplot2)
library(reshape)
library(fmsb)


## Reading life table data (original strandings data) ##
load("../../RObjects/lifeN.RData")


###################################### FITING SILER MODEL ##########################################

# zero values included in old clases to fit hazard Type II

# Mean in the first 5 ages

res <- fitSiler(,lifeN$qx)
#Siler(,lifeN$qx)
a1 <- 0.2
b1 <- 1
a2 <- 0.15
a3 <- 0.5
b3 <- 1
FLAG <- res[7]
while (FLAG>0) {
  res <- fitSiler(res[1:5],Jlife$qx2005M)
  FLAG <- res[7]
}
print(res)

  young <- lifeN[1:9, c("age", "qx")]
#supYoung <- lifeN[1:9, c("age", "lx")]
  m <- mean(young$qx[1:5])
  ma <- max(young$qx[1:5])
  young$qx <- c(ma, NA, m, NA, NA, young$qx[6:9])
  tyoung <- young
#  young <- young[young$age!=3 & young$age!=4 ,] # el problema no es que sea alta al principio, es que la media total está muy elevada
  adult <- lifeN[-c(1:2), c("age", "qx")]
  adult <- adult[adult$qx!=0,]
  old <- lifeN[7:30, c("age", "qx")]
#  old <- old[old$qx!=0,]
  ag <- c(0,seq(1:30))
  # Adult mortality
  a2 <- min(adult$qx)
#  exp(-a2*adult$age)
  Madult <- rep(a2,length(ag))
  # Younger mortality
  young$qx <- (young$qx) - a2
#  young <- rbind(young,data.frame(age=young$age[nrow(young)]:29, qx=rep(0,30-young$age[nrow(young)])))
  exp <- nls(qx ~ a*exp(b*age), data = young, start = list(a = 0.1, b = -0.5),control= nls.control(maxiter = 1e+03))
  a1 <- coef(exp)[[1]]
  b1 <- coef(exp)[[2]]
  Myoung <- (a1 * exp(b1 * ag)) + a2
#  Myoung <- data.frame (age=c(lifeN$age, 30), Myoung)
#  sup <- nls(lx ~ exp((-a/b)*(1-exp(-b*age/30))), data = supYoung, start = list(a = 30, b = -1),control= nls.control(maxiter = 1e+09))

# Senescence mortality
  old$qx <- (old$qx) - a2
  old$qx[old$qx<0] <- 0
#  old <- rbind(data.frame(age=0:(old$age[1]-1), qx=rep(0,old$age[1])),old)
  expS <- nls(qx ~ a*exp(b*age), data = old, start = list(a = 0.5, b = 0.1),control= nls.control(maxiter = 1e+09))
  a3 <- coef(expS)[[1]]
  b3 <- coef(expS)[[2]]
  Msenesc <- (a3 * exp(b3 * ag)) + a2
  # Model
  Siler <- a1*exp(b1*ag)+a2+a3*exp(b3*ag)
  # plot
  t <- rbind(tyoung,lifeN[10:30,c("age", "qx")], c(30,NA))
  Siler5 <-  cbind(t,Myoung,Madult,Msenesc,Siler)
  Sparams5 <-   c(a1,b1,a2,a3,b3)


l1 <- exp((-a1/b1)*(1-exp(-b1*ag)))
l2 <- exp((-a2*ag))
l3 <- exp((a3/b3)*(1-exp(b3*ag[10:29])))
plot(l1, xlim=c(0,30))
l1


# Saving data
sil <- ls(pattern="^Siler[0-9]")
#for (i in 1:length(sil)){
#  save(list=sil[i], 
#       file=paste("../../RObjects/", sil[i], ".RData", sep=""))
#}
#silPar <- ls(pattern="^Sparams[0-9]")
#for (i in 1:length(silPar)){
#  save(list=silPar[i], 
#       file=paste("../../RObjects/", silPar[i], ".RData", sep=""))
#}


### ggPlotting ###

# Computing data
ggSil <- data.frame(NULL)
for (i in 1:length(sil)){
  assign(paste("gg",sil[i], sep=""), melt(get(sil[i])[,-2], 
                                          id.vars="age"))  
  assign(paste("gg",sil[i], sep=""), `[[<-` 
         (get(paste("gg",sil[i], sep="")), "SilMod", value=sil[i]))
  ggSil <- rbind(ggSil, get(paste("gg",sil[i], sep="")))
}


ggData <- ggSil[ggSil$SilMod=="Siler5",]
#ggData <- ggData[,-4]
ggLife <- lifeN[,c("age", "qx")]
names(ggLife) <- c("age", "value")
#ggLife$variable <- "life"
#cbind(ggData, ggLife)

gg1 <- ggplot(ggData, aes(age, value)) + ylim(0,1) +
      geom_line(aes(group=variable, colour=variable)) +
    ylab("mortality") + xlab("age") +
    ggtitle("Mortality: Siler model (removing two ages)") +
    theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top")
  
gg2 <- gg1 + geom_point(data=ggLife, size = 1.5, pch=1)
gg3 <- gg2 + geom_point(data=ggLife[-c(1,2),], )

names(t) <- c("age", "value")
gg5 <- gg1 + geom_point(data=t, size = 1.5, pch=1)

## Plotting Siler model (removing two ages) ##
############png("../plots/DDE_Siler.png", width=600, height=400)
print(gg2)
#print(gg3)
print(gg5)
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
    file=paste("../../RObjects/", lifeSilers[i], ".RData", sep=""))
}



