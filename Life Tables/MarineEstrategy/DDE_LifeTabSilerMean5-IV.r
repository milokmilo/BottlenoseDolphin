####################################################################################################
#                       SILER LIFE TABLES (Mean first 5 ages)
#          Siler life table from the predicted proportion of strandings
#                   created: (camilo.saavedra@vi.ieo.es) 24/10/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 09/11/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 18/11/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 05/05/2014
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


## Reading life table data (original strandings data) ##
load("../../RObjects/lifeN.RData")

###################################### FITING SILER MODEL ##########################################

# Mean in the first 5 ages

ag <- c(0,seq(1:30))
# Adult mortality
adult <- lifeN[-c(1:2), c("age", "qx")]
adult <- adult[adult$qx!=0,]
a2 <- min(adult$qx)
Madult <- rep(a2,length(ag))
# Younger mortality
young <- lifeN[1:9, c("age", "qx")]
m <- mean(young$qx[1:5])
ma <- max(young$qx[1:5])
young$qx <- c(ma, m, m, m, m, young$qx[6:9])
#tyoung <- young
young$qx <- (young$qx) - a2
exp <- nls(qx ~ a*exp(b*age), data = young, start = list(a = 0.1, b = -0.5),control= nls.control(maxiter = 1e+03))
a1 <- coef(exp)[[1]]
b1 <- coef(exp)[[2]]
Myoung <- (a1 * exp(b1 * ag)) + a2
# Senescence mortality
old <- lifeN[7:30, c("age", "qx")]
old$qx <- (old$qx) - a2
old$qx[old$qx<0] <- NA
expS <- nls(qx ~ a*exp(b*age), data = old, start = list(a = 0.5, b = -0.1),control= nls.control(maxiter = 1e+09))
a3 <- coef(expS)[[1]]
b3 <- coef(expS)[[2]]
Msenesc <- (a3 * exp(b3 * ag)) + a2
# Model
Siler <- a1*exp(b1*ag)+a2+a3*exp(b3*ag)
# plot
t <- rbind(lifeN[1:30,c("age", "qx")], c(30,NA))
#t <- rbind(tyoung,lifeN[10:30,c("age", "qx")], c(30,NA))
Siler5 <-  cbind(t,Myoung,Madult,Msenesc,Siler)
Sparams5 <-   c(a1,b1,a2,a3,b3)

# Mortality plot
ggData <- melt(Siler5[,-2], id.vars="age")  
ggLife <- lifeN[,c("age", "qx")]
names(ggLife) <- c("age", "value")

gg1 <- ggplot(ggData, aes(age, value)) + ylim(0,1) +
  geom_line(aes(group=variable, colour=variable)) +
  ylab("Mortalidad \n") + xlab("Edad") +
  ggtitle("") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="null",
        axis.text.x = element_text(angle = 0, hjust = 1, size=13),
        axis.text.y = element_text(angle = 90, hjust = 1, size=13),
        axis.title.x = element_text(angle = 0, size=15),
        axis.title.y = element_text(angle = 90, size=15))

gg2 <- gg1 + geom_point(data=ggLife, size = 1.5, pch=1)
gg3 <- gg2 + geom_point(data=ggLife[-c(1,2,3,4,5),], )

## Plotting Siler model (removing two ages) ##
#png("../plots/DDE_Siler.png", width=600, height=400)
#print(gg2)
print(gg3)
#print(gg5)
#dev.off()


# Total Survivorship

l1 <- exp((Sparams5[1]/Sparams5[2])*(1-exp(Sparams5[2]*ag)))
l2 <- exp((-Sparams5[3]*ag))
l3 <- exp((Sparams5[4]/Sparams5[5])*(1-exp(Sparams5[5]*ag)))
l <- l1*l2*l3
lage <- data.frame(age=ag, l=l)

# Survivorship Plot
ggSurv <- cbind(lage, obs=c(lifeN[,"lx"],0))

ddSurv <- ggplot(ggSurv, aes(age, l)) + 
      geom_line( colour = "red") + geom_point(aes(age, obs), pch=1) +
      ylab("Supervivencia \n") + xlab("Edad") +
      ggtitle("") +
      theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
            legend.title=element_blank(), legend.position="top",
            axis.text.x = element_text(angle = 0, hjust = 1, size=13),
            axis.text.y = element_text(angle = 90, hjust = 1, size=13),
            axis.title.x = element_text(angle = 0, size=15),
            axis.title.y = element_text(angle = 90, size=15))

print(ddSurv)

### Bottlenose data ###

bot.qx <- c(0.164, 0.076,	0.188,	0.109,	0.106,	0.073,	0.039,	0.051,	0.032,
            0.033,	0.023,	0.035,	0.037,	0.076,	0.041,	0.114,	0.081,	0.088,
            0.154,	0.068,	0.268,	0.133,	0.038,	0.16,	0.143,	0.167,	0.4,	0.222,
            0.143,	0.333,	0,	0.25,	0,	0.333,	0,	1)

lifeSiler <- data.frame(age=c(0,seq(1:35)), qx=round(bot.qx,3))
n <- 1000
# nx and dx - survivors and deaths at age
for (j in 1:nrow(lifeSiler)){
  if (j == 1) {dx <- lifeSiler$qx[1]*n; nx <- n
  } else {
    nx <- c(nx, nx[j-1]- dx[j-1])
    dx <- c(dx, lifeSiler$qx[j]*nx[j])
  }
}
lifeSiler <- data.frame(lifeSiler, nx=round(nx,0), dx=round(dx,0))
# lx - Survivorship-at-age percent 
lifeSiler$lx <- round(lifeSiler$nx/n, 3)
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

bottle <- data.frame(age = 30/35 * 0:35, qx=bot.qx) # Convertimo edades 0:35 en edades 0:30 

# Model for common dolphin (30 ages)
ag <- c(0:30)
# Adult mortality
adult <- bottle[, c("age", "qx")]
adult <- adult[adult$qx!=0,]
a2 <- min(adult$qx)
Madult <- rep(0,length(ag))
# Younger mortality
young <- bottle[c(1:10), c("age", "qx")]
young$qx <- (young$qx)  - a2
exp <- nls(qx ~ a*exp(b*age), data = young, start = list(a = 0.1, b = -0.5),control= nls.control(maxiter = 1e+03))
a1 <- coef(exp)[[1]]
b1 <- coef(exp)[[2]]
Myoung <- (a1 * exp(b1 * ag))  #+ a2
# Senescence mortality
old <- bottle[10:36, c("age", "qx")]
old$qx <- (old$qx)  - a2
old$qx[old$qx<0] <- NA
expS <- nls(qx ~ a*exp(b*age), data = old, start = list(a = 0.5, b = -0.1),control= nls.control(maxiter = 1e+09))
a3 <- coef(expS)[[1]]
b3 <- coef(expS)[[2]]
Msenesc <- (a3 * exp(b3 * ag))  #+ a2
# Model
Siler <- a1*exp(b1*ag)+a3*exp(b3*ag)  #+ a2
# plot
SilerB <-  cbind(ag,Myoung,Madult,Msenesc,Siler)
SilerB <- as.data.frame(SilerB)
SparamsB <- c(a1,b1,a2,a3,b3)

#a1<-0.1937;a2<-0.0000;a3<-0.0064;b1<--0.3237;b3<-0.1310
#a1<-0.0960;a2<-0.0000;a3<-0.0031;b1<--0.3317;b3<-0.1523
#a1<-0.1914;a2<-0.0000;a3<-0.0209;b1<--0.2249;b3<-0.0978
#a1<-0.1201;a2<-0.0000;a3<-0.0129;b1<--0.2363;b3<-0.1103
    

# Mortality plot
ggSilerB <- melt(SilerB, id.vars="ag")
names(ggSilerB) <- c("age", "variable", "value")
ggLife <- bottle[,c("age", "qx")]
names(ggLife) <- c("age", "value")

gg1 <- ggplot(ggSilerB, aes(age, value)) + ylim(0,1) +
  geom_line(aes(group=variable, colour=variable)) +
  ylab("Mortalidad \n") + xlab("Edad") +
  ggtitle("") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="null",
        axis.text.x = element_text(angle = 0, hjust = 1, size=13),
        axis.text.y = element_text(angle = 90, hjust = 1, size=13),
        axis.title.x = element_text(angle = 0, size=15),
        axis.title.y = element_text(angle = 90, size=15))

gg2 <- gg1 + geom_point(data=ggLife, size = 1.5, pch=1)
#gg3 <- gg2 + geom_point(data=ggLife[-c(1,2,3,4,5),], )

print(gg2)

# Mortalidad den Galicia y Natural
ggSiler <- ggplot(subset(ggSilerB,variable=="Siler"), aes(age, value)) + ylim(0,1) +
  geom_line(colour="blue") +
  ylab("Mortalidad \n") + xlab("Edad") +
  ggtitle("") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="null",
        axis.text.x = element_text(angle = 0, hjust = 1, size=13),
        axis.text.y = element_text(angle = 90, hjust = 1, size=13),
        axis.title.x = element_text(angle = 0, size=15),
        axis.title.y = element_text(angle = 90, size=15))

ggSilerMZ <- ggSiler + 
        geom_line(data=subset(ggData,variable=="Siler"), 
                  aes(age, value), colour="red")
print(ggSilerMZ)

# Natural Survivorship
ag<-0:35
lB1 <- exp((SparamsB[1]/SparamsB[2])*(1-exp(SparamsB[2]*ag)))
lB2 <- exp((-SparamsB[3]*ag))
lB3 <- exp((SparamsB[4]/SparamsB[5])*(1-exp(SparamsB[5]*ag)))
#lB <- lB1*lB2*lB3
lB <- lB1*lB3 # ADAPTACIÓN, NO SUMAMOS MORTALIDAD CONSTANTE
lBage <- data.frame(age=ag, lB=lB)

# Survivorship Plot
ggSurvB <- cbind(lBage, obs=lifeSiler[,"lx"])
ggSurvB <- ggplot(ggSurvB, aes(age, lB)) + 
    geom_line( colour = "blue") + geom_point(aes(age, obs), pch=1) +
    ylab("Supervivencia \n") + xlab("Edad") +
    ggtitle("") +
    theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
          legend.title=element_blank(), legend.position="top",
          axis.text.x = element_text(angle = 0, size=13),
          axis.text.y = element_text(angle = 90, size=13),
          axis.title.x = element_text(angle = 0, size=15),
          axis.title.y = element_text(angle = 90, size=15))

print(ggSurvB)

ag<-0:30
lB1 <- exp((SparamsB[1]/SparamsB[2])*(1-exp(SparamsB[2]*ag)))
lB2 <- exp((-SparamsB[3]*ag))
lB3 <- exp((SparamsB[4]/SparamsB[5])*(1-exp(SparamsB[5]*ag)))
#lB <- lB1*lB2*lB3
lB <- lB1*lB3 # ADAPTACIÓN, NO SUMAMOS MORTALIDAD CONSTANTE
lBage <- data.frame(age=ag, lB=lB)

ggdata <- cbind(lBage, obs=c(lifeN[,"lx"],0))
ggSurvB <- ggplot(ggdata, aes(age, lB)) + 
  geom_line( colour = "blue") + geom_point(aes(age, obs), pch=1) +
  ylab("Supervivencia \n") + xlab("Edad") +
  ggtitle("") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top",
        axis.text.x = element_text(angle = 0, size=13),
        axis.text.y = element_text(angle = 90, size=13),
        axis.title.x = element_text(angle = 0, size=15),
        axis.title.y = element_text(angle = 90, size=15))
ggSurvNM <- ggSurvB + geom_line(data=lage, aes(age, l), colour="red")

print(ggSurvNM)


#### PREDICTED SILER LIFE TABLE (Galician mortality) ####

  lifeSiler <- data.frame(age=c(0,seq(1:30)), qx=round(Siler5$Siler,3))
  n <- 1000
  # nx and dx - survivors and deaths at age
  for (j in 1:nrow(lifeSiler)){
    if (j == 1) {dx <- lifeSiler$qx[1]*n; nx <- n
    } else {
    nx <- c(nx, nx[j-1]- dx[j-1])
    dx <- c(dx, lifeSiler$qx[j]*nx[j])
    }
  }
  lifeSiler <- data.frame(lifeSiler, nx=round(nx,0), dx=round(dx,0))
  # lx - Survivorship-at-age percent 
  lifeSiler$lx <- round(lifeSiler$nx/n, 3)
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

# Mortalidad Galicia
MG <- round((sum(lifeSiler$dx)/sum(lifeSiler$nx))*100,1) # 13.5%

mat <- c(0,0,0,0,0,0,0,0,
         1/3.33,1/3.33,1/3.33,1/3.33,
         1/2.13,1/2.13,1/2.13,1/2.13,
         1/3.03,1/3.03,1/3.03,1/3.03,
         1/7.14,1/7.14,1/7.14,1/7.14,
         1/7.14,1/7.14,1/7.14,1/7.14,1/7.14,1/7.14,
         0)

## Madurez: porcentaje de hembras maduras 8no neesario, ya incluido en la tasa de preñez
#load("../../RObjects/matFemC.RData")
#m <- mat * c(Mat,0)
#round(m-mat,3) # Porcentaje de reducción si lo aplicáramos

nx0 <- lifeSiler$nx
qx <- lifeSiler$qx
sN100 <- numeric()
for(i in 0:100){
    assign(paste("fem",i,sep=""),round(get(paste("nx",i,sep=""))/1.8,0)) # 1.46 sex-ratio BD
    assign(paste("calv",i,sep=""),round(get(paste("fem",i,sep="")) * mat,0))
    assign(paste("calvS",i,sep=""),sum(get(paste("calv",i,sep=""))))
    assign(paste("n",i+1,sep=""), c(get(paste("calvS",i,sep="")),
                                    get(paste("nx",i,sep="")) -
                                    get(paste("nx",i,sep="")) *
                                    qx))
    assign(paste("nx",i+1,sep=""), 
           round(get(paste("n",i+1,sep=""))[-length(get(paste("n",i+1,sep="")))]))
    sN100 <- c(sN100,sum(get(paste("nx",i,sep=""))))
}

sN100tip <- sN100/max(sN100)*100

ggData <- data.frame(A=0:(length(sN100tip)-1), prop=sN100tip)

RedGal <- ggplot(ggData, aes(A,prop)) + geom_line(colour="red")+
          ylab("Tendencia poblacional (%) \n") + xlab("Años") +
          ggtitle("") +
          theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
                legend.title=element_blank(), legend.position="top",
                axis.text.x = element_text(angle = 0, size=13),
                axis.text.y = element_text(angle = 90, size=13),
                axis.title.x = element_text(angle = 0, size=15),
                axis.title.y = element_text(angle = 90, size=15))

print(RedGal)


#### PREDICTED SILER LIFE TABLE (Natural mortality) ####

lifeSiler <- data.frame(age=c(0,seq(1:30)), qx=round(SilerB$Siler,3))
n <- 1000
# nx and dx - survivors and deaths at age
for (j in 1:nrow(lifeSiler)){
  if (j == 1) {dx <- lifeSiler$qx[1]*n; nx <- n
  } else {
    nx <- c(nx, nx[j-1]- dx[j-1])
    dx <- c(dx, lifeSiler$qx[j]*nx[j])
  }
}
lifeSiler <- data.frame(lifeSiler, nx=round(nx,0), dx=round(dx,0))
# lx - Survivorship-at-age percent 
lifeSiler$lx <- round(lifeSiler$nx/n, 3)
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

# Mortalidad Natural
MN <- round((sum(lifeSiler$dx)/sum(lifeSiler$nx))*100,1) # 7.9%

nx0 <- lifeSiler$nx
qx <- lifeSiler$qx
sN100 <- numeric()
for(i in 0:100){
  assign(paste("fem",i,sep=""),round(get(paste("nx",i,sep=""))/1.8,0))
  assign(paste("calv",i,sep=""),round(get(paste("fem",i,sep="")) * mat,0))
  assign(paste("calvS",i,sep=""),sum(get(paste("calv",i,sep=""))))
  assign(paste("n",i+1,sep=""), c(get(paste("calvS",i,sep="")),
                                  get(paste("nx",i,sep="")) -
                                    get(paste("nx",i,sep="")) *
                                    qx))
  assign(paste("nx",i+1,sep=""), 
         round(get(paste("n",i+1,sep=""))[-length(get(paste("n",i+1,sep="")))]))
  sN100 <- c(sN100,sum(get(paste("nx",i,sep=""))))
}

sN100tip <- sN100/sN100[1]*100

ggNat <- data.frame(A=0:(length(sN100tip)-1), prop=sN100tip)

RedNat <- ggplot(ggNat, aes(A,prop)) + geom_line(colour="blue")+
  ylab("Tendencia poblacional (%) \n") + xlab("Años") +
  ggtitle("") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top",
        axis.text.x = element_text(angle = 0, size=13),
        axis.text.y = element_text(angle = 90, size=13),
        axis.title.x = element_text(angle = 0, size=15),
        axis.title.y = element_text(angle = 90, size=15))

print(RedNat)


# Gráfico tendencias (Mortalidad Natural y Galicia) 
Red <- RedGal + geom_line(data=ggNat, aes(A,prop), colour="blue")

print(Red)


#### Maximum bycatch allowable ####

load("../../RObjects/byCatch.RData")

byCatch$byCub
by <- c(byCatch$byCub,0)

ggBC <- byCatch[,c("age","byCub")]

ByC <- ggplot(ggBC, aes(age,byCub)) + geom_line() +
  ylab("% bycatch \n") + xlab("Edad") +
  ggtitle("") + 
  scale_y_continuous(limits=c(0, 1), breaks = c(0,1.00), labels = c(0,100))+
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top",
        axis.text.x = element_text(angle = 0, size=13),
        axis.text.y = element_text(angle = 90, size=13),
        axis.title.x = element_text(angle = 0, size=15),
        axis.title.y = element_text(angle = 90, size=15))

print(ByC)

# Select some parameters
n <- 1000 # Initial population

M <- SilerB$Siler 
F <- SilerB$Siler * by

fun <- function(x){
  laZ <- M + F * x
  life <- data.frame(age=0:30,qx=laZ)
  for (j in 1:nrow(life)){
    if (j == 1) {dx <- life$qx[1]*n; nx <- n
    } else {
      nx <- c(nx, nx[j-1]- dx[j-1])
      dx <- c(dx, life$qx[j]*nx[j])
    }
  }
  birth <- sum((nx/1.8)*mat)
  return(abs(1-(birth/nx[1]))) 
}

byCprop <- optimize(f=fun, interval=c(-1,5))[[1]] # 0.41


#### PREDICTED SILER LIFE TABLE (Maximum mortality) ####

lifeSiler <- data.frame(age=c(0,seq(1:30)), qx=M + F * byCprop)
n <- 1000
# nx and dx - survivors and deaths at age
for (j in 1:nrow(lifeSiler)){
  if (j == 1) {dx <- lifeSiler$qx[1]*n; nx <- n
  } else {
    nx <- c(nx, nx[j-1]- dx[j-1])
    dx <- c(dx, lifeSiler$qx[j]*nx[j])
  }
}
lifeSiler <- data.frame(lifeSiler, nx=round(nx,0), dx=round(dx,0))
# lx - Survivorship-at-age percent 
lifeSiler$lx <- round(lifeSiler$nx/n, 3)
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

# Mortalidad máxima de la población
MT <- round((sum(lifeSiler$dx)/sum(lifeSiler$nx))*100,1) # 9.1 %

MB <- MT - MN # 1.2

nx0 <- lifeSiler$nx
qx <- lifeSiler$qx
sN100 <- numeric()
for(i in 0:100){
  assign(paste("fem",i,sep=""),round(get(paste("nx",i,sep=""))/1.8,0))
  assign(paste("calv",i,sep=""),round(get(paste("fem",i,sep="")) * mat,0))
  assign(paste("calvS",i,sep=""),sum(get(paste("calv",i,sep=""))))
  assign(paste("n",i+1,sep=""), c(get(paste("calvS",i,sep="")),
                                  get(paste("nx",i,sep="")) -
                                    get(paste("nx",i,sep="")) *
                                    qx))
  assign(paste("nx",i+1,sep=""), 
         round(get(paste("n",i+1,sep=""))[-length(get(paste("n",i+1,sep="")))]))
  sN100 <- c(sN100,sum(get(paste("nx",i,sep=""))))
}

sN100tip <- sN100/sN100[1]*100

ggMax <- data.frame(A=0:(length(sN100tip)-1), prop=sN100tip)

RedMax <- ggplot(ggMax, aes(A,prop)) + geom_line(colour="green")+
  ylab("Tendencia poblacional (%) \n") + xlab("Años") +
  ggtitle("") + ylim(1,101) +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top",
        axis.text.x = element_text(angle = 0, size=13),
        axis.text.y = element_text(angle = 90, size=13),
        axis.title.x = element_text(angle = 0, size=15),
        axis.title.y = element_text(angle = 90, size=15))

print(RedMax)

# Gráfico tendencias (Mortalidad Natural, Galicia y Máxima) 
Red <- RedGal + geom_line(data=ggNat, aes(A,prop), colour="blue") +
                geom_line(data=ggMax, aes(A,prop), colour="green")

print(Red)







