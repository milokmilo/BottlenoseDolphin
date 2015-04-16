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


## Reading life table data (original strandings data) ##
load("../../RObjects/lifeN.RData")


###################################### FITING SILER MODEL ##########################################

# Mean in the first 5 ages

adult <- lifeN[-c(1:2), c("age", "qx")]
adult <- adult[adult$qx!=0,]
old <- lifeN[7:30, c("age", "qx")]
ag <- c(0,seq(1:30))
# Adult mortality
a2 <- min(adult$qx)
Madult <- rep(a2,length(ag))
# Younger mortality
young <- lifeN[1:9, c("age", "qx")]
m <- mean(young$qx[1:5])
young$qx <- c(m, m, m, m, m, young$qx[6:9])
tyoung <- young
young$qx <- (young$qx) - a2
exp <- nls(qx ~ a*exp(b*age), data = young, start = list(a = 0.1, b = -0.5),control= nls.control(maxiter = 1e+03))
a1 <- coef(exp)[[1]]
b1 <- coef(exp)[[2]]
Myoung <- (a1 * exp(b1 * ag)) + a2
# Senescence mortality
old$qx <- (old$qx) - a2
old$qx[old$qx<0] <- 0
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
ggdata <- cbind(lage, obs=c(lifeN[,"lx"],0))

ddSurv <- ggplot(ggdata, aes(age, l)) + 
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

bottle <- data.frame(age = 30/35 * 0:35, qx=bot.qx) # Convertimo edades 0:35 en edades 0:30 


# Model for common dolphin (30 ages)
ag <- c(0:30)
# Adult mortality
adult <- bottle[, c("age", "qx")]
adult <- adult[adult$qx!=0,]
a2 <- min(adult$qx)
Madult <- rep(0,length(ag))
# Younger mortality
young <- bottle[c(1,2,4:10), c("age", "qx")] # ELIMINANDO TERCER VALOR
young$qx <- (young$qx)  - a2
exp <- nls(qx ~ a*exp(b*age), data = young, start = list(a = 0.1, b = -0.5),control= nls.control(maxiter = 1e+03))
a1 <- coef(exp)[[1]]
b1 <- coef(exp)[[2]]
Myoung <- (a1 * exp(b1 * ag)) #+ a2
# Senescence mortality
old <- bottle[11:36, c("age", "qx")]
old$qx <- (old$qx) - a2
old$qx[old$qx<0] <- 0
expS <- nls(qx ~ a*exp(b*age), data = old, start = list(a = 0.5, b = 0.1),control= nls.control(maxiter = 1e+09))
a3 <- coef(expS)[[1]]
b3 <- coef(expS)[[2]]
Msenesc <- (a3 * exp(b3 * ag)) #+ a2
# Model
Siler <- a1*exp(b1*ag)+a3*exp(b3*ag) # +a2
# plot
SilerB <-  cbind(ag,Myoung,Madult,Msenesc,Siler)
SparamsB <-   c(a1,b1,a2,a3,b3)

# Mortality plot
ggData <- melt(SilerB[,-1], id.vars="age")
ggData$X1 <- ggData$X1-1
names(ggData) <- c("age", "variable", "value")
ggLife <- bottle[,c("age", "qx")]
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
#gg3 <- gg2 + geom_point(data=ggLife[-c(1,2,3,4,5),], )

print(gg2)


# Natural Survivorship

lB1 <- exp((SparamsB[1]/SparamsB[2])*(1-exp(SparamsB[2]*ag)))
lB2 <- exp((-SparamsB[3]*ag))
lB3 <- exp((SparamsB[4]/SparamsB[5])*(1-exp(SparamsB[5]*ag)))
lB <- lB1*lB2*lB3
lBage <- data.frame(age=ag, lB=lB)

# Survivorship Plot
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

print(ggSurvB)

ggSurvNM <- ggSurvB + geom_line(data=lage, aes(age, l), colour="red")

print(ggSurvNM)




#### PREDICTED SILER LIFE TABLE (Predicted strandings amounts from Siler mortality) ####

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


mat <- c(0,0,0,0,0,0,0,0,
         1/3.33,1/3.33,1/3.33,1/3.33,
         1/2.13,1/2.13,1/2.13,1/2.13,
         1/3.03,1/3.03,1/3.03,1/3.03,
         1/7.14,1/7.14,1/7.14,1/7.14,
         1/7.14,1/7.14,1/7.14,1/7.14,1/7.14,1/7.14,
         0)

lifeSiler$fem <- round(lifeSiler$nx/2,0)

lifeSiler$calv <- round(lifeSiler$fem * mat,0)

calv <- sum(lifeSiler$calv)

n1 <- c(calv, lifeSiler$nx - lifeSiler$nx * lifeSiler$qx)

lifeSiler$nx1 <- round(n1[-length(n1)])


lifeSiler$fem1 <- round(lifeSiler$nx1/2,0)

lifeSiler$calv1 <- round(lifeSiler$fem1 * mat,0)

calv1 <- sum(lifeSiler$calv1)

n2 <- c(calv1, lifeSiler$nx1 - lifeSiler$nx1 * lifeSiler$qx)

lifeSiler$nx2 <- round(n2[-length(n2)])


lifeSiler$fem2 <- round(lifeSiler$nx2/2,0)

lifeSiler$calv2 <- round(lifeSiler$fem2 * mat,0)

calv2 <- sum(lifeSiler$calv2)

n3 <- c(calv2, lifeSiler$nx2 - lifeSiler$nx2 * lifeSiler$qx)

lifeSiler$nx3 <- round(n3[-length(n3)])


lifeSiler$fem3 <- round(lifeSiler$nx3/2,0)

lifeSiler$calv3 <- round(lifeSiler$fem3 * mat,0)

calv3 <- sum(lifeSiler$calv3)

n4 <- c(calv3, lifeSiler$nx3 - lifeSiler$nx3 * lifeSiler$qx)

lifeSiler$nx4 <- round(n4[-length(n4)])


lifeSiler$fem4 <- round(lifeSiler$nx4/2,0)

lifeSiler$calv4 <- round(lifeSiler$fem4 * mat,0)

calv4 <- sum(lifeSiler$calv4)

n5 <- c(calv4, lifeSiler$nx4 - lifeSiler$nx4 * lifeSiler$qx)

lifeSiler$nx5 <- round(n5[-length(n5)])


lifeSiler$fem5 <- round(lifeSiler$nx5/2,0)

lifeSiler$calv5 <- round(lifeSiler$fem5 * mat,0)

calv5 <- sum(lifeSiler$calv5)

n6 <- c(calv5, lifeSiler$nx5 - lifeSiler$nx5 * lifeSiler$qx)

lifeSiler$nx6 <- round(n6[-length(n6)])


lifeSiler$fem6 <- round(lifeSiler$nx6/2,0)

lifeSiler$calv6 <- round(lifeSiler$fem6 * mat,0)

calv6 <- sum(lifeSiler$calv6)

n7 <- c(calv6, lifeSiler$nx6 - lifeSiler$nx6 * lifeSiler$qx)

lifeSiler$nx7 <- round(n7[-length(n7)])


lifeSiler$fem7 <- round(lifeSiler$nx7/2,0)

lifeSiler$calv7 <- round(lifeSiler$fem7 * mat,0)

calv7 <- sum(lifeSiler$calv7)

n8 <- c(calv7, lifeSiler$nx7 - lifeSiler$nx7 * lifeSiler$qx)

lifeSiler$nx8 <- round(n8[-length(n8)])


lifeSiler$fem8 <- round(lifeSiler$nx8/2,0)

lifeSiler$calv8 <- round(lifeSiler$fem8 * mat,0)

calv8 <- sum(lifeSiler$calv8)

n9 <- c(calv8, lifeSiler$nx8 - lifeSiler$nx8 * lifeSiler$qx)

lifeSiler$nx9 <- round(n9[-length(n9)])


lifeSiler$fem9 <- round(lifeSiler$nx9/2,0)

lifeSiler$calv9 <- round(lifeSiler$fem9 * mat,0)

calv9 <- sum(lifeSiler$calv9)

n10 <- c(calv9, lifeSiler$nx9 - lifeSiler$nx9 * lifeSiler$qx)

lifeSiler$nx10 <- round(n10[-length(n10)])








plot(c(sum(lifeSiler$nx),sum(lifeSiler$nx1),sum(lifeSiler$nx2),sum(lifeSiler$nx3),sum(lifeSiler$nx4),sum(lifeSiler$nx5),
  sum(lifeSiler$nx6), sum(lifeSiler$nx7), sum(lifeSiler$nx8), sum(lifeSiler$nx9), sum(lifeSiler$nx10)),
  ylim=c(0,sum(lifeSiler$nx)), type="l")
