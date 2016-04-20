################################################################################################### #
#                 FLORIDA BOTTLENOSE DOLPHIN LIFE TABLES - A - M - F
#     Life tables from Florida Bottlenose data for male, female and both together
#    raw data -number of stranded dolphins- extracted from Stolen and Barlow, 2003                    
#                created: (camilo.saavedra@vi.ieo.es) 04/06/2014
#                modified: (camilo.saavedra@vi.ieo.es) 13/04/2015
################################################################################################### #

## inputs
## Stolen & Barlow, 2003

## outputs
## "../../RObjects/TTR_lifeA-S&B.RData"
## "../../RObjects/TTR_lifeM-S&B.RData"
## "../../RObjects/TTR_lifeF-S&B.RData"
## "../../RObjects/TTR_SilerA-S&B.RData"
## "../../RObjects/TTR_SparamsA-S&B.RData"
## "../../RObjects/TTR_SilerM-S&B.RData"
## "../../RObjects/TTR_SparamsM-S&B.RData"
## "../../RObjects/TTR_SilerF-S&B.RData"
## "../../RObjects/TTR_SparamsF-S&B.RData"
## "../plots/TTR_SilerA-S&B.png"
## "../plots/TTR_SilSurvA-S&B.png"
## "../plots/TTR_SilerM-S&B.png"
## "../plots/TTR_SilSurvM-S&B.png"
## "../plots/TTR_SilerF-S&B.png"
## "../plots/TTR_SilSurvF-S&B.png"


# IMPORTANT: Set working directory (to source file location)
#setwd("./Life Tables/finalCode")
#setwd("../../Life Tables/finalCode")


## Charge libraries
library(ggplot2)
library(reshape2)


############### Life table both sexes ###############

BotM <- c(36, 14, 32, 15, 13, 8, 4, 5, 3, 3, 2, 3, 3, 6, 3, 8, 5, 5, 8, 3, 
            11, 4, 1, 4, 3, 3, 6, 2, 1, 2, 0, 1, 0, 1, 0, 2)

BotAges <- 0:(length(BotM)-1)

BotStr <- data.frame(age=BotAges, M=BotM)

lifeBot <- BotStr
n <- 1000
## S - Creating survivorship column
d <- sum(lifeBot$M) # sum of deaths
for (j in 1:nrow(lifeBot)){
  lifeBot$S[[j]] <- d-sum(lifeBot$M[1:j-1])} 
## nx - Standardizing survivors [(N/∑M) * 1000]
lifeBot$nx <- round((lifeBot$S/d)*n,3) 
## dx - Dolphins death-at-age [nx - n(x+1)]
for (j in 1:nrow(lifeBot)) {
  if (j == 1) {dx <- vector("numeric")}
  d <- round(lifeBot$nx[j]-lifeBot$nx[j+1],3)
  if (j == nrow(lifeBot)) {d <- round(lifeBot$nx[j]-0,3)} 
  dx <- c(dx,d)}
lifeBot$dx <- dx
## qx - Death-at-age probability [dx / nx]
for (j in 1:nrow(lifeBot)) {
  if (j == 1) {qx <- vector("numeric")}
  q <- round(lifeBot$dx[j]/lifeBot$nx[j],3)
  qx <- c(qx,q) }
lifeBot$qx <- qx 
## lx - Survivorship-at-age percent [nx / n]
lifeBot$lx <- lifeBot$nx/n 
## ex - Life expentancy at age ex = ∑ly/lx
ex <- vector("numeric")
for (j in 1:nrow(lifeBot)) {
  e <- round(sum(lifeBot$lx[j:nrow(lifeBot)])/lifeBot$lx[j],3)
  ex <- c(ex,e) }
lifeBot$ex <- ex
## Z - Total mortality-at-age -L(nt/no)/t
Z <- c(NA)
for (j in 1:nrow(lifeBot)){
  if (j == 1) {Z <- vector("numeric")}
  z <- round(-log(lifeBot$nx[j+1]/lifeBot$nx[j])/1,2)
  if (j == nrow(lifeBot)) {z <- 1.00}
  Z <- c(Z,z)
}
lifeBot$Z <- Z

lifeBotA <- lifeBot

## Saving life table
save(lifeBotA, file="../../RObjects/TTR_lifeA-S&B.RData")


## Siler mortality model 

ag <- c(0,seq(1:nrow(lifeBot)))
## Adult mortality
adult <- lifeBot[, c("age", "qx")]
adult <- adult[adult$qx!=0,]
a2 <- min(adult$qx)
Madult <- rep(a2,length(ag))
## Younger mortality
young <- lifeBot[1:10, c("age", "qx")]
#tyoung <- young
young$qx <- (young$qx) - a2
exp <- nls(qx ~ a*exp(-b*age), data = young, start = list(a = 0.1, b = -0.5),
           control= nls.control(maxiter = 1e+03))
a1 <- coef(exp)[[1]]
b1 <- coef(exp)[[2]]
Myoung <- (a1 * exp(-b1 * ag)) + a2
## Senescence mortality
old <- lifeBot[11:nrow(lifeBot), c("age", "qx")]
old$qx <- (old$qx) - a2
old$qx[old$qx<=0] <- NA
expS <- nls(qx ~ a*exp(b*age), data = old, start = list(a = 0.5, b = -0.1),
            control= nls.control(maxiter = 1e+09))
a3 <- coef(expS)[[1]]
b3 <- coef(expS)[[2]]
Msenesc <- (a3 * exp(b3 * ag)) + a2
## Model
BotSiler <- a1*exp(-b1*ag)+a2+a3*exp(b3*ag)
round(BotSiler[(0:10)+1], 3)
# 0.145 0.115 0.093 0.077 0.065 0.056 0.050 0.045 0.042 0.040 0.039
round(BotSiler[(8:35)+1], 3)
# 0.042 0.040 0.039 0.039 0.039 0.040 0.042 0.044 0.047 0.051 0.055 0.061 0.069 0.078 0.089 0.103 0.121 0.142 0.168 0.199 0.239 0.287 0.346 0.419 0.508 0.618 0.752 0.918


## Siler table
t <- rbind(lifeBot[1:nrow(lifeBot),c("age", "qx")], c(nrow(lifeBot),NA))
#t <- rbind(tyoung,lifeBot[10:30,c("age", "qx")], c(30,NA))
SilerA <-  cbind(t,Myoung,Madult,Msenesc,BotSiler)
SparmsA <-   c(a1,a2,a3,b1,b3)


## Saving Siler data
save(SilerA, file="../../RObjects/TTR_SilerA-S&B.RData")
save(SparmsA, file="../../RObjects/TTR_SparamsA-S&B.RData")


## Mortality plot
ggData <- melt(SilerA[,-2], id.vars="age")  
ggLife <- lifeBot[,c("age", "qx")]
names(ggLife) <- c("age", "value")

gg1 <- ggplot(ggData, aes(age, value)) + ylim(0,1) +
  geom_line(aes(group=variable, colour=variable)) +
  ylab("mortality \n") + xlab("year") +
  ggtitle("Bottlenose dolphin Siler mortality - Stolen & Barlow, 2003") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="null",
        axis.text.x = element_text(angle = 0, hjust = 1, size=13),
        axis.text.y = element_text(angle = 90, hjust = 1, size=13),
        axis.title.x = element_text(angle = 0, size=15),
        axis.title.y = element_text(angle = 90, size=15))

gg2 <- gg1 + geom_point(data=ggLife)

## Plotting Siler model ##
png("../plots/TTR_SilerA-S&B.png", width=600, height=400)
print(gg2)
dev.off()

## Total Survivorship
l1 <- exp((-SparmsA[1]/SparmsA[4])*(1-exp(-SparmsA[4]*ag)))
l2 <- exp((-SparmsA[2]*ag))
l3 <- exp((SparmsA[3]/SparmsA[5])*(1-exp(SparmsA[5]*ag)))
l <- l1*l2*l3
lage <- data.frame(age=ag, l=l)

## Survivorship Plot
ggSurv <- cbind(lage, obs=c(lifeBot[,"lx"],0))

ddSurv <- ggplot(ggSurv, aes(age, l)) + 
  geom_line( colour = "red") + geom_point(aes(age, obs), pch=1) +
  ylab("survivorship \n") + xlab("year") +
  ggtitle("Bottlenose dolphin survivorship - Stolen & Barlow, 2003") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top",
        axis.text.x = element_text(angle = 0, hjust = 1, size=13),
        axis.text.y = element_text(angle = 90, hjust = 1, size=13),
        axis.title.x = element_text(angle = 0, size=15),
        axis.title.y = element_text(angle = 90, size=15))

## Plotting Siler model (removing two ages) ##
png("../plots/TTR_SilSurvA-S&B.png", width=600, height=400)
print(ddSurv)
dev.off()


############### Life table Male Bottlenose dolphins ###############

BotMM <- c(22, 9, 17, 10, 9, 5, 4, 4, 3, 0, 1, 3, 1, 5, 1, 7, 2, 3, 4, 2, 3, 
           1, 0, 2, 2, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)

BotMAges <- 0:(length(BotMM)-1)

BotMStr <- data.frame(age=BotMAges, M=BotMM)

lifeBotM <- BotMStr
n <- 1000
## S - Creating survivorship column
d <- sum(lifeBotM$M) # sum of deaths
for (j in 1:nrow(lifeBotM)){
  lifeBotM$S[[j]] <- d-sum(lifeBotM$M[1:j-1])} 
## nx - Standardizing survivors [(N/∑M) * 1000]
lifeBotM$nx <- round((lifeBotM$S/d)*n,3) 
## dx - Dolphins death-at-age [nx - n(x+1)]
for (j in 1:nrow(lifeBotM)) {
  if (j == 1) {dx <- vector("numeric")}
  d <- round(lifeBotM$nx[j]-lifeBotM$nx[j+1],3)
  if (j == nrow(lifeBotM)) {d <- round(lifeBotM$nx[j]-0,3)} 
  dx <- c(dx,d)}
lifeBotM$dx <- dx
## qx - Death-at-age probability [dx / nx]
for (j in 1:nrow(lifeBotM)) {
  if (j == 1) {qx <- vector("numeric")}
  q <- round(lifeBotM$dx[j]/lifeBotM$nx[j],3)
  qx <- c(qx,q) }
lifeBotM$qx <- qx 
## lx - Survivorship-at-age percent [nx / n]
lifeBotM$lx <- lifeBotM$nx/n 
## ex - Life expentancy at age ex = ∑ly/lx
ex <- vector("numeric")
for (j in 1:nrow(lifeBotM)) {
  e <- round(sum(lifeBotM$lx[j:nrow(lifeBotM)])/lifeBotM$lx[j],3)
  ex <- c(ex,e) }
lifeBotM$ex <- ex
## Z - Total mortality-at-age -L(nt/no)/t
Z <- c(NA)
for (j in 1:nrow(lifeBotM)){
  if (j == 1) {Z <- vector("numeric")}
  z <- round(-log(lifeBotM$nx[j+1]/lifeBotM$nx[j])/1,2)
  if (j == nrow(lifeBotM)) {z <- 1.00}
  Z <- c(Z,z)
}
lifeBotM$Z <- Z

## Saving life table
save(lifeBotM, file="../../RObjects/TTR_lifeM-S&B.RData")

## Siler mortality model 

ag <- c(0,seq(1:nrow(lifeBotM)))
## Adult mortality
adult <- lifeBotM[, c("age", "qx")]
adult <- adult[adult$qx!=0,]
adult <- adult[!is.na(adult$qx),]
a2 <- min(adult$qx)
#a2 <- 0
Madult <- rep(a2,length(ag))
## Younger mortality
young <- lifeBotM[1:10, c("age", "qx")]
#tyoung <- young
young$qx <- (young$qx) - a2
exp <- nls(qx ~ a*exp(-b*age), data = young, start = list(a = 0.1, b = -0.5),
           control= nls.control(maxiter = 1e+03))
a1 <- coef(exp)[[1]]
b1 <- coef(exp)[[2]]
Myoung <- (a1 * exp(-b1 * ag)) + a2
## Senescence mortality
old <- lifeBotM[11:nrow(lifeBotM), c("age", "qx")]
old$qx <- (old$qx) - a2
old$qx[old$qx<=0] <- NA
expS <- nls(qx ~ a*exp(b*age), data = old, start = list(a = 0.5, b = -0.1),
            control= nls.control(maxiter = 1e+09))
a3 <- coef(expS)[[1]]
b3 <- coef(expS)[[2]]
Msenesc <- (a3 * exp(b3 * ag)) + a2
## Model
BotSiler <- a1*exp(-b1*ag)+a2+a3*exp(b3*ag)


## Stolen and Barlow, 2003
#a1 <- 0.1914 
#a2 <- 0
#a3 <- 0.0129
#b1 <- 0.2363
#b3 <- 0.1103

## Siler table
t <- rbind(lifeBotM[1:nrow(lifeBotM),c("age", "qx")], c(nrow(lifeBotM),NA))
SilerM <-  cbind(t,Myoung,Madult,Msenesc,BotSiler)
SparmsM <-   c(a1,a2,a3,b1,b3)


## Saving Siler data
save(SilerM, file="../../RObjects/TTR_SilerM-S&B.RData")
save(SparmsM, file="../../RObjects/TTR_SparamsF-S&B.RData")


## Mortality plot
ggData <- melt(SilerM[,-2], id.vars="age")  
ggLife <- lifeBotM[,c("age", "qx")]
names(ggLife) <- c("age", "value")

gg1 <- ggplot(ggData, aes(age, value)) + ylim(0,1) +
  geom_line(aes(group=variable, colour=variable)) +
  ylab("mortality \n") + xlab("year") +
  ggtitle("Siler mortality of Male Bottlenose dolphins - Stolen & Barlow, 2003") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="null",
        axis.text.x = element_text(angle = 0, hjust = 1, size=13),
        axis.text.y = element_text(angle = 90, hjust = 1, size=13),
        axis.title.x = element_text(angle = 0, size=15),
        axis.title.y = element_text(angle = 90, size=15))

gg2 <- gg1 + geom_point(data=ggLife)

## Plotting Siler model ##
png("../plots/TTR_SilerM-S&B.png", width=600, height=400)
print(gg2)
dev.off()

## Total Survivorship
l1 <- exp((-SparmsM[1]/SparmsM[4])*(1-exp(-SparmsM[4]*ag)))
l2 <- exp((-SparmsM[2]*ag))
l3 <- exp((SparmsM[3]/SparmsM[5])*(1-exp(SparmsM[5]*ag)))
l <- l1*l2*l3

lage <- data.frame(age=ag, l=l)

## Survivorship Plot
ggSurv <- cbind(lage, obs=c(lifeBotM[,"lx"],0))

ddSurv <- ggplot(ggSurv, aes(age, l)) + 
  geom_line( colour = "red") + geom_point(aes(age, obs), pch=1) +
  ylab("survivorship \n") + xlab("year") +
  ggtitle("Survivorship of Male Bottlenose dolphins - Stolen & Barlow, 1993") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top",
        axis.text.x = element_text(angle = 0, hjust = 1, size=13),
        axis.text.y = element_text(angle = 90, hjust = 1, size=13),
        axis.title.x = element_text(angle = 0, size=15),
        axis.title.y = element_text(angle = 90, size=15))

## Plotting Siler model (removing two ages) ##
png("../plots/TTR_SilSurvM-S&B.png", width=600, height=400)
print(ddSurv)
dev.off()


############### Life table Female Bottlenose dolphins ###############

BotFM <- c(10, 5, 13, 4, 2, 3, 0, 0, 0, 3, 0, 0, 2, 1, 2, 1, 3, 1, 3, 
           1, 7, 2, 1, 1, 1, 1, 4, 1, 1, 2, 0, 1, 0, 1, 0, 1)

BotFAges <- 0:(length(BotFM)-1)

BotFStr <- data.frame(age=BotFAges, M=BotFM)

lifeBotF <- BotFStr
n <- 1000
## S - Creating survivorship column
d <- sum(lifeBotF$M) # sum of deaths
for (j in 1:nrow(lifeBotF)){
  lifeBotF$S[[j]] <- d-sum(lifeBotF$M[1:j-1])} 
## nx - Standardizing survivors [(N/∑M) * 1000]
lifeBotF$nx <- round((lifeBotF$S/d)*n,3) 
## dx - Dolphins death-at-age [nx - n(x+1)]
for (j in 1:nrow(lifeBotF)) {
  if (j == 1) {dx <- vector("numeric")}
  d <- round(lifeBotF$nx[j]-lifeBotF$nx[j+1],3)
  if (j == nrow(lifeBotF)) {d <- round(lifeBotF$nx[j]-0,3)} 
  dx <- c(dx,d)}
lifeBotF$dx <- dx
## qx - Death-at-age probability [dx / nx]
for (j in 1:nrow(lifeBotF)) {
  if (j == 1) {qx <- vector("numeric")}
  q <- round(lifeBotF$dx[j]/lifeBotF$nx[j],3)
  qx <- c(qx,q) }
lifeBotF$qx <- qx 
## lx - Survivorship-at-age percent [nx / n]
lifeBotF$lx <- lifeBotF$nx/n 
## ex - Life expentancy at age ex = ∑ly/lx
ex <- vector("numeric")
for (j in 1:nrow(lifeBotF)) {
  e <- round(sum(lifeBotF$lx[j:nrow(lifeBotF)])/lifeBotF$lx[j],3)
  ex <- c(ex,e) }
lifeBotF$ex <- ex
## Z - Total mortality-at-age -L(nt/no)/t
Z <- c(NA)
for (j in 1:nrow(lifeBotF)){
  if (j == 1) {Z <- vector("numeric")}
  z <- round(-log(lifeBotF$nx[j+1]/lifeBotF$nx[j])/1,2)
  if (j == nrow(lifeBotF)) {z <- 1.00}
  Z <- c(Z,z)
}
lifeBotF$Z <- Z

## Saving life table
save(lifeBotF, file="../../TTR_lifeF-S&B.RData")

## Siler mortality model 

ag <- c(0,seq(1:nrow(lifeBotF)))
## Adult mortality
adult <- lifeBotF[, c("age", "qx")]
adult <- adult[adult$qx!=0,]
adult <- adult[!is.na(adult$qx),]
a2 <- min(adult$qx)
#a2 <- 0
Madult <- rep(a2,length(ag))
## Younger mortality
young <- lifeBotF[1:10, c("age", "qx")]
#tyoung <- young
young$qx <- (young$qx) - a2
exp <- nls(qx ~ a*exp(-b*age), data = young, start = list(a = 0.1, b = -0.5),
           control= nls.control(maxiter = 1e+03))
a1 <- coef(exp)[[1]]
b1 <- coef(exp)[[2]]
Myoung <- (a1 * exp(-b1 * ag)) + a2
## Senescence mortality
old <- lifeBotF[11:nrow(lifeBotF), c("age", "qx")]
old$qx <- (old$qx) - a2
old$qx[old$qx<=0] <- NA
expS <- nls(qx ~ a*exp(b*age), data = old, start = list(a = 0.5, b = -0.1),
            control= nls.control(maxiter = 1e+09))
a3 <- coef(expS)[[1]]
b3 <- coef(expS)[[2]]
Msenesc <- (a3 * exp(b3 * ag)) + a2
## Model
BotSiler <- a1*exp(-b1*ag)+a2+a3*exp(b3*ag)


## Stolen and Barlow, 2003
#a1 <- 0.1937
#a2 <- 0
#a3 <- 0.0064
#b1 <- 0.3237
#b3 <- 0.1310

## Siler table
t <- rbind(lifeBotF[1:nrow(lifeBotF),c("age", "qx")], c(nrow(lifeBotF),NA))
SilerF <-  cbind(t,Myoung,Madult,Msenesc,BotSiler)
SparmsF <-   c(a1,a2,a3,b1,b3)


## Saving Siler data
save(SilerF, file="../../RObjects/TTR_SilerF-S&B.RData")
save(SparmsF, file="../../RObjects/TTR_SparamsF-S&B.RData")


## Mortality plot
ggData <- melt(SilerF[,-2], id.vars="age")  
ggLife <- lifeBotF[,c("age", "qx")]
names(ggLife) <- c("age", "value")

gg1 <- ggplot(ggData, aes(age, value)) + ylim(0,1) +
  geom_line(aes(group=variable, colour=variable)) +
  ylab("mortality \n") + xlab("year") +
  ggtitle("Siler mortality of Female Bottlenose dolphins - Stolen & Barlow, 1993") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="null",
        axis.text.x = element_text(angle = 0, hjust = 1, size=13),
        axis.text.y = element_text(angle = 90, hjust = 1, size=13),
        axis.title.x = element_text(angle = 0, size=15),
        axis.title.y = element_text(angle = 90, size=15))

gg2 <- gg1 + geom_point(data=ggLife)

## Plotting Siler model ##
png("../plots/TTR_SilerF-S&B.png", width=600, height=400)
print(gg2)
dev.off()

## Total Survivorship
l1 <- exp((-SparmsF[1]/SparmsF[4])*(1-exp(-SparmsF[4]*ag)))
l2 <- exp((-SparmsF[2]*ag))
l3 <- exp((SparmsF[3]/SparmsF[5])*(1-exp(SparmsF[5]*ag)))
l <- l1*l2*l3

lage <- data.frame(age=ag, l=l)

## Survivorship Plot
ggSurv <- cbind(lage, obs=c(lifeBotF[,"lx"],0))

ddSurv <- ggplot(ggSurv, aes(age, l)) + 
  geom_line( colour = "red") + geom_point(aes(age, obs), pch=1) +
  ylab("survivorship \n") + xlab("year") +
  ggtitle("Survivorship of Female Bottlenose dolphins - Stolen & Barlow, 1993") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top",
        axis.text.x = element_text(angle = 0, hjust = 1, size=13),
        axis.text.y = element_text(angle = 90, hjust = 1, size=13),
        axis.title.x = element_text(angle = 0, size=15),
        axis.title.y = element_text(angle = 90, size=15))

## Plotting Siler model (removing two ages) ##
png("../plots/TTR_SilSurvF-S&B.png", width=600, height=400)
print(ddSurv)
dev.off()


