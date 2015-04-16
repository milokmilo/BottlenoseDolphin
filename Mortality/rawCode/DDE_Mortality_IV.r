####################################################################################################
#                           MORTALITY - SURVIVORSHIP CURVE
#            
#               created: (camilo.saavedra@vi.ieo.es) 26/09/2013
#               modified: (camilo.saavedra@vi.ieo.es) 09/11/2013
####################################################################################################

# Inputs
# "../../RData/CEMMA.csv"
# "../RObjects/strPred.RData"

# Outputs
# "../../RObjects/lifeTables/lifeTab.RData"
# "../../RObjects/lifeTables/lifeTabYgr.RData"
# 


# IMPORTANT: Set working directory (to source file location)

# Charge libraries
library(ggplot2)

# Read data 
CEMMA <- read.csv("../../RData/CEMMA.csv")
ddePop <- CEMMA[CEMMA$sp == "DDE",c("age")]
ddePop <- ddePop[complete.cases(ddePop)]
# Changing age values (0.5 and 1.5 for 0 and 1)
ddePop[ddePop == 0.5] <- 0
ddePop[ddePop == 1.5] <- 1

MortAge <- table(ddePop)
MortAge <-data.frame(age = as.numeric(rownames(MortAge)), n = MortAge[])
ages <- data.frame(age=c(0,seq(1:29))) # Create a dataframe with all the ages
MortAge <- merge(ages, MortAge, by="age", all.x=TRUE) # Merge dataframes
MortAge[is.na(MortAge)] <- 0 # Replace to zeros

############################ LIFE TABLE ##############################

# age: real age of the dolphin
# M: Stranded-at-age
# S: Survivors-at-age
# Nx: Standardized survivors (by n=1000)
# dx: Death-at-age (standarized stranded dolphins) [N(x-1) - Nx] 
# qx: Death-at-age probability (probability of death by age) [dx / N(x-1)]
# lx: Survivors-at-age percent (how many survivors remain in each age) [Nx / N0]
# ex: Life expentancy at age (average years of life in each age) [∑ly/lx]

########################### Real stranded data #################################

lifeTab <- data.frame(age=MortAge[,"age"], ageCl=MortAge[,"age"]+1, M=MortAge[,"n"]) # mortality table
lifeTab <- rbind(c(NA, 0, 0), lifeTab) 
n <- 1000
# S - Creating survivorship column
d <- sum(lifeTab$M) # sum of deaths
for (i in 1:nrow(lifeTab)){
  lifeTab$S[[i]] <- d-sum(lifeTab$M[1:i])  
} 
# Nx - Standardizing survivors
lifeTab$Nx <- round((lifeTab$S/d)*n,1) 
# dx - Dolphins death-at-age N(x-1) - Nx
#dx<-c(n-lifeTab$Nx[1])
#for (i in 2:nrow(lifeTab)) {
#  d <- round(lifeTab$Nx[i-1]-lifeTab$Nx[i],3)
#  dx <- c(dx,d)
#}
#lifeTab$dx <- dx 
lifeTab$dx <- round((lifeTab$M/d)*n,1) # shorter alternative (we know the number of deaths by age)
# qx - Death-at-age probability dx / N(x-1)
qx <- c(lifeTab$dx[1]/n)
for (i in 2:nrow(lifeTab)) {
  q <- round(lifeTab$dx[i]/lifeTab$Nx[i-1],3)
  qx <- c(qx,q)
}
lifeTab$qx <- qx 
# lx - Survivorship-at-age percent Nx / N0
lifeTab$lx <- lifeTab$Nx/n 
# ex - Life expentancy at age ex = ∑ly/lx
ex<-vector("numeric")
for (i in 1:nrow(lifeTab)) {
  e <- round(sum(lifeTab$lx[i:nrow(lifeTab)])/lifeTab$lx[i],3)
  ex <- c(ex,e)
}
lifeTab$ex <- ex 

#lifeTab
# save data
save(file="../../RObjects/lifeTables/lifeTab.RData", lifeTab)

#mean(lifeTab$qx)*100 # average mortality (%)
mlm <- nls (lx ~ I(exp(1)^(a + b * ageCl)), data=lifeTab, start = list(a = 0, b = 1)); summary(mlm)
Z50 <- data.frame(ageCl=seq(max(lifeTab$ageCl), min(lifeTab$ageCl), length.out=300))
Z50$predLen <- predict(mlm, Z50)
Z50$predLen <- round(Z50$predLen,2)
a <- Z50[Z50$predLen==0.5,"ageCl"]
a <- round(mean(a),2)
# Z at which half the population is dead
-log(0.5)/a


## Z (Natural mortality + Fishing mortality) ##

Z <- round(-log(lifeTab$lx)/(lifeTab$age+0.5),3)
mean(Z[-c(1, nrow(lifeTab))]) # Z (total mortality - annual average)
Mort <- data.frame(age=lifeTab$age,Z)[-1,]
Mort$Z[nrow(Mort)] <- round(max(Mort$Z[(nrow(Mort)-3):nrow(Mort)-1]),3)

gM <- ggplot(Mort,aes(age,Z)) + geom_point()  + ylim(0, 0.4) +
  ylab("Mortality-at-age") + xlab("Years") + 
  ggtitle("DDE Z-Mortality")
print(gM) 

#png("../../plots/dde_Mort.png")
#print(gM)
#dev.off()

Mort$M <- Mort$Z * 0.6
Mort$F <- Mort$Z * 0.4
Mort

## Fitting survirvorship curve ##

Surv <- nls(Nx ~ I(exp(1)^(a + b * ageCl)), data = lifeTab, start = list(a = 0, b = 1))
# Coefficients
summary(Surv)
a <- round(summary(Surv)$coefficients[1, 1], 4); a
b <- round(summary(Surv)$coefficients[2, 1], 4); b

gS <- ggplot(lifeTab,aes(ageCl,Nx)) + geom_point()  + ylim(0, 1000) +
  ylab("Number of dolphins alive") + xlab("Years") + 
  ggtitle("DDE Survirvorship curve")
pframeA <- data.frame(ageCl=seq(0,30,length.out=300)) 
pframeA$Nx <- predict(Surv,newdata=pframeA) 
gSFix <- gS + geom_line(data=pframeA, colour="red") + 
  geom_text(data = NULL, x = 0, y = 0, label = paste("y=e^(",a,"+(",b,")*x)",sep=""), 
            aes(family=c("mono")), hjust=0)
print(gSFix)

#png("../../plots/dde_Surv.png")
#print(gSFix)
#dev.off()

# Log survivorship curve
gSLog <- ggplot(lifeTab,aes(ageCl,log(Nx))) + geom_point() + 
  ylab("Survivorship log(x)") + xlab("Years") + 
  ggtitle("DDE Log-Survirvorship curve")
print(gSLog)

################## Younger correction #######################

lifeTabYgr <- data.frame(age=MortAge[,"age"], ageCl=MortAge[,"age"]+1, M=MortAge[,"n"]) # mortality table
# Solving lack data in younger (multiplying first ages by 5, 3 and 1.5)
lifeTabYgr$M[c(1,2,3)] <- lifeTabYgr$M[c(1,2,3)] * c(5, 3, 1.5) 
# Adding first ageClass
lifeTabYgr <- rbind(c(NA, 0, 0), lifeTabYgr) 
n <- 1000 # initial population
# S - Creating survivorship column
d <- sum(lifeTabYgr$M) # sum of deaths
for (i in 1:nrow(lifeTabYgr)){
  lifeTabYgr$S[[i]] <- d-sum(lifeTabYgr$M[1:i])  
} 
# Nx - Standardizing survivors
lifeTabYgr$Nx <- round((lifeTabYgr$S/d)*n,1) 
# dx - Dolphins death-at-age N(x-1) - Nx
#dx<-c(n-lifeTabYgr$Nx[1])
#for (i in 2:nrow(lifeTabYgr)) {
#  d <- round(lifeTabYgr$Nx[i-1]-lifeTabYgr$Nx[i],3)
#  dx <- c(dx,d)
#}
#lifeTabYgr$dx <- dx 
lifeTabYgr$dx <- round((lifeTabYgr$M/d)*n,1) # shorter alternative (we know the number of deaths by age)
# qx - Death-at-age probability dx / N(x-1)
qx <- c(lifeTabYgr$dx[1]/n)
for (i in 2:nrow(lifeTabYgr)) {
  q <- round(lifeTabYgr$dx[i]/lifeTabYgr$Nx[i-1],3)
  qx <- c(qx,q)
}
lifeTabYgr$qx <- qx 
# lx - Survivorship-at-age percent Nx / N0
lifeTabYgr$lx <- lifeTabYgr$Nx/n 
# ex - Life expentancy at age ex = ∑ly/lx
ex<-vector("numeric")
for (i in 1:nrow(lifeTabYgr)) {
  e <- round(sum(lifeTabYgr$lx[i:nrow(lifeTabYgr)])/lifeTabYgr$lx[i],3)
  ex <- c(ex,e)
}
lifeTabYgr$ex <- ex 

#lifeTabYgr
# save data
save(file="../../RObjects/lifeTables/lifeTabYgr.RData", lifeTabYgr)

#mean(lifeTabYgr$qx)*100 # average mortality (%)
mlm <- nls (lx ~ I(exp(1)^(a + b * ageCl)), data=lifeTabYgr, start = list(a = 0, b = 1)); summary(mlm)
Z50 <- data.frame(ageCl=seq(max(lifeTabYgr$ageCl), min(lifeTabYgr$ageCl), length.out=300))
Z50$predLen <- predict(mlm, Z50)
Z50$predLen <- round(Z50$predLen,2)
a <- Z50[Z50$predLen==0.5,"ageCl"]
a <- round(mean(a),2)
# Z at which half the population is dead
-log(0.5)/a

## Z (Natural mortality + Fishing mortality) ##
Z <- round(-log(lifeTabYgr$lx)/(lifeTabYgr$age+0.5),3)
mean(Z[-c(1, nrow(lifeTabYgr))]) # Z (total mortality - annual average)
MortYngr <- data.frame(age=lifeTabYgr$age,Z)[-1,]
MortYngr$Z[nrow(MortYngr)] <- round(max(MortYngr$Z[(nrow(MortYngr)-3):nrow(MortYngr)-1]),3)
# Total Mortality (Z)
round(MortYngr$Z,2)

gM <- ggplot(MortYngr,aes(age,Z)) + geom_point()  + ylim(0, 0.4) +
  ylab("Mortality-at-age") + xlab("Years") + 
  ggtitle("DDE Z-Mortality (younger correction)")
print(gM) 

#png("../../plots/dde_MortYngr.png")
#print(gM)
#dev.off()

MortYngr$M <- MortYngr$Z * 0.6
MortYngr$F <- MortYngr$Z * 0.4
MortYngr

## Fitting survirvorship curve ##

SurvYgr <- nls(Nx ~ I(exp(1)^(a + b * ageCl)), data = lifeTabYgr, start = list(a = 0, b = 1))
# Coefficients
summary(SurvYgr)
a <- round(summary(SurvYgr)$coefficients[1, 1], 4); a
b <- round(summary(SurvYgr)$coefficients[2, 1], 4); b

gSYgr <- ggplot(lifeTabYgr,aes(ageCl,Nx)) + geom_point() + ylim(0, 1000) +
  ylab("Number of dolphins alive") + xlab("Years") + 
  ggtitle("DDE Survirvorship curve (younger correction)")
pframeA <- data.frame(ageCl=seq(0,30,length.out=300)) 
pframeA$Nx <- predict(SurvYgr,newdata=pframeA) 
gSYgrFix <- gSYgr + geom_line(data=pframeA, colour="red") + 
  geom_text(data = NULL, x = 0, y = 0, label = paste("y=e^(",a,"+(",b,")*x)",sep=""), 
            aes(family=c("mono")), hjust=0)
print(gSYgrFix)

#png("../../plots/dde_SurvYgr.png")
#print(gSYgrFix)
#dev.off()

# Log survivorship curve
gSYgrLog <- ggplot(lifeTabYgr,aes(ageCl,log(Nx))) + geom_point() + 
  ylab("Survivorship log(x)") + xlab("Years") + 
  ggtitle("DDE Log-Survirvorship curve (younger correction)")
print(gSYgrLog)

####################################################################################################
############################## Predicted initial population ########################################
####################################################################################################

################################### Exponential model ##############################################

load("../../RObjects/popStr/popStr.RData")
lifeTabPred <- data.frame(age=strPred[,"age"], ageCl=strPred[,"age"]+1, M=strPred[,"MPred"]) # mortality table

# Adding first ageClass
lifeTabPred <- rbind(c(NA, 0, 0), lifeTabPred) 
n <- 1000 # initial population
# S - Creating survivorship column
d <- sum(lifeTabPred$M) # sum of deaths
for (i in 1:nrow(lifeTabPred)){
  lifeTabPred$S[[i]] <- d-sum(lifeTabPred$M[1:i])  
} 
# Nx - Standardizing survivors
lifeTabPred$Nx <- round((lifeTabPred$S/d)*n,1) 
# dx - Dolphins death-at-age N(x-1) - Nx
#dx<-c(n-lifeTabPred$Nx[1])
#for (i in 2:nrow(lifeTabPred)) {
#  d <- round(lifeTabPred$Nx[i-1]-lifeTabPred$Nx[i],3)
#  dx <- c(dx,d)
#}
#lifeTabPred$dx <- dx 
lifeTabPred$dx <- round((lifeTabPred$M/d)*n,1) # shorter alternative (we know the number of deaths by age)
# qx - Death-at-age probability dx / N(x-1)
qx <- c(lifeTabPred$dx[1]/n)
for (i in 2:nrow(lifeTabPred)) {
  q <- round(lifeTabPred$dx[i]/lifeTabPred$Nx[i-1],3)
  qx <- c(qx,q)
}
lifeTabPred$qx <- qx 
# lx - Survivorship-at-age percent Nx / N0
lifeTabPred$lx <- lifeTabPred$Nx/n 
# ex - Life expentancy at age ex = ∑ly/lx
ex<-vector("numeric")
for (i in 1:nrow(lifeTabPred)) {
  e <- round(sum(lifeTabPred$lx[i:nrow(lifeTabPred)])/lifeTabPred$lx[i],3)
  ex <- c(ex,e)
}
lifeTabPred$ex <- ex 

#lifeTabPred
# save data
save(file="../RObjects/lifeTabPred.RData", lifeTabPred)

#mean(lifeTabPred$qx)*100 # average mortality (%)
mlm <- nls (lx ~ I(exp(1)^(a + b * ageCl)), data=lifeTabPred, start = list(a = 0, b = 1)); summary(mlm)
Z50 <- data.frame(ageCl=seq(max(lifeTabPred$ageCl), min(lifeTabPred$ageCl), length.out=300))
Z50$predLen <- predict(mlm, Z50)
Z50$predLen <- round(Z50$predLen,2)
a <- Z50[Z50$predLen==0.5,"ageCl"]
a <- round(mean(a),2)
# Z at which half the population is dead
-log(0.5)/a

## Z (Natural mortality + Fishing mortality) ##
Z <- round(-log(lifeTabPred$lx)/(lifeTabPred$age+0.5),3)
mean(Z[-c(1, nrow(lifeTabPred))]) # Z (total mortality - annual average)
MortPred <- data.frame(age=lifeTabPred$age,Z)[-1,]
MortPred$Z[nrow(MortPred)] <- round(MortPred$Z[nrow(MortPred)-1] + 
                                      (MortPred$Z[nrow(MortPred)-1] - MortPred$Z[nrow(MortPred)-2]),3)
#MortPred$Z[nrow(MortPred)] <- round(max(MortPred$Z[(nrow(MortPred)-3):nrow(MortPred)-1]),3)

# Total Mortality (Z)
round(MortPred$Z,2)

# Plot
gM <- ggplot(MortPred,aes(age,Z)) + geom_point()  + ylim(0, 0.4) +
  ylab("Mortality-at-age") + xlab("Years") + 
  ggtitle("DDE Z-Mortality (exponential prdiction)")
print(gM) 
#png("../../plots/dde_MortPred.png")
#print(gM)
#dev.off()

MortPred$M <- MortPred$Z * 0.6
MortPred$F <- MortPred$Z * 0.4
MortPred

## Fitting survirvorship curve ##

SurvPred <- nls(Nx ~ I(exp(1)^(a + b * ageCl)), data = lifeTabPred, start = list(a = 0, b = 1))
# Coefficients
summary(SurvPred)
a <- round(summary(SurvPred)$coefficients[1, 1], 4); a
b <- round(summary(SurvPred)$coefficients[2, 1], 4); b

gSPred <- ggplot(lifeTabPred,aes(ageCl,Nx)) + geom_point() + ylim(0, 1000) +
  ylab("Number of dolphins alive") + xlab("Years") + 
  ggtitle("DDE Survirvorship curve (exponential prdiction)")
pframeA <- data.frame(ageCl=seq(0,30,length.out=300)) 
pframeA$Nx <- predict(SurvPred,newdata=pframeA) 
gSPredFix <- gSPred + geom_line(data=pframeA, colour="red") + 
  geom_text(data = NULL, x = 0, y = 0, label = paste("y=e^(",a,"+(",b,")*x)",sep=""), 
            aes(family=c("mono")), hjust=0)
print(gSPredFix)

#png("../../plots/dde_SurvPred.png")
#print(gSPredFix)
#dev.off()

# Log survivorship curve
gSPredLog <- ggplot(lifeTabPred,aes(ageCl,log(Nx))) + geom_point() + 
  ylab("Survivorship log(x)") + xlab("Years") + 
  ggtitle("DDE Log-Survirvorship curve (exponential prdiction)")
print(gSPredLog)

############## Predicted initial population (second-degree polynomial model) #######################
########################## ERROR: NEGATIVE VALUES ##################################################

load("../RObjects/strPred2.RData")
lifeTabPred2 <- data.frame(age=strPred2[,"age"], ageCl=strPred2[,"age"]+1, M=strPred2[,"MPred"]) # mortality table
lifeTabPred2[(nrow(lifeTabPred2)-3) : nrow(lifeTabPred2),3] <- 1 # Remove negative and zero values

# Adding first ageClass
lifeTabPred2 <- rbind(c(NA, 0, 0), lifeTabPred2) 
n <- 1000 # initial population
# S - Creating survivorship column
d <- sum(lifeTabPred2$M) # sum of deaths
for (i in 1:nrow(lifeTabPred2)){
  lifeTabPred2$S[[i]] <- d-sum(lifeTabPred2$M[1:i])  
} 
# Nx - Standardizing survivors
lifeTabPred2$Nx <- round((lifeTabPred2$S/d)*n,1) 
# dx - Dolphins death-at-age N(x-1) - Nx
#dx<-c(n-lifeTabPred2$Nx[1])
#for (i in 2:nrow(lifeTabPred2)) {
#  d <- round(lifeTabPred2$Nx[i-1]-lifeTabPred2$Nx[i],3)
#  dx <- c(dx,d)
#}
#lifeTabPred2$dx <- dx 
lifeTabPred2$dx <- round((lifeTabPred2$M/d)*n,1) # shorter alternative (we know the number of deaths by age)
# qx - Death-at-age probability dx / N(x-1)
qx <- c(lifeTabPred2$dx[1]/n)
for (i in 2:nrow(lifeTabPred2)) {
  q <- round(lifeTabPred2$dx[i]/lifeTabPred2$Nx[i-1],3)
  qx <- c(qx,q)
}
lifeTabPred2$qx <- qx 
# lx - Survivorship-at-age percent Nx / N0
lifeTabPred2$lx <- lifeTabPred2$Nx/n 
# ex - Life expentancy at age ex = ∑ly/lx
ex<-vector("numeric")
for (i in 1:nrow(lifeTabPred2)) {
  e <- round(sum(lifeTabPred2$lx[i:nrow(lifeTabPred2)])/lifeTabPred2$lx[i],3)
  ex <- c(ex,e)
}
lifeTabPred2$ex <- ex 

#lifeTabPred2
# save data
save(file="../RObjects/lifeTabPred2.RData", lifeTabPred2)

#mean(lifeTabPred2$qx)*100 # average mortality (%)
#mlm <- nls (lx ~ I(exp(1)^(a + b * ageCl)), data=lifeTabPred2, start = list(a = 0, b = 1)); summary(mlm)
mlm <- nls(lx~b0+b1*ageCl+b2*ageCl^2+b3*ageCl^3,data=lifeTabPred2, # third-degree polynomial
           start=list(b0=0,b1=1,b2=1, b3=1)); summary(mlm)
Z50 <- data.frame(ageCl=seq(max(lifeTabPred2$ageCl), min(lifeTabPred2$ageCl), length.out=300))
Z50$Pred2Len <- predict(mlm, Z50)
Z50$Pred2Len <- round(Z50$Pred2Len,2)
a <- Z50[Z50$Pred2Len==0.5,"ageCl"]
a <- round(mean(a),2)
# Z at which half the population is dead
-log(0.5)/a

## Z (Natural mortality + Fishing mortality) ##
Z <- round(-log(lifeTabPred2$lx)/(lifeTabPred2$age+0.5),3)
mean(Z[-c(1, nrow(lifeTabPred2))]) # Z (total mortality - annual average)
MortPred2 <- data.frame(age=lifeTabPred2$age,Z)[-1,]
MortPred2$Z[nrow(MortPred2)] <- round(max(MortPred2$Z[(nrow(MortPred2)-3):nrow(MortPred2)-1]),3)
# Total Mortality (Z)
round(MortPred2$Z,2)

gM <- ggplot(MortPred2,aes(age,Z)) + geom_point()  + ylim(0, 0.4) +
  ylab("Mortality-at-age") + xlab("Years") + 
  ggtitle("DDE Z-Mortality (second-degree polynomial)")
print(gM) 

#png("../../plots/dde_MortPred2.png")
#print(gM)
#dev.off()

MortPred2$M <- MortPred2$Z * 0.6
MortPred2$F <- MortPred2$Z * 0.4
MortPred2

## Fitting survirvorship curve ##

SurvPred2 <- nls(Nx ~ I(exp(1)^(a + b * ageCl)), data = lifeTabPred2, start = list(a = 0, b = 1))

# Coefficients
summary(SurvPred2)
a <- round(summary(SurvPred2)$coefficients[1, 1], 4); a
b <- round(summary(SurvPred2)$coefficients[2, 1], 4); b

gSPred2 <- ggplot(lifeTabPred2,aes(ageCl,Nx)) + geom_point() + ylim(0, 1000) +
  ylab("Number of dolphins alive") + xlab("Years") + 
  ggtitle("DDE Survirvorship curve (second-degree polynomial)")
pframeA <- data.frame(ageCl=seq(0,30,length.out=300)) 
pframeA$Nx <- predict(SurvPred2,newdata=pframeA) 
gSPred2Fix <- gSPred2 + geom_line(data=pframeA, colour="red") + 
  geom_text(data = NULL, x = 0, y = 0, label = paste("y=e^(",a,"+(",b,")*x)",sep=""), 
            aes(family=c("mono")), hjust=0)
print(gSPred2Fix)

#png("../../plots/dde_SurvPred2.png")
#print(gSPred2Fix)
#dev.off()

# Log survivorship curve
gSPred2Log <- ggplot(lifeTabPred2,aes(ageCl,log(Nx))) + geom_point() + 
  ylab("Survivorship log(x)") + xlab("Years") + 
  ggtitle("DDE Log-Survirvorship curve (second-degree polynomial)")
print(gSPred2Log)

############## Predicted initial population (third-degree polynomial model) #######################

load("../RObjects/strPred3.RData")
lifeTabPred3 <- data.frame(age=strPred3[,"age"], ageCl=strPred3[,"age"]+1, M=strPred3[,"MPred"]) # mortality table

# Adding first ageClass
lifeTabPred3 <- rbind(c(NA, 0, 0), lifeTabPred3) 
n <- 1000 # initial population
# S - Creating survivorship column
d <- sum(lifeTabPred3$M) # sum of deaths
for (i in 1:nrow(lifeTabPred3)){
  lifeTabPred3$S[[i]] <- d-sum(lifeTabPred3$M[1:i])  
} 
# Nx - Standardizing survivors
lifeTabPred3$Nx <- round((lifeTabPred3$S/d)*n,1) 
# dx - Dolphins death-at-age N(x-1) - Nx
#dx<-c(n-lifeTabPred3$Nx[1])
#for (i in 2:nrow(lifeTabPred3)) {
#  d <- round(lifeTabPred3$Nx[i-1]-lifeTabPred3$Nx[i],3)
#  dx <- c(dx,d)
#}
#lifeTabPred3$dx <- dx 
lifeTabPred3$dx <- round((lifeTabPred3$M/d)*n,1) # shorter alternative (we know the number of deaths by age)
# qx - Death-at-age probability dx / N(x-1)
qx <- c(lifeTabPred3$dx[1]/n)
for (i in 2:nrow(lifeTabPred3)) {
  q <- round(lifeTabPred3$dx[i]/lifeTabPred3$Nx[i-1],3)
  qx <- c(qx,q)
}
lifeTabPred3$qx <- qx 
# lx - Survivorship-at-age percent Nx / N0
lifeTabPred3$lx <- lifeTabPred3$Nx/n 
# ex - Life expentancy at age ex = ∑ly/lx
ex<-vector("numeric")
for (i in 1:nrow(lifeTabPred3)) {
  e <- round(sum(lifeTabPred3$lx[i:nrow(lifeTabPred3)])/lifeTabPred3$lx[i],3)
  ex <- c(ex,e)
}
lifeTabPred3$ex <- ex 

#lifeTabPred3
# save data
save(file="../Robjects/lifeTabPred3.RData", lifeTabPred3)

#mean(lifeTabPred3$qx)*100 # average mortality (%)
#mlm <- nls (lx ~ I(exp(1)^(a + b * ageCl)), data=lifeTabPred3, start = list(a = 0, b = 1)); summary(mlm)
mlm <- nls(lx~b0+b1*ageCl+b2*ageCl^2+b3*ageCl^3,data=lifeTabPred3, # third-degree polynomial
start=list(b0=0,b1=1,b2=1, b3=1)); summary(mlm)
Z50 <- data.frame(ageCl=seq(max(lifeTabPred3$ageCl), min(lifeTabPred3$ageCl), length.out=300))
Z50$Pred3Len <- predict(mlm, Z50)
Z50$Pred3Len <- round(Z50$Pred3Len,2)
a <- Z50[Z50$Pred3Len==0.5,"ageCl"]
a <- round(mean(a),2)
# Z at which half the population is dead
-log(0.5)/a

## Z (Natural mortality + Fishing mortality) ##
Z <- round(-log(lifeTabPred3$lx)/(lifeTabPred3$age+0.5),3)
mean(Z[-c(1, nrow(lifeTabPred3))]) # Z (total mortality - annual average)
MortPred3 <- data.frame(age=lifeTabPred3$age,Z)[-1,]
MortPred3$Z[nrow(MortPred3)] <- round(max(MortPred3$Z[(nrow(MortPred3)-3):nrow(MortPred3)-1]),3)
# Total Mortality (Z)
round(MortPred3$Z,2)

gM <- ggplot(MortPred3,aes(age,Z)) + geom_point()  + ylim(0, 0.4) +
  ylab("Mortality-at-age") + xlab("Years") + 
  ggtitle("DDE Z-Mortality (third-degree polynomial)")
print(gM) 

#png("../../plots/dde_MortPred3.png")
#print(gM)
#dev.off()

MortPred3$M <- MortPred3$Z * 0.6
MortPred3$F <- MortPred3$Z * 0.4
MortPred3


## Fitting survirvorship curve ##

SurvPred3 <- nls(Nx ~ I(exp(1)^(a + b * ageCl)), data = lifeTabPred3, start = list(a = 0, b = 1))
# Coefficients
summary(SurvPred3)
a <- round(summary(SurvPred3)$coefficients[1, 1], 4); a
b <- round(summary(SurvPred3)$coefficients[2, 1], 4); b

gSPred3 <- ggplot(lifeTabPred3,aes(ageCl,Nx)) + geom_point() + ylim(0, 1000) +
  ylab("Number of dolphins alive") + xlab("Years") + 
  ggtitle("DDE Survirvorship curve (third-degree polynomial)")
pframeA <- data.frame(ageCl=seq(0,30,length.out=300)) 
pframeA$Nx <- predict(SurvPred3,newdata=pframeA) 
gSPred3Fix <- gSPred3 + geom_line(data=pframeA, colour="red") + 
  geom_text(data = NULL, x = 0, y = 0, label = paste("y=e^(",a,"+(",b,")*x)",sep=""), 
            aes(family=c("mono")), hjust=0)
print(gSPred3Fix)

#png("../../plots/dde_SurvPred3.png")
#print(gSPred3Fix)
#dev.off()

# Log survivorship curve
gSPred3Log <- ggplot(lifeTabPred3,aes(ageCl,log(Nx))) + geom_point() + 
  ylab("Survivorship log(x)") + xlab("Years") + 
  ggtitle("DDE Log-Survirvorship curve (third-degree polynomial)")
print(gSPred3Log)



#######################################################################################
############################### M approaches comparison ############################### 
#######################################################################################

tmax <- 30

# Life table
apply(Mort[,-1],2,mean)
#lifeTab
-log(0.5)/4.8
apply(MortYngr[,-1],2,mean)
#lifeTabYgr
-log(0.5)/4.5

## Hewit, D.A. & Hoenig, J.M. 2005

# Hoening (1983) 
H <- exp(1.44-0.982*log(tmax)); H
# aprox
Haprox <- 4.22/tmax; Haprox

# rule-of-thumb M=-ln(P)/tmax
rt <- -log(1/tmax)/tmax; rt #¿?
# FAO aprox (Sparre & Venema, 1998; Cadima, 2003)
rtaprox <- 3/tmax; rtaprox

# Fiona's value of 13% deaths-per-year
s <- 1-0.13
Fi <- -log(s); Fi
  
# Cubilos, L.A. (2003)  t*= to+(1/K)ln((3K/M)+1)
# puesto que M=3K/(exp((Kt*)-1))  y   t*=-(1/K)ln(1-w)  ya que w=1-exp(-Kt*)  entonces  M=3K(1-w)/w 
# OJO, NO funciona: Se calcula directamente de la k de von Bertalanffy si asumimos w como 0.62 pero si la K está mal estimada la M también
load("../../../DDE Growth/data/vbAll.RData")
#linf <- summary(vbAll)$parameters[1]
K <- summary(vbAll)$parameters[2]
to <- summary(vbAll)$parameters[3]
w <- 0.62 # se podría calcular un w específico para cetáceos conociendo Linf y L*
# with "to"
(3*K*exp(to)*(1-w))/(1-exp(to)+w*exp(to))
# without "to"
(3*K*(1-w))/w
1.839*K

