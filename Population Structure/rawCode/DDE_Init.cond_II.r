####################################################################################################
#                        POPULATION STRUCTURE - COMMON DOLPHIN 
#             Analisys of stranded-at-age, initial conditions and recruitment
#                  C.Saavedra (camilo.saavedra@vi.ieo.es) 24/09/2013
####################################################################################################

# IMPORTANT: Set working directory (to source file location)

## Charge libraries ##
library(ggplot2)

## Read data ##
CEMMA <- read.csv("../../data/RData/CEMMA.csv")
ddePop <- CEMMA[CEMMA$sp == "DDE",c("length.real", "length", "sex", "age")]
# Changing age values (0.5 and 1.5 for 0 and 1)
ddePop$age[ddePop$age == 0.5] <- 0
ddePop$age[ddePop$age == 1.5] <- 1

####################################################################################

## sex ratio ##

nMale <- nrow(ddePop[ddePop$sex==1,])
nFemale <- nrow(ddePop[ddePop$sex==2,])
Male <- 1
Female <- nFemale/nMale
sxr <- data.frame(Male=Male,Female=round(Female,1))
rownames(sxr) <- "sex-ratio" ; sxr

## Strandings by age ##

ageDistrTot <- tapply(ddePop$age, list(ddePop$age), length) # Total
#ageDistrS <- tapply(ddePop$age, list(ddePop$age, ddePop$sex), length) # By sex 
# Transform and merge in a data frame
#ageStrnd <- data.frame (age = as.numeric(rownames(ageDistr)), M=ageDistrS[,1],
#                         F=ageDistrS[,2], nTot=ageDistrTot) # By sex
ageStrnd <- data.frame (age = as.numeric(rownames(ageDistr)), nTot=ageDistrTot)
ages <- data.frame(age=1:30)
ageStrnd <- merge(ages,ageStrnd, all=TRUE) # Merge with a vector with all missing ages
# Removing NA
ageStrnd[is.na(ageStrnd)] <- 0

# Plot
gStrnd <- ggplot(ageStrnd,aes(age,nTot)) + geom_point()
gStrndSm <- gStrnd + geom_smooth(method="loess") + # methods= lm, glm, gam, loess, rlm.
  ylab("Number of stranded dolphins") + xlab("Age of stranded dolphins") + 
  ggtitle("Frequency of stranded dolphins by age")
gStrndSm

png("../../plots/AgeStranded.png", width=600, height=480)
print(gStrndSm)
dev.off()

# Solving lack data in first ages (multiplying by 5, 3 and 1.5)
ageStrndSlv <- ageStrnd
ageStrndSlv[c(1,2,3),-1] <- round(ageStrnd[c(1,2,3),-1] * c(5,3,1.5),0)

gStrndSlv <- ggplot(ageStrndSlv,aes(age,nTot)) + geom_point()
gStrndSmSlv <- gStrndSlv + geom_smooth(method="loess") + # methods= lm, glm, gam, loess, rlm.
  ylab("Number of stranded dolphins (younger correction)") + xlab("Age of stranded dolphins") + 
  ggtitle("Frequency of stranded dolphins by age")# Plot
gStrndSmSlv

png("../../plots/AgeStranded(Correction).png", width=600, height=480)
print(gStrndSmSlv)
dev.off()

#################################################################################################

## Plotting strandings both by length and by age ##

Str <- ddePop[, c("age", "length")]
Str <- Str[complete.cases(Str),]

## Strandings by length ##

Str$rg <- cut(Str$length,breaks=seq(90,240,by=10)) # Creating ranges
LenStr <- tapply(Str$rg, list(Str$rg), length) # Count strandings by length range
LenStr <- data.frame(rg=names(LenStr), n=LenStr[]) # Create a data frame with ranges and counts
# Ordering ranges for plotting
LenStr$rg <- factor(LenStr$rg, 
                       levels=LenStr$rg[!duplicated(LenStr$rg)], ordered = T)

# Plot
gLenStr <- ggplot(LenStr,aes(rg,n)) + geom_point()
gLenStrSm <- gLenStr + geom_smooth(aes(group = 1), method="loess") + # methods= lm, glm, gam, loess, rlm.
  ylab("Number of stranded dolphins") + xlab("Length of stranded dolphins") + 
  ggtitle("Stranded dolphins by length (younger correction)")# Plot
gLenStrSm

png("../../plots/LenStr.png", width=600, height=480)
print(gStrLenSlvSm)
dev.off()


## Strandings by age ##

AgeStr <- tapply(Str$age, list(Str$age), length) # Total
AgeStr <- data.frame(age=as.numeric(names(AgeStr)), n=AgeStr[])
# Adding years who doesn't exist
#ages <- data.frame(age=1:30)
#AgeStr <- merge(ages,AgeStr, all=TRUE) # Merge with a vector with all missing ages
#AgeStr[is.na(AgeStr)] <- 0

# Plot
gAgeStr <- ggplot(AgeStr,aes(age,n)) + geom_point()
gAgeStrSm <- gAgeStr + geom_smooth(method="loess") + # methods= lm, glm, gam, loess, rlm.
  ylab("Number of stranded dolphins ") + xlab("Age of stranded dolphins") + 
  ggtitle("Stranded dolphins by age (younger correction)")# Plot
gAgeStrSm

png("../../plots/AgeStr.png", width=600, height=480)
print(gAgeStrSm)
dev.off()


## Younger correction ##

# Solving lack data in younger (multiplying first ages by 5, 3 and 1.5)
n <- c(0,1,2) # ages 0, 1, 2  
Age0 <- Str[Str$age == n[1],]
Age1 <- Str[Str$age == n[2],]
Age2 <- Str[Str$age == n[3],]
mult <- c(5, 3, 1.2) - 1 # subtract one to don't repeat same data when merge in the original
Age0 <- Age0[sample(nrow(Age0), nrow(Age0)*mult[1], replace=TRUE), ]
Age1 <- Age1[sample(nrow(Age1), nrow(Age1)*mult[2], replace=TRUE), ]
Age2 <- Age2[sample(nrow(Age2), nrow(Age2)*mult[3], replace=TRUE), ]

StrYgr <- rbind(Age0, Age1, Age2, Str) # Merge all data
StrYgr <- StrYgr[order(StrYgr$age),] # Order by age


## Strandings by length ##

StrYgr$rg <- cut(StrYgr$length,breaks=seq(90,240,by=10)) # Creating ranges
LenStrYgr <- tapply(StrYgr$rg, list(StrYgr$rg), length) # Count strandings by length range
LenStrYgr <- data.frame(rg=names(LenStrYgr), n=LenStrYgr[]) # Create a data frame with ranges and counts
# Ordering ranges for plotting
LenStrYgr$rg <- factor(LenStrYgr$rg, 
  levels=LenStrYgr$rg[!duplicated(LenStrYgr$rg)], ordered = T)

# Plot
gLenStrYgr <- ggplot(LenStrYgr,aes(rg,n)) + geom_point()
gLenStrYgrSm <- gLenStrYgr + geom_smooth(aes(group = 1), method="loess") + # methods= lm, glm, gam, loess, rlm.
  ylab("Number of stranded dolphins") + xlab("Length of stranded dolphins") + 
  ggtitle("Stranded dolphins by length (younger correction)")# Plot
gLenStrYgrSm

png("../../plots/LenStr(younger-correction).png", width=600, height=480)
print(gStrLenSlvSm)
dev.off()


## Strandings by age ##

AgeStrYgr <- tapply(StrYgr$age, list(StrYgr$age), length) # Total
AgeStrYgr <- data.frame(age=as.numeric(names(AgeStrYgr)), n=AgeStrYgr[])
# Adding years who doesn't exist
#ages <- data.frame(age=1:30)
#AgeStrYgr <- merge(ages,AgeStrYgr, all=TRUE) # Merge with a vector with all missing ages
#AgeStrYgr[is.na(AgeStrYgr)] <- 0

# Plot
gAgeStrYgr <- ggplot(AgeStrYgr,aes(age,n)) + geom_point()
gAgeStrYgrSm <- gAgeStrYgr + geom_smooth(method="loess") + # methods= lm, glm, gam, loess, rlm.
  ylab("Number of stranded dolphins ") + xlab("Age of stranded dolphins") + 
  ggtitle("Stranded dolphins by age (younger correction)")# Plot
gAgeStrYgrSm

png("../../plots/AgeStr(younger-correction).png", width=600, height=480)
print(gAgeStrYgrSm)
dev.off()














## Initial conditions ##

# percentage of each age class
ageStrndSlv$perc <- round(ageStrndSlv$nTot/sum(ageStrndSlv$nTot), 3)

# 15000 dolphins population (aprox 4/5 of the SCANS estimations for the W area)
nPop <- 15000
ageStrndSlv$pop <- ageStrndSlv$perc*nPop
# Calculating weight by predicted length for each age class





ageDistr <- merge(pred, ageDistr, by="t_age", all.x=TRUE)
ageDistr$w <- 10^(-4.68088+(2.88534*log10(ageDistr$Lt_est)))  # W=10^(-4.68088+2.88534*LOG10(C6))
ageDistr$wTot <- round(ageDistr$w * ageDistr$pop,0) # kg of dolphins by age



# Data for params file, initial condition (age)
ageDistr$init <- round(ageDistr$pop / 10000,3)  #Porque las codiciones iniciales están expresadas en diecmiles
ageDistr$age <- paste("age", seq(1:30), sep="")
ageDistr[,c("age","n")] # Strandings frequency 
ageDistr[,c("age","init")] # kg per age ## INITIAL CONDITIONS ##

# Recruits per year (33% pregnancy female - if sexratio = 1:1)
calf <- nPop * 0.5 * 0.3 * 0.3 # calfs per year (dividido entre: proporción de hembras, de maduras y de embarazadas)
calf/10000
# CALCULAR REALMENTE LAS HEMBRAS MADURAS EN EL TOTAL DE LOS ANIMALES VARADOS



# Maturity 12 yr
# firs birth 13 yr
# mature females 0.290
# sex-ratio 50%
# gestation time 0.958

(0.290/0.958)*0.5 # parece que asume que paren una cria anualmente



