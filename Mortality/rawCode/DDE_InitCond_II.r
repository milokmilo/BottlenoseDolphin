####################################################################################################
#                   INITIAL CONDITIONS, STOCK WEIGHT BY AGE AND RECRUITMENT
#            
#                     C.Saavedra (camilo.saavedra@vi.ieo.es) 08/10/2013
####################################################################################################


# IMPORTANT: Set working directory (to source file location)

# Set if you want younger correction (TRUE) on not (FALSE)
# Younger correction (strandings of the first 3 ages multiplied by 5, 3 and 1.5)
YoungerCorrection <- FALSE

if (YoungerCorrection ==TRUE){
  load("../../data/lifeTabYgr.RData")
  init <- data.frame(age=lifeTabYgr$age, M=lifeTabYgr$M, perc=lifeTabYgr$M/sum(lifeTabYgr$M))[-1,]
} else {
  load("../../data/lifeTab.RData")  
  init <- data.frame(age=lifeTab$age, M=lifeTab$M, perc=lifeTab$M/sum(lifeTab$M))[-1,]
}
## Initial population (INITIAL CONDITIONS FILE)

# Starting with a 15000 dolphins population (4/5 of the SCANS-II estimation for the W area)
nPop <- 15000
init$pop <- round(init$perc*nPop,1)

# Modeling population structure
modPS <- nls(pop ~ exp(a + b * age), data = init, # exponential 
             start = list(a = 0, b = 1)); summary(modPS)
#modPS2 <- nls(pop ~ a*exp(b * age), data = init, start = list(a = 1000, b = -1)); summary(modPS2) 
mod2PS <- nls(pop~b0+b1*age+b2*age^2,data=init, # second-degree polynomial
              start=list(b0=0,b1=1,b2=1)); summary(mod2PS)
mod3PS <- nls(pop~b0+b1*age+b2*age^2+b3*age^3,data=init, # third-degree polynomial
              start=list(b0=0,b1=1,b2=1, b3=1)); summary(mod3PS)
init$popPred <- predict(modPS, new.data=init)
init$popPred2 <- predict(mod2PS, new.data=init)
init$popPred3 <- predict(mod3PS, new.data=init)
initPop <- init[,c("age","pop")]; initPop/10000 # Initial population  
initPopPred <- init[,c("age","popPred")]; initPopPred/10000 # Predicted initial population (exponential)
initPopPred2 <- init[,c("age","popPred2")]; initPopPred2/10000 # Predicted initial population (second-degree polynomial)
initPopPred3 <- init[,c("age","popPred3")]; initPopPred3/10000 # Predicted initial population (third-degree polynomial)

plot(initPop$age, initPop$pop)
lines(initPopPred$popPred, col=1) # Black: Predicted initial population (exponential)
lines(initPop$age, fitted(modPS), col=2) # Red: Predicted initial population (exponential)
lines(initPop$age, fitted(mod2PS), col=3) # Green: Predicted initial population (second-degree polynomial)
lines(initPop$age,fitted(mod3PS),col=4) # Blue: Predicted initial population (third-degree polynomial)

#--------------------------------------------------------------------------------------------------#
# Strandings of the first 3 ages if assume true the population structure model
strPred <- data.frame(age=init$age, M=init$M, MPred=round((init$popPred/nPop)*sum(init$M),0))
strPred # Predicted strandings (exponential)
save(file="../../data/strPred.RData", strPred)
strPred2 <- data.frame(age=init$age, M=init$M, MPred=round((init$popPred2/nPop)*sum(init$M),0))
strPred2 # Predicted strandings (second-degree polynomial)
save(file="../../data/strPred2.RData", strPred2)
strPred3 <- data.frame(age=init$age, M=init$M, MPred=round((init$popPred3/nPop)*sum(init$M),0))
strPred3 # Predicted strandings (third-degree polynomial)
save(file="../../data/strPred3.RData", strPred3)
#plot(strPred$age, strPred$MPred)
#strPred$MPred / init$M
#--------------------------------------------------------------------------------------------------#

## Length and weight at age (PARAMETRES FILE)

# Loading length-age growth model
#load("../../../DDE Growth/data/vbAll.RData")
#lPred <- data.frame(t_age=seq(min(init$age), max(init$age)))
#lPred$len <- predict(vbAll, newdata=lPred)
#names(lPred) <- c("age", "l")
# merge the two data frames
#init <- merge(init, lPred, by="age", all.x=TRUE)
# Weight at age
#init$w <- 10^(-4.68088+(2.88534*log10(init$l)))  # log(W)=-4.68088+2.88534*log(L)
# Total kg of dolphins at age
#init$wTot <- round((init$w * init$pop)/10000,3) # divided by 10000 because Gadget constructs populations of 10,000 fish for each age group
#initW <- init[,c("age","wTot")]; initW # Total weight of the stock by age
#init$wTotPred <- round((init$w * init$popPred)/10000,3) # divided by 10000 because Gadget constructs populations of 10,000 fish for each age group
#initWPred <- init[,c("age","wTotPred")]; initWPred # Total predicted weight of the stock by age
#init$wTotPred2 <- round((init$w * init$popPred2)/10000,3) # divided by 10000 because Gadget constructs populations of 10,000 fish for each age group
#initWPred2 <- init[,c("age","wTotPred2")]; initWPred2 # Total predicted weight of the stock by age
#init$wTotPred3 <- round((init$w * init$popPred3)/10000,3) # divided by 10000 because Gadget constructs populations of 10,000 fish for each age group
#initWPred3 <- init[,c("age","wTotPred3")]; initWPred3 # Total predicted weight of the stock by age

## Recruitment (RECRUITMENT FILE)

# Recruits per year (33% pregnancy female - if sexratio = 1:1)
rec <- init[,c("age", "pop")]
rec$popFem <- rec$pop / 2 # sex ratio 1:1
rec <- rec[rec$age >= 8,] # A50 = 8 (50% mature female)
rec[rec$age == 8,c("pop", "popFem")] <- rec[rec$age == 8,c("pop", "popFem")]/2 # dolphins of age 8 divided by 2 (only 50% mature)
matFem <- sum(rec$popFem) # total mature females
calfs <- round(matFem/3,0) # Pregnancy rate = 33%
calfs/10000 # (divided to use as imput in Gadget)

rec <- init[,c("age", "popPred")]
rec$popFem <- rec$popPred / 2 # sex ratio 1:1
rec <- rec[rec$age >= 8,] # A50 = 8 (50% mature female)
rec[rec$age == 8,c("popPred", "popFem")] <- rec[rec$age == 8,c("popPred", "popFem")]/2 # dolphins of age 8 divided by 2 (only 50% mature)
matFem <- sum(rec$popFem) # total mature females
calfs <- round(matFem/2,0) # Pregnancy rate = 33%
calfs/10000 # exponential

rec <- init[,c("age", "popPred2")]
rec$popFem <- rec$popPred2 / 2 # sex ratio 1:1
rec <- rec[rec$age >= 8,] # A50 = 8 (50% mature female)
rec[rec$age == 8,c("popPred2", "popFem")] <- rec[rec$age == 8,c("popPred2", "popFem")]/2 # dolphins of age 8 divided by 2 (only 50% mature)
matFem <- sum(rec$popFem) # total mature females
calfs <- round(matFem/3,0) # Pregnancy rate = 33%
calfs/10000 # second-degree polynomial

rec <- init[,c("age", "popPred3")]
rec$popFem <- rec$popPred3 / 2 # sex ratio 1:1
rec <- rec[rec$age >= 8,] # A50 = 8 (50% mature female)
rec[rec$age == 8,c("popPred3", "popFem")] <- rec[rec$age == 8,c("popPred3", "popFem")]/2 # dolphins of age 8 divided by 2 (only 50% mature)
matFem <- sum(rec$popFem) # total mature females
calfs <- round(matFem/3,0) # Pregnancy rate = 33%
calfs/10000 # third-degree polynomial
