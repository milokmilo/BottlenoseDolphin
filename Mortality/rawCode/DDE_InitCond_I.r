####################################################################################################
#                   INITIAL CONDITIONS, STOCK WEIGHT BY AT AND RECRUITMENT
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
# Modeling populatin structure
modPS <- nls(pop ~ exp(a + b * age), data = init, start = list(a = 0, b = 1)); summary(modPS)
#modPS2 <- nls(pop ~ a*exp(b * age), data = init, start = list(a = 1000, b = -1)); summary(modPS2) 
init$popPred <- predict(modPS, new.data=init)
initPop <- init[,c("age","pop")]; initPop # Initial population  
initPopPred <- init[,c("age","popPred")]; initPopPred # Predicted initial population

plot(initPop$age, initPop$pop)
lines(initPopPred$popPred, col="red")

#--------------------------------------------------------------------------------------------------#
# Strandings of the first 3 ages if assume true the population structure model
strPred <- data.frame(age=init$age, M=init$M, MPred=round((init$popPred/nPop)*sum(init$M),0)); strPred
save(file="../../data/strPred.RData", strPred)
#plot(strPred$age, strPred$MPred)
#strPred$MPred / init$M
#--------------------------------------------------------------------------------------------------#

## Length and weight at age (PARAMETRES FILE)

# Loading length-age growth model
load("../../../DDE Growth/data/vbAll.RData")
lPred <- data.frame(t_age=seq(min(init$age), max(init$age)))
lPred$len <- predict(vbAll, newdata=lPred)
names(lPred) <- c("age", "l")
# merge the two data frames
init <- merge(init, lPred, by="age", all.x=TRUE)
# Weight at age
init$w <- 10^(-4.68088+(2.88534*log10(init$l)))  # log(W)=-4.68088+2.88534*log(L)
# Total kg of dolphins at age
init$wTot <- round((init$w * init$pop)/10000,3) # divided by 10000 because Gadget constructs populations of 10,000 fish for each age group
initW <- init[,c("age","wTot")]; initW # Total weight of the stock by age
init$wTotPred <- round((init$w * init$popPred)/10000,3) # divided by 10000 because Gadget constructs populations of 10,000 fish for each age group
initWPred <- init[,c("age","wTotPred")]; initWPred # Total predicted weight of the stock by age

## Recruitment (RECRUITMENT FILE)

# Recruits per year (33% pregnancy female - if sexratio = 1:1)
rec <- init[,c("age", "pop")]
rec$popFem <- rec$pop / 2 # sex ratio 1:1
rec <- rec[rec$age >= 8,] # A50 = 8 (50% mature female)
rec[rec$age == 8,c("pop", "popFem")] <- rec[rec$age == 8,c("pop", "popFem")]/2 # dolphins of age 8 divided by 2 (only 50% mature)
matFem <- sum(rec$popFem) # total mature females
calfs <- round(matFem/3,0) # Pregnancy rate = 33%
calfs

rec <- init[,c("age", "popPred")]
rec$popFem <- rec$popPred / 2 # sex ratio 1:1
rec <- rec[rec$age >= 8,] # A50 = 8 (50% mature female)
rec[rec$age == 8,c("popPred", "popFem")] <- rec[rec$age == 8,c("popPred", "popFem")]/2 # dolphins of age 8 divided by 2 (only 50% mature)
matFem <- sum(rec$popFem) # total mature females
calfs <- round(matFem/3,0) # Pregnancy rate = 33%
calfs
#calfs <- round(matFem/2,0) # If pregnancy rate = 50%
#calfs
