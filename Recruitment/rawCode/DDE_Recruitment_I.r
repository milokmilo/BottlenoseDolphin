####################################################################################################
#                                   RECRUITMENT
#           
#                  created: (camilo.saavedra@vi.ieo.es) 24/10/2013
####################################################################################################

# IMPORTANT: Set working directory (to source file location)

# Loading data
load(file="../../data/RObjects/maturity/Maturity.RData")
load(file="../../data/RObjects/lifeTables/lifeSiler01.RData")
#lifeSiler01 <- lifeSiler
#load(file="../../data/RObjects/lifeTables/LifeTabStr02.RData")
#lifeStr02 <- lifeTab
#rm(list=c("lifeSiler", "lifeTab"))


pop <- (lifeTab$nx/sum(lifeTab$nx))*15000
#pop <- (lifeStr02$nx/sum(lifeStr02$nx))*15000
#sum(pop)
matPop <- pop * Maturity
sum(matPop)
femMatPop <- matPop * 0.5
Reprod <- sum(femMatPop)
recruits <- Reprod *0.3






