####################################################################################################
#                                POPULATION STRUCTURE
#                   Population structure calculated with different models 
#          (observed data, glm and Siler removing the first two ages and Siler without bycatch)
#                   created: (camilo.saavedra@vi.ieo.es) 07/11/2013
####################################################################################################

# inputs
# "../../RObjects/lifeglmN.RData"
# "../../RObjects/lifeglmPois2.RData"
# "../../RObjects/lifeSiler2.RData"
## "../../RObjects/lifeSilerNByC.RData"

# outputs
# "../../RObjects/popStr.RData"

# IMPORTANT: Set working directory (to source file location)

# Population size
n <- 15000

## Loading data ##

# Loading GLM life table (observed data)
load("../../RObjects/lifeglmN.RData")
# Loading GLM life table (2 age classes removed)
load("../../RObjects/lifeglmPois2.RData")
# Loading Siler life table (2 age classes removed)
load("../../RObjects/lifeSiler2.RData")
# Loading Siler life table (1 age class removed) -> No-ByCatch data
#load("../../RObjects/lifeSilerNByC.RData")


# Population structure observed data
Obs <- lifeglmPois0$nx/sum(lifeglmPois0$nx) * n
# Population structure GLM (removing 3 age clases)
GLM2 <- lifeglmPois2$nx/sum(lifeglmPois2$nx) * n
# Population structure Siler
Siler2 <- lifeSiler2$nx/sum(lifeSiler2$nx) * n
# Population structure Siler No ByCatch
#SilerNByCStr <- lifeSilerNByC$nx/sum(lifeSilerNByC$nx) * n



# N-by-age with the different models
age <- c(0,seq(1:29))
popStr <- data.frame (age=age, Obs=round(Obs,0), 
                GLM2=round(GLM2,0), 
                Siler2=round(Siler2, 0)
#                SilerNByC=round(SilerNByCStr, 0)
                )

# Saving data
save(popStr, file="../../RObjects/popStr.RData")




