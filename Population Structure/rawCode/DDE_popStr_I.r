####################################################################################################
#                                POPULATION STRUCTURE
#
#                   created: (camilo.saavedra@vi.ieo.es) 07/11/2013
####################################################################################################

# inputs
# "../../RObjects/lifeTables/LifeTabn.RData"
# "../../RObjects/lifeTables/LifeTabStr02.RData"
# "../../RObjects/lifeTables/lifeSiler01.RData"
# "../../RObjects/lifeTables/lifeSilerNByC.RData"

# outputs
# "../../RObjects/popStr/popStr.RData"

# IMPORTANT: Set working directory (to source file location)

# Population size
n <- 15000

# Loading GLM life table (actual data)
load("../../RObjects/lifeTables/LifeTabn.RData")
LifeTabn <- lifeTab
# Loading GLM life table (3 age classes removed)
load("../../RObjects/lifeTables/LifeTabStr01.RData")
LifeTabStr01 <- lifeTab
# Loading Siler life table (2 age classes removed)
load("../../RObjects/lifeTables/lifeSiler01.RData")
lifeSiler01 <- lifeSiler
# Loading Siler life table (1 age class removed) -> No-ByCatch data
#load("../../RObjects/lifeTables/lifeSilerNByC.RData")
#lifeSiler00NByC <- lifeSilerNByC

# Population structure actual data
nStr <- LifeTabn$nx/sum(LifeTabn$nx) * n
# Population structure GLM (removing 3 age clases)
GLM01Str <- LifeTabStr01$nx/sum(LifeTabStr01$nx) * n
# Population structure Siler
Siler01Str <- lifeSiler01$nx/sum(lifeSiler01$nx) * n
# Population structure Siler No ByCatch
#Siler00NByCStr <- lifeSiler00NByC$nx/sum(lifeSiler00NByC$nx) * n

# Updated life tables (total population = 15000)
LifeTabn$nx <- nStr
LifeTabStr01$nx <- GLM01Str
lifeSiler01$nxS <- Siler01Str
#lifeSiler00NByC$nxS <- Siler00NByCStr

# N-by-age with the different models
age <- c(0,seq(1:29))
popStr <- data.frame (age=age, n=round(nStr,0), 
                GLM=round(GLM01Str,0), 
                Siler=round(Siler01Str, 0)
#                SilerNByC=round(Siler00NByCStr, 0)
                )

# Saving data
save(popStr, file="../../RObjects/popStr/popStr.RData")







