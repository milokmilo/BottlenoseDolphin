####################################################################################################
#                  ENERGY MODELS ANALYSIS - BOTTLENOSE DOLPHIN DIET (HAKE)
#                   Build an object with the diet and dolphin weight
#                   created: (camilo.saavedra@vi.ieo.es) 20/04/2015
####################################################################################################

# inputs
# "../../RData/DDE_AllDiet.csv"
# "../../RObjects/LWmales.RData"
# "../../RObjects/LWfemales.RData"
# "../../RObjects/LWall.RData"

# outputs
# "../../RObjects/TTR_AllDietWeight.RData"


# IMPORTANT: Set working directory (to source file location)
#setwd("./Diet/finalCode")
#setwd("../../Diet/finalCode")


# Read data
ttrDiet <- read.csv2("../../RData/TTR_AllDiet.csv")
load("../../RObjects/LWmales.RData") # LWmales
load("../../RObjects/LWfemales.RData") # LWfemales
load("../../RObjects/LWall.RData") # LWall

ttrDiet$W <- rep(NA,nrow(ttrDiet))

# Adding estimated weight for males, females and indeterminate (three different growth equations)
for (i in 1:nrow(ttrDiet)){
  if (is.na(ttrDiet$Length[i])) {ttrDiet$W[i] <- NA} else {
  if (ttrDiet$Sex[i] == 0) {ttrDiet$W[i] <- exp(LWall[[1]] + LWall[[2]] * ttrDiet$Length[i])} else { # all
    if (ttrDiet$Sex[i] == 1) {ttrDiet$W[i] <- exp(LWmales[[1]] + LWmales[[2]] * ttrDiet$Length[i])} else { # males
      if (ttrDiet$Sex[i] == 2) {ttrDiet$W[i] <- exp(LWfemales[[1]] + LWfemales[[2]] * ttrDiet$Length[i])} else { # females
        stop ("Something is wrong")}
    }
  }
  } 
} # Weight estimated for males, females and for all when sex is not available 


save(file="../../RObjects/TTR_AllDietWeight.RData", ttrDiet) 

