####################################################################################################
#                  ENERGY MODELS ANALYSIS - COMMON DOLPHIN DIET (HAKE)
#                   Build an object with the diet and dolphin weight
#                    created: (camilo.saavedra@vi.ieo.es) 04/12/2013
####################################################################################################

# inputs
# "../../RData/DDE_AllDiet.csv"

# outputs
# "../../RObjects/DDE_AllDietWeight.RData"

# IMPORTANT: Set working directory (to source file location)

# Read data
dde <- read.csv("../../RData/DDE_AllDiet.csv")

# Adding estimated weight for males, females and indeterminate (three different growth equations)
for (i in 1:nrow(dde)){
  if (is.na(dde$Sex[i])) {dde$W[i] <- 10^(-4.68088 + 2.88534 * log10(dde$Length[i]))} else { # all
    if (dde$Sex[i] == 1) {dde$W[i] <- 10^(-4.56751 + 2.82045 * log10(dde$Length[i]))} else { # males
      if (dde$Sex[i] == 2) {dde$W[i] <- 10^(-4.74097 + 2.91829 * log10(dde$Length[i]))} else { # females
        stop ("Something is wrong")}
    }
  }
} # Weight estimates for males and females

save(file="../../RObjects/DDE_AllDietWeight.RData", dde) 

# Transforming logarithmic in exponential
# !! FOR ALL DOLPHINS (BOTH SEXES)
# W = 10^(-4.68088 + 2.88534 * log10(L))# 
# W = 10^(-4.68088) * 10^(2.88534 * log10(L))
# W = 10^(-4.68088) * (10^log10(L))^2.88534
# W = 10^(-4.68088) * L^2.88534
# W = 0.00002085067 * L^2.88534

# L = c(121,240)
# 10^(-4.68088 + 2.88534 * log10(L))# 
# 10^(-4.68088) * 10^(2.88534 * log10(L))
# 10^(-4.68088) * (10^log10(L))^2.88534
# 10^(-4.68088) * L^2.88534
# W = 0.00002085067 * L^2.88534
# 0.0718 * W
# 0.000001497 * L^2.88534
