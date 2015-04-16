####################################################################################################
#                               RECRUITMENT (BIRTHS)
#          Recruits calculated with three pregnancy rates for all fitted models
#     (observed data, glm and Siler removing the first two ages and Siler without bycatch)
#                   created: (camilo.saavedra@vi.ieo.es) 07/11/2013
####################################################################################################

# inputs
# "../../RObjects/matFemC.RData"
# "../../RObjects/popStr.RData"

# outputs
# "../../RObjects/Rec.RData"


# IMPORTANT: Set working directory (to source file location)


# Loading maturity data
load("../../RObjects/matFemC.RData")
# Loading population structure data
load("../../RObjects/popStr.RData")

# Total maturing females at the population that could get birth next year (remove last age class)
Mat <- Mat[-length(Mat)]
Mat <- c(0,Mat)
totMatFem <- apply(popStr[,-c(1)], 2, function(x)sum(x/2*Mat))
#head(popStr,1) # Comparing with the first age class abundance

# Number of births with three different pregnancy rates (25%, 33%, 50%)
rec <- rbind(totMatFem/4, totMatFem/3, totMatFem/2)
rec <- round(rec) 
rownames(rec) <- c("25%","33%","50%")

# Saving data
save(file="../../RObjects/Rec.RData", rec)

