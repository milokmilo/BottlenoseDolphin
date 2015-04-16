####################################################################################################
#                               RECRUITMENT (BIRTHS)
#
#                   created: (camilo.saavedra@vi.ieo.es) 07/11/2013
####################################################################################################


# IMPORTANT: Set working directory (to source file location)


# Loading maturity data
load("../../RObjects/maturity/matFemC.RData")
# Loading population structure data
load("../../RObjects/popStr/popStr.RData")

# Total maturing females at the population that could get birth next year (remove last age class)
Mat <- Mat[-length(Mat)]
Mat <- c(0,Mat)
totMatFem <- apply(popStr[,c(2,3,4,5)], 2, function(x)sum(x/2*Mat))
#head(popStr,1) # Comparing with the first age class abundance

# Number of births with three different pregnancy rates (25%, 33%, 50%)
rec <- rbind(totMatFem/4, totMatFem/3, totMatFem/2)
rec <- round(rec) 
rownames(rec) <- c("25%","33%","50%")

rec

