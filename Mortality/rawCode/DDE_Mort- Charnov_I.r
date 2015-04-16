####################################################################################################
#                             MORTALITY - COMMON DOLPHIN 
#                     Mortality (from weight) - Charnov (1993)
#              created: (camilo.saavedra@vi.ieo.es)  15/10/2013
####################################################################################################

# inputs
# "../../RObjects/vbAll.RData"

# outputs


# Set working directory (to source file location)

# Load von Bertalanffy growth model
load (file="../../RObjects/vbAll.RData")

# All

# Length-at-age
initAll <- data.frame(t_age=0:29)
initAll$length <- predict(vbAll,newdata=initAll) # Predicted values

# Weight-at-age 
initAll$W <- 10^(-4.68088 + 2.88534 * log10(initAll$length))

# Charnov (1993) M = 0.75 * W ^ -0.25; W(Kg) and M(age-1)
# Primates
initAll$MP <- 0.32*(initAll$W)^-0.25
# Other mammals
initAll$MA <- 0.75*(initAll$W)^-0.25



# Plot
plot(initAll$t_age, initAll$MA, type="l", col=2)
lines(initAll$t_age, initAll$MP, col=1)

mean(initAll$M)
