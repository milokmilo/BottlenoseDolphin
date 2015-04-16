####################################################################################################
#                     MODELS COMPARISON
#
#       created: (camilo.saavedra@vi.ieo.es) 14/11/2013
####################################################################################################


# inputs
# "../../RObjects/lifeTables/LifeTabn.RData"
# "../../RObjects/lifeTables/LifeTabglmPoisStr02.RData"
# "../../RObjects/lifeTables/lifeSiler01.RData"

# outputs

# IMPORTANT: Set working directory (to source file location)

# Charge libraries
library(reshape)
library(ggplot2)

# Loading data
load("../../RObjects/lifeTables/LifeTabn.RData")
lifeN <- lifeTab
load("../../RObjects/lifeTables/LifeTabglmPoisStr02.RData")
lifeGLM1 <- lifeTab
load("../../RObjects/lifeTables/lifeSiler01.RData")
lifeSiler <- lifeSiler


df <- data.frame(age=lifeN$age, RawS=round(lifeN$nx/10,1), RawM=lifeN$Z,
           GLMS=round(lifeGLM1$nx/10,1), GLMM=lifeGLM1$Z,
           SilerS=round(lifeSiler$nxS/10,1), SilerM=lifeSiler$ZS)

dfplot <-  df
dfplot$RawS <- dfplot$RawS/100
dfplot$GLMS <- dfplot$GLMS/100
dfplot$SilerS <- dfplot$SilerS/100

# Plot
plot(dfplot$age, dfplot$RawS, type="l", col="red", lty=2)
lines(dfplot$age, dfplot$RawM, col="red", lty=1)
lines(dfplot$age, dfplot$GLMS, col="blue", lty=2)
lines(dfplot$age, dfplot$GLMM, col="blue", lty=1)
lines(dfplot$age, dfplot$SilerS, col="green", lty=2)
lines(dfplot$age, dfplot$SilerM, col="green", lty=1)

# ggPlot

ggdf <- melt(dfplot, id.vars="age")

gg <- ggplot(ggdf, aes(age, value)) + 
  geom_line(aes(group=variable, linetype=variable, colour=variable)) +
  scale_linetype_manual(values=c(2,1,2,1,2,1)) +
  scale_colour_manual(values=c("red", "red", "blue", "blue", "green", "green")) +
  ggtitle("Life Tables comparison (Raw, GLM, Siler)") + 
  ylab("Survivorship / Mortality") + xlab("age") +
  theme(legend.position="none")
  
png("../plots/DDE_ModComp.png", width=800, height=600)
print(gg)
dev.off()



