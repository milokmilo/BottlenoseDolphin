####################################################################################################
#                    BOX PLOT BY QUARTER - COMMON DOLPHIN DIET (HAKE)
#              
# 
####################################################################################################


# Set working directory (to source file location)

# Leemos los datos de la base de CEMMA
CEMMA <- read.csv("../../data/RData/CEMMA.csv")

# Seleccionamos solo los delfines comunes
ddeCEMMA <- subset(CEMMA, sp=="DDE")

# Seleccionamos aquellos en que se haya leído al edad y se haya tomado una medida precisa
ddeCEMMAage <- ddeCEMMA[!is.na(ddeCEMMA$age) & !is.na(ddeCEMMA$length),]

# Transformamos las "medias edades" en enteros
ddeCEMMAage$age[ddeCEMMAage$age == 0.5] <- 0
ddeCEMMAage$age[ddeCEMMAage$age == 1.5] <- 1

# Calculamos la media de cada edad
meanlength <- tapply(ddeCEMMAage$length, ddeCEMMAage$age, mean)

# Creamos una matriz con los datos
meanlength <- as.data.frame(round(meanlength,0))
meanlength <- cbind(row.names(meanlength), meanlength)
names(meanlength) <- c("age","mean")

lm(mean ~ age, data=meanlength)

lmT <- lm(length ~ age, data=ddeCEMMAage)
summary(lmT)

plot(ddeCEMMAage$age, ddeCEMMAage$length)
abline(lmT)

