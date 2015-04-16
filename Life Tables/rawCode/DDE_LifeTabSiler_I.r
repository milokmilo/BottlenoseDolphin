####################################################################################################
#                                      LIFE TABLE SILER
#
#                   created:  (camilo.saavedra@vi.ieo.es) 22/10/2013
####################################################################################################

# IMPORTANT: Set working directory (to source file location)

# Call libraries 
library(ggplot2)

# Read data 
CEMMA <- read.csv("../../data/RData/CEMMA.csv")
ddePop <- CEMMA[CEMMA$sp == "DDE",c("age")]
ddePop <- ddePop[complete.cases(ddePop)]
# Changing age values (0.5 and 1.5 for 0 and 1)
ddePop[ddePop == 0.5] <- 0
ddePop[ddePop == 1.5] <- 1
# Calculating stranding frequencies by age 
Str <- table(ddePop)
Str <-data.frame(age = as.numeric(rownames(Str)), n = Str[])
ages <- data.frame(age=c(0,seq(1:29))) # Creates a dataframe with all the ages
Str <- merge(ages, Str, by="age", all.x=TRUE) # Merges dataframes
Str[is.na(Str)] <- 0 # Replaces to zeros
Str

# CON EL DE SILER PARTIENDO DEL QX DE LA TABLA DE VIDA
young <- lifeTab[4:6, c("ageCl", "qx")]
adult <- lifeTab[4:29, c("ageCl", "qx")]
adult <- adult[adult$qx!=0,]
old <- lifeTab[7:29, c("ageCl", "qx")]
old <- old[old$qx!=0,]
# Younger mortality
exp <- nls(qx ~ a*exp(b*ageCl), data = young, start = list(a = 10, b = 1))
ag <- c(0,seq(1:29))
a <- coef(exp)[[1]]
b <- coef(exp)[[2]]
fit <- a * exp(b * ag)
# Adult mortality
expA <- nls(qx ~ a*exp(b*ageCl), data =adult, start = list(a = 1, b = 0))
a <- coef(expA)[[1]]
b <- coef(expA)[[2]]
fitA <- a * exp(b * ag)
# Senescence mortality
expS <- nls(qx ~ a*exp(b*ageCl), data = old, start = list(a = 1, b = 0.5))
a <- coef(expS)[[1]]
b <- coef(expS)[[2]]
fitS <- a * exp(b * ag)
# plot
t <- lifeTab[,c("ageCl", "qx")]
mat <- cbind(t,fit, fitA, fitS)
plot(lifeTab[,c("ageCl", "qx")], xlim=c(0,30))
lines(mat$ageCl, mat$fit, col=2)
lines(mat$ageCl, mat$fitA, col=4)
lines(mat$ageCl, mat$fitS, col=3)

# CON EL DE SILER PARTIENDO DEL QX DE LA TABLA DE VIDA FIJANDO M MINIMA COMO 0
young <- lifeTab[4:6, c("ageCl", "qx")]
adult <- lifeTab[4:20, c("ageCl", "qx")]
adult <- adult[adult$qx!=0,]
old <- lifeTab[7:30, c("ageCl", "qx")]
old <- old[old$qx!=0,]
ag <- c(0,seq(1:29))
# Adult mortality
a2 <- min(adult$qx)
# Younger mortality
young$qx <- (young$qx) - a2
exp <- nls(qx ~ a*exp(b*ageCl), data = young, start = list(a = 10, b = 1))
a1 <- coef(exp)[[1]]
b1 <- coef(exp)[[2]]
fit <- a1 * exp(b1 * ag)
# Senescence mortality
old$qx <- (old$qx) - a2
expS <- nls(qx ~ a*exp(b*ageCl), data = old, start = list(a = 1, b = 0.5))
a3 <- coef(expS)[[1]]
b3 <- coef(expS)[[2]]
fitS <- a2 * exp(b2 * ag)
# Model
Siler <- a1*exp(b1*ag)+a2+a3*exp(b3*ag)
# plot
t <- lifeTab[,c("ageCl", "qx")]
mat <- cbind(t,fit, fitA, fitS, Siler)
plot(lifeTab[,c("ageCl", "qx")], xlim=c(0,30))
lines(mat$ageCl, mat$fit+a2, col=2)
abline(h=a2, col=4)
lines(mat$ageCl, mat$fitS+a2, col=3)
lines (mat$ageCl, mat$Siler, col=1)


# CON EL DE SILER PARTIENDO DE LOS VARAMIENTOS OBSERVADOS
# Younger mortality
exp <- nls(n ~ a*exp(b*age), data = Str[4:6,], start = list(a = 10, b = 1))
ag <- c(0,seq(1:29))
a <- coef(exp)[[1]]
b <- coef(exp)[[2]]
fit <- a * exp(b * ag)
# Adult mortality
expA <- nls(n ~ a*exp(b*age), data = Str[4:29,], start = list(a = 1, b = 0))
a <- coef(expA)[[1]]
b <- coef(expA)[[2]]
fitA <- a * exp(b * ag)
# Senescence mortality
expS <- nls(n ~ a*exp(b*age), data = Str[7:29,], start = list(a = 1, b = 0))
a <- coef(expS)[[1]]
b <- coef(expS)[[2]]
fitS <- a * exp(b * ag)
# plot
plot(Str[4:29,], ylim=c(0,200), xlim=c(0,30))
lines(fit, col=2)
lines(fitS, col=3)
lines(fitA, col=4)





