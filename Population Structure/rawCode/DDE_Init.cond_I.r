####################################################################################################
#                        POPULATION STRUCTURE - COMMON DOLPHIN 
#             Analisys of stranded-at-age, initial conditions and recruitment
#                  C.Saavedra (camilo.saavedra@vi.ieo.es) 24/09/2013
####################################################################################################

# IMPORTANT: Set working directory (to source file location)

# Charge libraries
library(ggplot2)

# Read data
ddeMales <- read.csv("../../data/RData/DDE_MalesFiona.csv")
ddeFemales <- read.csv("../../data/RData/DDE_FemalesFiona.csv")

# Add "sex" column and bind data
ddeMales <- data.frame(ddeMales, sex="Male")
ddeFemales <- data.frame(ddeFemales, sex="Female")
dde <- rbind(ddeMales, ddeFemales)


####################################################################################

## sex ratio

ntot <- nrow(dde)
nMale <- nrow(dde[dde$sex=="Male",])
nFemale <- nrow(dde[dde$sex=="Female",])

Male <- 1
Female <- nFemale/nMale
sxr <- data.frame(Male=Male,Female=round(Female,1))
rownames(sxr) <- "sex-ratio" ; sxr

### Predecimos con el modelo la talla a cada edad (con el modelo general de todos los delfines) ###
# Starting values for parameters 
par<-list(Linf=219, K=0.18, to=-3) 
# Growth model  with the parameters estimated with non linear least squares
vbAll<-nls(Lt_obs~Linf*(1-exp(-K*(t_age-to))), start=par, data=dde)   
pred <- data.frame(t_age=seq(0.5,29.5)) # Para las edades de 0 a 30
pred$Lt_est <- predict(vbAll, newdata=pred)

## total abundance by age
ageDistr <- tapply(dde$t_age, dde$t_age, length)
ageDistr <- as.data.frame(ageDistr)
colnames(ageDistr) <- c("n")
ageDistr$t_age <- as.numeric(rownames(ageDistr))
# percentage of each age class
ageDistr$perc <- ageDistr$n/sum(ageDistr$n)

# empezamos con una población de 1500 delfines (aprox lo estimado en SCANS para 4/5 de la zona W)
nPop <- 15000
ageDistr$pop <- ageDistr$perc*nPop
# Calculating the weight by the predicted length for each age class
ageDistr <- merge(pred, ageDistr, by="t_age", all.x=TRUE)
ageDistr$w <- 10^(-4.68088+(2.88534*log10(ageDistr$Lt_est)))  # W=10^(-4.68088+2.88534*LOG10(C6))
ageDistr$wTot <- round(ageDistr$w * ageDistr$pop,0) # kg of dolphins by age

# plot
g <- ggplot(ageDistr,aes(t_age,n)) + geom_point()
g <- g + geom_smooth(method="loess") + # methods= lm, glm, gam, loess, rlm.
  ylab("Number of stranded dolphins") + xlab("Age of stranded dolphins") + 
  ggtitle("Frequency of stranded dolphins by age")
print(g)
png("../../plots/strandedByAge.png", width=600, height=480)
print(g)
dev.off()

# Data for params file, initial condition (age)
ageDistr$init <- round(ageDistr$pop / 10000,3)  #Porque las codiciones iniciales están expresadas en diecmiles
ageDistr$age <- paste("age", seq(1:30), sep="")
ageDistr[,c("age","n")] # Strandings frequency 
ageDistr[,c("age","init")] # kg per age ## INITIAL CONDITIONS ##

# Recruits per year (33% pregnancy female - if sexratio = 1:1)
calf <- nPop * 0.5 * 0.3 * 0.3 # calfs per year (dividido entre: proporción de hembras, de maduras y de embarazadas)
calf/10000
# CALCULAR REALMENTE LAS HEMBRAS MADURAS EN EL TOTAL DE LOS ANIMALES VARADOS



# Maturity 12 yr
# firs birth 13 yr
# mature females 0.290
# sex-ratio 50%
# gestation time 0.958

(0.290/0.958)*0.5 # parece que asume que paren una cria anualmente



