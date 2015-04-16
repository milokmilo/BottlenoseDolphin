####################################################################################################
#                               GROWTH - COMMON DOLPHIN 
#              Growth curve for mature and immature dolphins (splitted by sex) 
#                          camilo.saavedra@vi.ieo.es  04/10/2013
####################################################################################################

# Set working directory (to source file location)

# IF to = 0 SELECT TRUE, IF NOT FALSE
t0 <- FALSE

# Charge libraries
library(ggplot2)

# Read data

# Note that, if we select only males and females is better not to use maturity data in the subset (there are more dolphins without maturity data)
CEMMA <- read.csv("../../data/RData/CEMMA.csv")
dde <- CEMMA[CEMMA$sp == "DDE",c("length", "sex", "age")]
nrow(dde) # Total number of stranded dolphins
length(dde$length[complete.cases(dde$length)]) # Number of dolphins with length data
length(dde$sex[complete.cases(dde$sex)]) # Number of dolphins with sex data
length(dde$age[complete.cases(dde$age)]) # Number of dolphins with age data


# Selecting complete cases
dde <- dde[complete.cases(dde),]
nrow(dde) # Number of dolphins with all the data

# Changing middle ages (0.5, 1.5)
dde$age[dde$age==0.5] <- 0
dde$age[dde$age==1.5] <- 1

# Starting values for parameters for the von Bertalanffy growth model
if (t0 == TRUE) {
  to <- 0 
  par <-list(Linf=217, K=0.19) 
} else { 
  par <-list(Linf=217, K=0.19, to=-3.8) # Don't converge with the other parametres
}
# Growth model, parameters estimated with non linear least 
# All
vbAll <- nls(length~Linf*(1-exp(-K*(age-to))), start=par, data=dde); summary(vbAll)
# All males
vbMales <- nls(length~Linf*(1-exp(-K*(age-to))), start=par, data=subset(dde, dde$sex == 1)); summary(vbMales)
# All females
vbFemales <- nls(length~Linf*(1-exp(-K*(age-to))), start=par, data=subset(dde, dde$sex == 2)); summary(vbFemales)

# Saving data
save(file="../../data/vbAll.RData", vbAll)
save(file="../../data/vbMales.RData", vbMales)
save(file="../../data/vbFemales.RData", vbFemales)

# Mean length by age (INITIAL CONDITIONS FILE)
# All
initAll <- data.frame(age=0:29)
initAll$length <- predict(vbAll,newdata=initAll) # Predicted values
# Males
initMales <- data.frame(age=0:29)
initMales$length <- predict(vbMales,newdata=initMales) # Predicted values
# Females
initFemales <- data.frame(age=0:29)
initFemales$length <- predict(vbFemales,newdata=initFemales) # Predicted values

# All
if (t0 == TRUE){
    min <-0
  } else {
    min <- min(dde$age)
}
max <- max(dde$age)
vbAllPred <- data.frame(age=seq(min,max,length.out=300))
vbAllPred$length <- predict(vbAll,newdata=vbAllPred) # Predicted values
# All males
if (t0 == TRUE){
  min <-0
} else {
  min <- min(subset(dde, dde$sex == 1)$age)
}
max <- max(subset(dde, dde$sex == 1)$age)
vbMalesPred <- data.frame(age=seq(min,max,length.out=300)) 
vbMalesPred$length <- predict(vbMales,newdata=vbMalesPred) # Predicted values
# All females
if (t0 == TRUE){
  min <-0
} else {
  min <- min(subset(dde, dde$sex == 2)$age)
}
max <- max(subset(dde, dde$sex == 2)$age)
vbFemalesPred <- data.frame(age=seq(min,max,length.out=300)) 
vbFemalesPred$length <- predict(vbFemales,newdata=vbFemalesPred) # Predicted values


# ggplot
n <- 2; hcl(h=seq(15, 375-360/n, length=n)%%360, c=100, l=65) # To obtain RGB default colors 
gMatSex <- ggplot(dde, aes(age, length)) + geom_point(aes(colour=factor(sex), shape=factor(maturity)), size=4)
# by Sex
gSexFit <- gMatSex  + geom_line(data=vbMalesPred, colour="#F8766D") + geom_line(data=vbFemalesPred, colour="#00BFC4") +
  ylab("length (cm)") + xlab("age (yr)") + ggtitle("Growth curve splitted by sex") 
#gSexFit

if (t0 == TRUE){
  png("../../plots/DDE_GrowthSex-to.png", width = 880, height = 480)
  print(gSexFit)
  dev.off()
} else {
  png("../../plots/DDE_GrowthSex.png", width = 880, height = 480)
  print(gSexFit)
  dev.off()
}