####################################################################################################
#                               GROWTH - COMMON DOLPHIN 
#              Growth curve for mature and immature dolphins (splitted by sex) 
#                          camilo.saavedra@vi.ieo.es  04/10/2013
####################################################################################################

# Set working directory (to source file location)

# IF to = 0 SELECT TRUE, IF NOT FALSE
t0 <- TRUE

# Charge libraries
library(ggplot2)

# Read data
# Note that, if we select only males and females is better not to use maturity data in the subset (there are more dolphins without maturity data)
CEMMA <- read.csv("../../data/RData/CEMMA.csv")
dde <- CEMMA[CEMMA$sp == "DDE",c("length", "sex", "age", "maturity")]
nrow(dde) # Total number of stranded dolphins
length(dde$length[complete.cases(dde$length)]) # Number of dolphins with length data
length(dde$sex[complete.cases(dde$sex)]) # Number of dolphins with sex data
length(dde$age[complete.cases(dde$age)]) # Number of dolphins with age data
length(dde$maturity[complete.cases(dde$maturity)]) # Number of dolphins with maturity data
# Selecting complete cases
dde <- dde[complete.cases(dde),]
nrow(dde) # Number of dolphins with all the data
# Changing middle ages (0.5, 1.5)
dde$age[dde$age==0.5] <- 0
dde$age[dde$age==1.5] <- 1

# Removing Immature female with 18 years !!!
dde <- dde[!(dde$sex == 2 & dde$maturity == 0 & dde$age == 18),]

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
# All mature
vbAllMat <- nls(length~Linf*(1-exp(-K*(age-to))), start=par, data=subset(dde, dde$maturity == 1)); summary(vbAllMat)
# All immature
vbAllImmat <- nls(length~Linf*(1-exp(-K*(age-to))), start=par, data=subset(dde, dde$maturity == 0)); summary(vbAllImmat)
# Mature males 
vbMaleMat <- nls(length~Linf*(1-exp(-K*(age-to))), control= nls.control(maxiter = 100), 
                 start=par, data=subset(dde, dde$sex == 1 & dde$maturity == 1)); summary(vbMaleMat)
# Immature males
vbMaleImmat <- nls(length~Linf*(1-exp(-K*(age-to))), control= nls.control(maxiter = 100), 
                   start=par, data=subset(dde, dde$sex == 1 & dde$maturity == 0)); summary(vbMaleImmat)
# Mature females
vbFemaleMat <- nls(length~Linf*(1-exp(-K*(age-to))), control= nls.control(maxiter = 1e+09, tol = 0.7, minFactor = 1e-05), 
                   start=par1, data=subset(dde, dde$sex == 2 & dde$maturity == 1)); summary(vbFemaleMat)
# Immature females
vbFemaleImmat <- nls(length~Linf*(1-exp(-K*(age-to))), control= nls.control(maxiter = 50, minFactor = 1/1024), 
                     start=par, data=subset(dde, dde$sex == 2 & dde$maturity == 0)); summary(vbFemaleImmat)

# Saving data
save(file="../../data/vbAll.RData", vbAll)
save(file="../../data/vbMales.RData", vbMales)
save(file="../../data/vbFemales.RData", vbFemales)


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
# All mature
min <- min(subset(dde, dde$maturity == 1)$age)
max <- max(subset(dde, dde$maturity == 1)$age)
vbAllMatPred <- data.frame(age=seq(min,max,length.out=300)) 
vbAllMatPred$length <- predict(vbAllMat,newdata=vbAllMatPred)
# All immature
if (t0 == TRUE){
  min <-0
} else {
  min <- min(subset(dde, dde$maturity == 0)$age)
}
min <-0 
max <- max(subset(dde, dde$maturity == 0)$age)
vbAllImmatPred <- data.frame(age=seq(min,max,length.out=300)) 
vbAllImmatPred$length <- predict(vbAllImmat,newdata=vbAllImmatPred)
# Mature males 
min <- min(subset(dde, dde$sex == 1 & dde$maturity == 1)$age)
max <- max(subset(dde, dde$sex == 1 & dde$maturity == 1)$age)
vbMaleMatPred <- data.frame(age=seq(min,max,length.out=300)) 
vbMaleMatPred$length <- predict(vbMaleMat,newdata=vbMaleMatPred)
# Immature males
if (t0 == TRUE){
  min <-0
} else {
  min <- min(subset(dde, dde$sex == 1 & dde$maturity == 0)$age)
}
min <-0 
max <- max(subset(dde, dde$sex == 1 & dde$maturity == 0)$age)
vbMaleImmatPred <- data.frame(age=seq(min,max,length.out=300)) 
vbMaleImmatPred$length <- predict(vbMaleImmat,newdata=vbMaleImmatPred)
# Mature females
min <- min(subset(dde, dde$sex == 2 & dde$maturity == 1)$age)
max <- max(subset(dde, dde$sex == 2 & dde$maturity == 1)$age)
vbFemaleMatPred <- data.frame(age=seq(min,max,length.out=300)) 
vbFemaleMatPred$length <- predict(vbFemaleMat,newdata=vbFemaleMatPred)
# Immature females
if (t0 == TRUE){
  min <-0
} else {
  min <- min(subset(dde, dde$sex == 2 & dde$maturity == 0)$age)
}
min <-0 
max <- max(subset(dde, dde$sex == 2 & dde$maturity == 0)$age)
vbFemaleImmatPred <- data.frame(age=seq(min,max,length.out=300)) 
vbFemaleImmatPred$length <- predict(vbFemaleImmat,newdata=vbFemaleImmatPred)

# ggplot
n <- 2; hcl(h=seq(15, 375-360/n, length=n)%%360, c=100, l=65) # To obtain RGB default colors 
gMatSex <- ggplot(dde, aes(age, length)) + geom_point(aes(colour=factor(sex), shape=factor(maturity)), size=4)
# by Sex
gSexFit <- gMatSex  + geom_line(data=vbMalesPred, colour="#F8766D") + geom_line(data=vbFemalesPred, colour="#00BFC4") +
  ylab("length (cm)") + xlab("age (yr)") + ggtitle("Growth curve splitted by sex") 
#gSexFit
# by Maturity
gMatAllFit <- gMatSex + geom_line(data=vbAllMatPred) + geom_line(data=vbAllImmatPred) +
      ylab("length (cm)") + xlab("age (yr)") + ggtitle("Growth curve splitted by maturity") 
#gMatAllFit
# by Sex and Maturity
gMatSexFit <- gMatSex + geom_line(data=vbMaleMatPred, colour="#F8766D") + geom_line(data=vbMaleImmatPred, colour="#F8766D") +
      geom_line(data=vbFemaleMatPred, colour="#00BFC4") + geom_line(data=vbFemaleImmatPred, colour="#00BFC4") +
      ylab("length (cm)") + xlab("age (yr)") + ggtitle("Growth curve splitted by sex and maturity") 
#gMatSexFit


if (t0 == TRUE){
  png("../../plots/DDE_GrowthSex-to.png", width = 880, height = 480)
  print(gSexFit)
  dev.off()
  png("../../plots/DDE_GrowthMat-to.png", width = 880, height = 480)
  print(gMatAllFit)
  dev.off()
  png("../../plots/DDE_GrowthMatSex-to.png", width = 880, height = 480)
  print(gMatSexFit)
  dev.off()
} else {
  png("../../plots/DDE_GrowthSex.png", width = 880, height = 480)
  print(gSexFit)
  dev.off()
  png("../../plots/DDE_GrowthMat.png", width = 880, height = 480)
  print(gMatAllFit)
  dev.off()
  png("../../plots/DDE_GrowthMatSex.png", width = 880, height = 480)
  print(gMatSexFit)
  dev.off()
}