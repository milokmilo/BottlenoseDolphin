####################################################################################################
#                     LENGTH-WEIGHT CURVES FOR BOTLLENOSE DOLPHIN
#                    Estimate length-weight relationship based on 
#                     Ridgway and Fenner, 1982 relationship and
#            age-length and age-weight relationship in Wells and Scott, 1999
#             created: C.Saavedra (camilo.saavedra@vi.ieo.es) 09/04/2015
####################################################################################################

# inputs
# "../../RData/MaleTTR-AgeWeight.txt
# "../../RData/FemaleTTR-AgeWeight.txt
# "../../RObjects/vbM.RData"
# "../../RObjects/vbF.RData"
# "../../RObjects/vbA.RData"

# outputs
# "../plots/TTR_LengthWeight-Wells&Scott.png"


# IMPORTANT: Set working directory (to source file location)
setwd("./Length weight/finalCode")

# Charge libraries
library(ggplot2)


###########     Length-Weight relationship  (Ridgway and Fenner, 1982)    #############

## From Ridgway and Fenner, 1982 points of Fig.4 were extracted in order to fit 3 regresions ##
# See file "../../RData/LWBottlenose_Ridgway.Fenner.1982.txt"

# All
xAll <- c(188,259)
yAll <- c(85,184)
Alm <- lm(yAll ~ xAll)
#Alm <- lm(yAll ~ log(xAll))
#summary(Alm)
All <- data.frame(xAll=90:350)
All$yAll <- predict(Alm, newdata=All)
plot(xAll,yAll,ylim=c(0,300),xlim=c(90,350))
lines(All)

# Males:
xMales <- c(187,195,200,202,213,217,217,222,227,230,247,243)
yMales <- c(93,101,103,99,116,119,118,121,132,158,158,182)
Malm <- lm(yMales ~ xMales)
#Malm <- lm(yMales ~ log(xMales))
#summary(Malm)
Males <- data.frame(xMales=90:350)
Males$yMales <- predict(Malm, newdata=Males)
plot(xMales,yMales,ylim=c(0,300),xlim=c(90,350))
lines(Males)

# Females:
xFemales <- c(194,205,205,207,223,224,228,219,239,223,234,249,250,244,252,256)
yFemales <- c(98,110,105,105,118,121,131,143,142,171,157,184,173,158,161,155)
Femalm <- lm(yFemales ~ xFemales)
#Femalm <- lm(yFemales ~ log(xFemales))
#summary(Femalm)
Females <- data.frame(xFemales=90:350)
Females$yFemales <- predict(Femalm, newdata=Females)
plot(xFemales,yFemales,ylim=c(0,300),xlim=c(90,350))
lines(Females)

# All2
xAll2 <- c(xMales,xFemales)
yAll2 <- c(yMales,yFemales)
Alm2 <- lm(yAll2 ~ xAll2)
#Alm2 <- lm(yAll2 ~ log(xAll2))
#summary(Alm2)
All2 <- data.frame(xAll2=90:350)
All2$yAll2 <- predict(Alm2, newdata=All2)
plot(xAll2,yAll2,ylim=c(0,300),xlim=c(90,350))
lines(All2)

###
plot(xAll,yAll,ylim=c(0,300),xlim=c(90,350))
points(xMales, yMales, col="blue")
points(xFemales, yFemales, col="red")
lines(All)
lines(All2,col="grey")
lines(Males,col="blue")
lines(Females,col="red")
###


###########     Length-Weight relationship  (Wells and Scott, 1999)    #############

# See files with length/age relationships: "TTR-AgeLength.Males.txt" and "TTR-AgeLength.Females.txt"
# (von Bertalanffy models in vbA, vbM, vbF)
# and files with weight/age relationships: "MaleTTR-AgeWeight.txt" and "FemaleTTR-AgeWeight.txt"


TTR.WM <- read.table("../../RData/MaleTTR-AgeWeight.txt", comment.char="#", head=T)
range(TTR.WM$Y)
# [1]  62.3023 288.9056
TTR.WF <- read.table("../../RData/FemaleTTR-AgeWeight.txt", comment.char="#", head=T)
range(TTR.WF$Y)
# [1]  71.40549 233.76414


# Starting values for parameters 
parVB <- list(Linf=290, K=0.18, to=-3) 

  # Males

# Predict values from  liner model for age/length
load(file="../../RObjects/vbM.RData")
MnewVB <- data.frame(X=seq(1,50,length.out=491)) 
MnewVB$Y <- predict(vbM,newdata=MnewVB)  

# Fit linear model for age/weight and predict values
vbWM <- nls(Y~Linf*(1-exp(-K*(X-to))), start=parVB, data=TTR.WM) 
MWnewVB <- data.frame(X=seq(1,50,length.out=491)) 
MWnewVB$Y <- predict(vbWM,newdata=MWnewVB) 

# Fit linear model for length/weight and predict values
names(MnewVB) <- c("age", "length")
MnewVB$weight <- MWnewVB$Y
MnewVB$sex <- "M"

Mlm <- lm(length ~ log(weight), data=MnewVB)
# Coefficients:
# (Intercept)  log(weight)  
# -115.5         68.9
Mnew <- data.frame(weight=MnewVB$weight) 
Mnew$length <- predict(Mlm,newdata=Mnew) 


  # Females

# Predict values from liner model for age/length
load(file="../../RObjects/vbF.RData")
FnewVB <- data.frame(X=seq(1,50,length.out=491)) 
FnewVB$Y <- predict(vbF,newdata=FnewVB)  

# Fit linear model for age/weight and predict values
vbWF <- nls(Y~Linf*(1-exp(-K*(X-to))), start=parVB, data=TTR.WF)      
FWnewVB <- data.frame(X=seq(1,50,length.out=491)) 
FWnewVB$Y <- predict(vbWF,newdata=FWnewVB) 

# Fit linear model for length/weight and predict values
names(FnewVB) <- c("age", "length")
FnewVB$weight <- FWnewVB$Y
FnewVB$sex <- "F"

Flm <- lm(length ~ log(weight), data=FnewVB)
# Coefficients:
# (Intercept)  log(weight)  
# -131.19        72.03
Fnew <- data.frame(weight=FnewVB$weight) 
Fnew$length <- predict(Flm,newdata=Fnew) 


  # All

# Join males and females
LWAS <- rbind(MnewVB, FnewVB)

# Fit loglinear model for length/weight and predict values
Alm <- lm(length ~ log(weight), data=LWAS)
# Coefficients:
# (Intercept)  log(weight)  
# -117.03        69.25 
Anew <- data.frame(weight=LWAS$weight)
Anew$length <- predict(Alm,newdata=Anew)  


## Plotting 

Mnew$sex <- "M"
Fnew$sex <- "F"
MFnew <- rbind(Mnew,Fnew)

grl <- ggplot(Anew, aes(x=weight, y=length)) + geom_line() +
  geom_line(data=MFnew, aes(x=weight, y=length, colour=factor(sex))) +
              scale_colour_manual(values=c("orangered", "deepskyblue")) +
  ggtitle("Bottlenose dolphin length-weight relationship following Wells & Scott, 1999") +
  xlab("length (cm)") + ylab("weight (kg)  \n") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
      legend.title= element_blank(), legend.position="top") 

png("../plots/TTR_LengthWeight-Wells&Scott.png", 700, 600)
print(grl)
dev.off()






########################################################################################


# Read data
CEMMA <- read.csv("../../RData/CEMMA.csv")
ttrLW <- CEMMA[CEMMA$sp == "TTR",c("sex", "length.real", "length", "length.total")]

## Length real (length measured directly) ##
ttrLW.lr <- ttrLW[complete.cases(ttrLW$length.real),c("sex","length.real")] # Remove rows without values (NA)
ddeLW.lr$allW <- 10^(-4.68088 + 2.88534 * log10(ddeLW.lr$length.real)) # Weight estimates for all dolphins
for (i in 1:nrow(ddeLW.lr)){
  if (is.na(ddeLW.lr$sex[i])) {ddeLW.lr$sexW[i] <- NA} else {
    if (ddeLW.lr$sex[i] == 1) {ddeLW.lr$sexW[i] <- 10^(-4.56751 + 2.82045 * log10(ttrLW.lr$length.real[i]))} else {
      if (ddeLW.lr$sex[i] == 2) {ddeLW.lr$sexW[i] <- 10^(-4.74097 + 2.91829 * log10(ddeLW.lr$length.real[i]))} else {
        stop ("Something is wrong")}
    }
  }
} # Weight estimates for males and females

# Ploting
grl <- ggplot(ddeLW.lr, aes(x=length.real)) + geom_line(aes(x=length.real, y=allW)) + 
  geom_line(aes(x=length.real, y=sexW, colour=factor(sex))) + 
  scale_colour_discrete(name="", breaks=c(1,2), labels=c("Male", "Female")) +
  theme(legend.position = "top") +
  xlab("length (cm)") + ylab("weight (kg)") + ggtitle("DDE Length-Weight curve (real length)")
grl

png("../plots/DDE_LenWei.png")
print(grl)
dev.off()

# All (REFERENCE WEIGHTS FILE)
refWAll <- data.frame(len=seq(80.5,234.5))
refWAll$W <- 10^(-4.68088 + 2.88534 * log10(refWAll$len))
# Males (REFERENCE WEIGHTS FILE)
refWMales <- data.frame(len=seq(80.5,234.5))
refWMales$W <- 10^(-4.56751 + 2.82045 * log10(refWMales$len))
# Females (REFERENCE WEIGHTS FILE)
refWFemales <- data.frame(len=seq(80.5,234.5))
refWFemales$W <- 10^(-4.74097 + 2.91829 * log10(refWFemales$len))

# Calculation of the parameters "alpha" and "beta" for Gadget-Length VBSimple Growth Function
# Wi-Wo=alpha((Li+(Li-Lo))^beta-Li^beta)
# y = ax^b -> log(y) = log(a) + b*log(x)
# All 
# log weight = -4.68088 + 2.88534 log length (N=138, R2=0.990)
aA <- 10^-4.68088
bA <- 2.88534
# Males
# log weight = -4.56751 + 2.82045 log length (N=47, R2=0.992)
aM <- 10^-4.56751
bM <- 2.82045
# Females
# log weight = -4.74097 + 2.91829 log length (N=92, R2=0.989)
aF <- 10^-4.74097
bF <- 2.91829


