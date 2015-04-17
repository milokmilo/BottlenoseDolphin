####################################################################################################
#                     LENGTH-WEIGHT CURVES FOR BOTLLENOSE DOLPHIN
#                    Estimate length-weight relationship based on 
#                     Ridgway and Fenner, 1982 relationship and
#            age-length and age-weight relationship in Wells and Scott, 1999
#             created: C.Saavedra (camilo.saavedra@vi.ieo.es) 09/04/2015
#             modified: C.Saavedra (camilo.saavedra@vi.ieo.es) 17/04/2015
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
#setwd("./Length weight/finalCode")
#setwd("../..//Length weight/finalCode")

# Charge libraries
library(ggplot2)


###########     Length-Weight relationship  (Ridgway and Fenner, 1982)    #############

## From Ridgway and Fenner, 1982 points of Fig.4 were extracted in order to fit 3 regresions ##
# See file "../../RData/LWBottlenose_Ridgway.Fenner.1982.txt"


  # All

xAll <- c(188,259)
yAll <- c(85,184)
Alm <- lm(yAll ~ xAll)
All <- data.frame(x=xAll, y=yAll)
#Allem <- nls(y ~ exp(a + b * x), data = All, start = list(a = 2.7, b = 0.01))
All <- data.frame(xAll=90:350)
All$yAll <- predict(Alm, newdata=All)
#All$yAll <- predict(Allem, newdata=All)
plot(xAll,yAll,ylim=c(0,300),xlim=c(90,350))
lines(All)


  # Males:

xMales <- c(187,195,200,202,213,217,217,222,227,230,247,243)
yMales <- c(93,101,103,99,116,119,118,121,132,158,158,182)
#Malm <- lm(yMales ~ xMales)
M <- data.frame(xMales=xMales, yMales=yMales)
Mem <- nls(yMales ~ exp(a + b * xMales), data = M, start = list(a = 4, b = 0.003))
Males <- data.frame(xMales=90:350)
#Males$yMales <- predict(Malm, newdata=Males)
Males$yMales <- predict(Mem, newdata=Males)
plot(xMales,yMales,ylim=c(0,300),xlim=c(90,350))
lines(Males)
summary(Fem)
coef(Fem)
summary(Mem)
coef(Mem)
# a          b 
# 2.38196812 0.01119829 


  # Females:

xFemales <- c(194,205,205,207,223,224,228,219,239,223,234,249,250,244,252,256)
yFemales <- c(98,110,105,105,118,121,131,143,142,171,157,184,173,158,161,155)
#Femalm <- lm(yFemales ~ xFemales)
F <- data.frame(xFemales=xFemales, yFemales=yFemales)
Fem <- nls(yFemales ~ exp(a + b * xFemales), data = F, start = list(a = 4, b = 0.003))
Females <- data.frame(xFemales=90:350)
#Females$yFemales <- predict(Femalm, newdata=Females)
Females$yfemales <- predict(Fem, newdata=Females)
plot(xFemales,yFemales,ylim=c(0,300),xlim=c(90,350))
lines(Females)
summary(Fem)
coef(Fem)
#  a           b 
#  2.971140356 0.008564414 


  # All2

xAll2 <- c(xMales,xFemales)
yAll2 <- c(yMales,yFemales)
A <- data.frame(xAll2=xAll2, xyAll2=yAll2)
Aem <- nls(yAll2 ~ exp(a + b * xAll2), data = A, start = list(a = 4, b = 0.003))
All2 <- data.frame(xAll2=90:350)
#All2$yAll2 <- predict(Alm2, newdata=All2)
All2$yAll2 <- predict(Aem, newdata=All2)
plot(xAll2,yAll2,ylim=c(0,300),xlim=c(90,350))
lines(All2)
summary(Aem)
coef(Aem)
#  a           b 
#  2.763609038 0.009463226 


###
plot(xAll2,yAll2,ylim=c(0,450),xlim=c(90,350))
points(xMales, yMales, col="blue")
points(xFemales, yFemales, col="red")
#lines(All)
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

# Predict values from von Bertalanffy model for age/length
load(file="../../RObjects/vbM.RData")
MnewVB <- data.frame(X=seq(1,50,length.out=491)) 
MnewVB$Y <- predict(vbM,newdata=MnewVB)  
plot(x=MnewVB$X,y=MnewVB$Y,ylim=c(150,300),xlim=c(0,50), type="l")

# Fit von Bertalanffy model for age/weight and predict values
vbWM <- nls(Y~Linf*(1-exp(-K*(X-to))), start=parVB, data=TTR.WM) 
MWnewVB <- data.frame(X=seq(1,50,length.out=491)) 
MWnewVB$Y <- predict(vbWM,newdata=MWnewVB) 
plot(x=MWnewVB$X,y=MWnewVB$Y,ylim=c(50,300),xlim=c(0,50), type="l")

# Fit exponential model for length/weight and predict values
names(MnewVB) <- c("age", "length")
MnewVB$weight <- MWnewVB$Y
MnewVB$sex <- "M"
Mlm <- nls(weight ~ exp(a + b * length), data=MnewVB, start = list(a = 4, b = 0.003))
coef(Mlm)
# a          b 
# 1.4900499 0.0152306 
Mnew <- data.frame(length=seq(90,300)) 
Mnew$weight <- predict(Mlm,newdata=Mnew) 
plot(x=MnewVB$length,y=MnewVB$weight,xlim=c(90,300),ylim=c(0,350))
lines(Mnew, col="blue")

  # Females

# Predict values from von Bertalanffy model for age/length
load(file="../../RObjects/vbF.RData")
FnewVB <- data.frame(X=seq(1,50,length.out=491)) 
FnewVB$Y <- predict(vbF,newdata=FnewVB)
plot(x=FnewVB$X,y=FnewVB$Y,ylim=c(150,300),xlim=c(0,50), type="l")

# Fit von Bertalanffy model for age/weight and predict values
vbWF <- nls(Y~Linf*(1-exp(-K*(X-to))), start=parVB, data=TTR.WF)      
FWnewVB <- data.frame(X=seq(1,50,length.out=491)) 
FWnewVB$Y <- predict(vbWF,newdata=FWnewVB) 
plot(x=FWnewVB$X,y=FWnewVB$Y,ylim=c(50,300),xlim=c(0,50), type="l")

# Fit exponential model for length/weight and predict values
names(FnewVB) <- c("age", "length")
FnewVB$weight <- FWnewVB$Y
FnewVB$sex <- "F"
Flm <- nls(weight ~ exp(a + b * length), data=FnewVB, start = list(a = 4, b = 0.003))
coef(Flm)
# a          b 
# 1.67861312 0.01446434 
Fnew <- data.frame(length=seq(90,300)) 
Fnew$weight <- predict(Flm,newdata=Fnew) 
plot(x=FnewVB$length, y=FnewVB$weight, xlim=c(90,300), ylim=c(0,350))
lines(Fnew, col="red")

  # All

# Join males and females
LWAS <- rbind(MnewVB, FnewVB)

# Fit exponential model for length/weight and predict values
Alm <- nls(weight ~ exp(a + b * length), data=LWAS, start = list(a = 4, b = 0.003))
# Coefficients:
# (Intercept)  log(weight)  
# -117.03        69.25 
Anew <- data.frame(length=seq(90,300))
Anew$weight <- predict(Alm,newdata=Anew)  


## Plotting 

Mnew$sex <- "M"
Fnew$sex <- "F"
MFnew <- rbind(Mnew,Fnew)

grl <- ggplot(Anew, aes(x=length, y=weight)) + geom_line() +
  geom_line(data=MFnew, aes(x=length, y=weight, colour=factor(sex))) +
              scale_colour_manual(values=c("orangered", "deepskyblue")) +
  ggtitle("Bottlenose dolphin length-weight relationship following Wells & Scott, 1999") +
  xlab("weight (kg)") + ylab("length (cm) \n") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
      legend.title= element_blank(), legend.position="top") 

png("../plots/TTR_LengthWeight-Wells&Scott.png", 700, 600)
print(grl)
dev.off()



