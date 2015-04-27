################################################################################################## #
#                     LENGTH-WEIGHT CURVES FOR BOTLLENOSE DOLPHIN
#                    Estimate length-weight relationship based on 
#                     Ridgway and Fenner, 1982 relationship and
#            age-length and age-weight relationship in Wells and Scott, 1999
#             created: C.Saavedra (camilo.saavedra@vi.ieo.es) 09/04/2015
#             modified: C.Saavedra (camilo.saavedra@vi.ieo.es) 17/04/2015
################################################################################################## #

# inputs
# "../../RData/MaleTTR-AgeWeight.txt
# "../../RData/FemaleTTR-AgeWeight.txt
# "../../RObjects/vbM.RData"
# "../../RObjects/vbF.RData"
# "../../RObjects/vbA.RData"

# outputs
# "../plots/TTR_LengthWeight-Wells&Scott.png"
# "../../RObjects/LWmales.RData"
# "../../RObjects/LWfemales.RData"
# "../../RObjects/LWall.RData"

################################################################################################## #


# IMPORTANT: Set working directory (to source file location)
#setwd("./Length weight/finalCode")
#setwd("../../Length weight/finalCode")

# Charge libraries
library(ggplot2)
mytheme <- theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
                 legend.title= element_blank(), legend.position="top")


###########     Length-Weight relationship  (Pierce et al., 2005)    #############


#log W = -5.597 + 3.2692 log L (R2=0.711)
#log W = -5.597 + log L^3.2692
#10^log W = 10^(-5.597 + log L^3.2692)
#W = 10^(-5.597) * 10^(log L^3.2692)
#W = 10^(-5.597) * L^3.2692
#W = 0.00000259298 + L^3.2692
LWallPierce <- c(0.00000259298, 3.2692)
save(LWallPierce, file="../../RObjects/LWallPierce.RData")


###########     Length-Weight relationship  (Ridgway and Fenner, 1982)    #############

## From Ridgway and Fenner, 1982 points of Fig.4 were extracted in order to fit 3 regresions ##
# See file "../../RData/LWBottlenose_Ridgway.Fenner.1982.txt"


  # All

#LAll <- c(188,259)
#WAll <- c(85,184)
#Alm <- lm(WAll ~ LAll)
#All <- data.frame(L=LAll, W=WAll)
##Allem <- nls(WAll ~ (a * LAll^b), data = All, start = list(a = 0.01, b = 2))
#All <- data.frame(LAll=90:350)
#All$WAll <- predict(Alm, newdata=All)
##All$WAll <- predict(Allem, newdata=All)
#plot(LAll,WAll,ylim=c(0,300),xlim=c(90,350))
#lines(All)


  # Males:

LMales <- c(187,195,200,202,213,217,217,222,227,230,247,243)
WMales <- c(93,101,103,99,116,119,118,121,132,158,158,182)
M <- data.frame(LMales=LMales, WMales=WMales)
#Mem <- nls(WMales ~ exp(a + b * LMales), data = M, start = list(a = 4, b = 0.003))
Mem <- nls(WMales ~ (a * LMales^b), data = M, start = list(a = -10, b = 3))
Males <- data.frame(LMales=90:350)
Males$WMales <- predict(Mem, newdata=Males)
#plot(LMales,WMales,ylim=c(0,300),xlim=c(90,350))
#lines(Males)
#summary(Mem)
LWmalesRF <- coef(Mem)
# a          b 
# 0.0002219646 2.4596457684 
save(LWmalesRF, file="../../RObjects/LWmalesRF.RData")


  # Females:

LFemales <- c(194,205,205,207,223,224,228,219,239,223,234,249,250,244,252,256)
WFemales <- c(98,110,105,105,118,121,131,143,142,171,157,184,173,158,161,155)
F <- data.frame(LFemales=LFemales, WFemales=WFemales)
#Fem <- nls(WFemales ~ exp(a + b * LFemales), data = F, start = list(a = 4, b = 0.003))
Fem <- nls(WFemales ~ (a * LFemales^b), data = F, start = list(a = -10, b = 3))
Females <- data.frame(LFemales=90:350)
Females$Wfemales <- predict(Fem, newdata=Females)
#plot(LFemales,WFemales,ylim=c(0,300),xlim=c(90,350))
#lines(Females)
#summary(Fem)
LWfemalesRF <- coef(Fem)
# a           b 
# 0.003172785 1.967658822 
save(LWfemalesRF, file="../../RObjects/LWfemalesRF.RData")


  # All2

LAll2 <- c(LMales,LFemales)
WAll2 <- c(WMales,WFemales)
A <- data.frame(LAll2=LAll2, WAll2=WAll2)
#Aem <- nls(WAll2 ~ exp(a + b * LAll2), data = A, start = list(a = 4, b = 0.003))
Aem <- nls(WAll2 ~ (a * LAll2^b), data = A, start = list(a = -10, b = 3))
All2 <- data.frame(LAll2=90:350)
#All2$WAll2 <- predict(Alm2, newdata=All2)
All2$WAll2 <- predict(Aem, newdata=All2)
#plot(LAll2,WAll2,ylim=c(0,300),xlim=c(90,350))
#lines(All2)
#summary(Aem)
LWallRF <- coef(Aem)
# a           b 
# 0.001232174 2.141609659 
save(LWallRF, file="../../RObjects/LWallRF.RData")


### Plotting Length Weight following Ridgway and Fenner 1982

ggA <- data.frame(LAll2,WAll2)
names(ggA) <- c("Length", "Weight")
names(All2)  <- c("Length", "Weight")
Males$sex <- "M"
names(Males)  <- c("Length", "Weight", "Sex")
Females$sex <- "F"
names(Females)  <- c("Length", "Weight", "Sex")
ggMF <- rbind(Males, Females)


LW <- ggplot(ggA, aes(Length, Weight)) + geom_point() + xlim(90,300) + ylim(0,350) +
            geom_line(data=All2, col="black") + 
            geom_line(data=ggMF, aes(x=Length, y=Weight, colour=factor(Sex))) +
                      scale_colour_manual(values=c("orangered", "deepskyblue")) +
            labs(x="Length", y="Weight \n", 
                 title="Bottlenose dolphin Length-weight relationship following Ridgway and Fenner 1982") +
            mytheme

png("../plots/TTR_LengthWeight-Ridgway&Fenner.png")
print(LW)
dev.off()  


###########     Length-Weight relationship  (Wells and Scott, 1999)    #############

# See files with length/age relationships: "TTR-AgeLength.Males.txt" and "TTR-AgeLength.Females.txt"
# (von Bertalanffy models in vbA, vbM, vbF)
# and files with weight/age relationships: "MaleTTR-AgeWeight.txt" and "FemaleTTR-AgeWeight.txt"


TTR.WM <- read.table("../../RData/MaleTTR-AgeWeight.txt", comment.char="#", head=T)
range(TTR.WM$Y)
names(TTR.WM) <- c("age", "weight")
# [1]  62.3023 288.9056
TTR.WF <- read.table("../../RData/FemaleTTR-AgeWeight.txt", comment.char="#", head=T)
range(TTR.WF$Y)
names(TTR.WF) <- c("age", "weight")
# [1]  71.40549 233.76414


# Starting values for parameters 
parVB <- list(Linf=290, K=0.18, to=-3) 

  # Males

# Predict values from von Bertalanffy model for age/length
load(file="../../RObjects/vbM.RData")
ALMales <- data.frame(age=seq(0,50,length.out=51)) 
ALMales$length <- predict(vbM, newdata=ALMales)  
#plot(x=ALMales$age,y=ALMales$length,ylim=c(100,300),xlim=c(0,50), type="l")

# Fit von Bertalanffy model for age/weight and predict values
vbWM <- nls(weight ~ Linf*(1-exp(-K*(age-to))), start=parVB, data=TTR.WM) 
AWMales <- data.frame(age=seq(0,50,length.out=51)) 
AWMales$weight <- predict(vbWM, newdata=AWMales) 
#plot(x=AWMales$age,y=AWMales$weight,ylim=c(50,250),xlim=c(0,50), type="l")

# Fit exponential model for length/weight and predict values
ALWMales <- cbind(ALMales, weight=AWMales$weight)
#Mem <- nls(weight ~ exp(a + b * length), data=ALWMales, start = list(a = 4, b = 0.003))
Mem <- nls(weight ~ (a * length^b), data=ALWMales, start = list(a = 1, b = 1),
           nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024))
LWmalesWS <- coef(Mem)
# a                 b 
# 0.0000005322137 3.578987 
Mmod <- data.frame(length=seq(90,300)) 
Mmod$weight <- predict(Mem,newdata=Mmod) 
#plot(x=ALWMales$length,y=ALWMales$weight,xlim=c(100,300),ylim=c(0,350))
#lines(Mmod, col="blue")
save(LWmalesWS, file="../../RObjects/LWmalesWS.RData")


  # Females

# Predict values from von Bertalanffy model for age/length
load(file="../../RObjects/vbF.RData")
ALFemales <- data.frame(age=seq(0,50,length.out=51)) 
ALFemales$length <- predict(vbF, newdata=ALFemales)
#plot(x=ALFemales$age,y=ALFemales$length,ylim=c(100,300),xlim=c(0,50), type="l")

# Fit von Bertalanffy model for age/weight and predict values
vbWF <- nls(weight ~ Linf*(1-exp(-K*(age-to))), start=parVB, data=TTR.WF)      
AWFemales <- data.frame(age=seq(0,50,length.out=51)) 
AWFemales$weight <- predict(vbWF, newdata=AWFemales) 
#plot(x=AWFemales$age,y=AWFemales$weight,ylim=c(50,250),xlim=c(0,50), type="l")

# Fit exponential model for length/weight and predict values
ALWFemales <- cbind(ALFemales, weight=AWFemales$weight)
#Fem <- nls(weight ~ exp(a + b * length), data=ALWFemales, start = list(a = 4, b = 0.003))
Fem <- nls(weight ~ (a * length^b), data=ALWFemales, start = list(a = -10, b = 3))
LWfemalesWS <- coef(Fem)
# a              b 
# 0.000008040019 3.082574
Fmod <- data.frame(length=seq(100,300)) 
Fmod$weight <- predict(Fem,newdata=Fmod) 
#plot(x=ALWFemales$length, y=ALWFemales$weight, xlim=c(100,300), ylim=c(0,350))
#lines(Fmod, col="red")
save(LWfemalesWS, file="../../RObjects/LWfemalesWS.RData")


  # All

# Joining males and females
ALWMales$sex <- "M"
ALWFemales$sex <- "F"
ALW <- rbind(ALWMales, ALWFemales)

# Fit exponential model for length/weight and predict values
#Aem <- nls(weight ~ exp(a + b * length), data=ALW, start = list(a = 4, b = 0.003))
Aem <- nls(weight ~ (a * length^b), data=ALW, start = list(a = 1, b = 1),
           nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024))
LWallWS <- coef(Aem)
#        a          b 
# 0.0000004981065 3.589617
Amod <- data.frame(length=seq(100,300))
Amod$weight <- predict(Aem,newdata=Amod)  
#plot(x=ALW$length, y=ALW$weight, xlim=c(100,300), ylim=c(0,350))
#lines(Amod, col="green")
save(LWallWS, file="../../RObjects/LWallWS.RData")


############################# 4Gadget #####################################

# File for the model length-weight from Pierce
#load("../../RObjects/LWallPierce.RData")
#len <- seq(100.5, 254.5)
#wei <- LWallPierce[[1]] * len^LWallPierce[[2]]
#data.frame(len, wei)
#len <- seq(100.5, 255.5, by=5)
#wei <- LWallPierce[[1]] * len^LWallPierce[[2]]
#data.frame(len, wei)

# File for the model length-weight from Ridgway and Fenner, 1982
#load("../../RObjects/LWallRF.RData")
#len <- seq(100.5, 254.5)
#wei <- LWallRF[[1]] * len^LWallRF[[2]]
#data.frame(len, wei)
#len <- seq(100.5, 255.5, by=5)
#wei <- LWallRF[[1]] * len^LWallRF[[2]]
#data.frame(len, wei)

# File for the model length-weight from Wells and scott, 1999
#load("../../RObjects/LWallWS.RData")
#len <- seq(100.5, 254.5)
#wei <- LWallWS[[1]] * len^LWallWS[[2]]
#data.frame(len, wei)
#len <- seq(100.5, 255.5, by=5)
#wei <- LWallWS[[1]] * len^LWallWS[[2]]
#data.frame(len, wei)

####################################################################### #



### Plotting ###

Mmod$sex <- "M"
Fmod$sex <- "F"
MFmod <- rbind(Mmod,Fmod)

grl <- ggplot(Amod, aes(x=length, y=weight)) + geom_line() +
          geom_point(data=ALW, aes(x=length, y=weight, colour=factor(sex))) +
          geom_line(data=MFmod, aes(x=length, y=weight, colour=factor(sex))) +
              scale_colour_manual(values=c("orangered", "deepskyblue")) +
          xlim(100,300) + ylim(0,350) +
          labs(title="Bottlenose dolphin length-weight relationship following Wells & Scott, 1999",
          x="weight (kg)", y="length (cm) \n") + mytheme
 
png("../plots/TTR_LengthWeight-Wells&Scott.png", 700, 600)
print(grl)
dev.off()



