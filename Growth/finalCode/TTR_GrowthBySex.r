####################################################################################################
#                    GROWTH (MALES AND FEMALES) - BOTTLENOSE DOLPHIN 
#          von Bertalanffy and Gompertz growth models for males and females
#               following Wells and Scott, 1999 and with our own data 
#                  C.Saavedra (camilo.saavedra@vi.ieo.es) 10/04/2015
####################################################################################################

# inputs
# "../../RData/TTR-AgeLength.Males.txt"
# "../../RData/TTR-AgeLength.Females.txt"

# outputs
# "../../RObjects/vbA.RData"
# "../../RObjects/vbM.RData"
# "../../RObjects/vbF.RData"
# "../../RObjects/gzA.RData"
# "../../RObjects/gzM.RData"
# "../../RObjects/gzF.RData"
# "../../plots/TTR_vbSex-Wells&Scott.png"
# "../../plots/TTR_gzSex-Wells&Scott.png"


# IMPORTANT: Set working directory (to source file location)
#setwd("./Growth/finalCode")
#setwd("../../Growth/finalCode")

# Charge libraries
library(ggplot2)
mytheme <- theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
                 legend.title= element_blank(), legend.position="top")


######## Growth model following Wells and Scott, 1999 ########

# See files with length/age relationships: "TTR-AgeLength.Males.txt" and "TTR-AgeLength.Females.txt"
TTR.M <- read.table("../../RData/TTR-AgeLength.Males.txt", comment.char="#", head=T)
range(TTR.M$Y)
names(TTR.M) <- c("age", "length")
# [1]  140.7098 282.1363
TTR.F <- read.table("../../RData/TTR-AgeLength.Females.txt", comment.char="#", head=T)
range(TTR.F$Y)
names(TTR.F) <- c("age", "length")
# [1]  134.0188 267.0540


# Starting values for parameters 
parVB <- list(Linf=290, K=0.18, to=-3)
parGZ <- list(Linf=290, K=0.5, to=0.1)

# Males
vbM <- nls(length ~ Linf*(1-exp(-K*(age-to))), start=parVB, data=TTR.M)      
#coef(vbM)
# Linf           K          to 
# 266.1398056   0.1585034  -6.0138670 
gzM <- nls(length ~ Linf*exp(-K*exp(-age*to)), start=parGZ, data=TTR.M)      
#coef(gzM)
# Linf           K          to 
# 265.5960328   0.4644287   0.1761632 

# Females
vbF <- nls(length~Linf*(1-exp(-K*(age-to))), start=parVB, data=TTR.F)      
#coef(vbF)
# Linf           K          to 
# 248.7892779   0.2776871  -3.6354046 
gzF <- nls(length~Linf*exp(-K*exp(-age*to)), start=parGZ, data=TTR.F)      
#coef(gzF)
# Linf           K          to 
# 248.6692910   0.4322174   0.3032412 

# All
TTR.A <- data.frame(age=c(TTR.M$age, TTR.F$age), length=c(TTR.M$length, TTR.F$length), 
                    Sex=c(rep("M",length.out=nrow(TTR.M)), rep("F",length.out=nrow(TTR.F))))

# Forcing t0 in order to get a initial length of arround 100cm 
#to <- 1.1 
vbA <- nls(length~Linf*(1-exp(-K*(age-(-to)))), start=parVB, data=TTR.A) 
#coef(vbA)
# Linf           K          to 
# 254.9330578   0.2227458   4.4724255 

# Forcing t0 in order to get a initial length of arround 100cm 
#to <- 0.8
gzA <- nls(length~Linf*exp(-K*exp(-age*to)), start=parGZ, data=TTR.A)      
#coef(gzA)
# Linf           K          to 
# 254.7423232   0.4390624   0.2433028  


# Saving data
save(file="../../RObjects/vbA.RData", vbA)
save(file="../../RObjects/vbM.RData", vbM)
save(file="../../RObjects/vbF.RData", vbF)
save(file="../../RObjects/gzA.RData", gzA)
save(file="../../RObjects/gzM.RData", gzM)
save(file="../../RObjects/gzF.RData", gzF)


#### Ploting data with ggplot ####

#g0 <- ggplot(TTR.A,aes(age,length)) + geom_point()
#g1 <- g0 + geom_smooth(method="loess") + xlim(0,50) + ylim(90,300) +
#  labs(x="age", y="length \n", title="All bottlenose dolphins - loess \n") +
#  mytheme        # methods= lm, glm, gam, loess, rlm.
#g1

  # All ages von Bertalanffy

newVB <- data.frame(age=seq(0,50,length.out=51)) 
newVB$length <- predict(vbA,newdata=newVB)  # predicting von Bertalanffy growth model
g2 <- g0 + geom_line(data=newVB) + xlim(0,50) + ylim(90,300) +
  labs(x="age", y="length \n", title="All bottlenose dolphins - von Bertalanffy \n") +
  mytheme 
g2

  # All ages Gompertz

newGZ <- data.frame(age=seq(0,50,length.out=51)) 
newGZ$length <- predict(gzA,newdata=newGZ)  # predicting Gompertz growth model 
g3 <- g0 + geom_line(data=newGZ) + xlim(0,50) + ylim(90,300) +
  labs(x="age", y="length \n", title="All bottlenose dolphins - Gompertz \n") +
  mytheme
g3


### Males, Females and All ###

mf1 <- ggplot(TTR.A, aes(age,length)) + geom_point(aes(colour=factor(Sex))) +
  scale_colour_manual(values=c("orangered", "deepskyblue"))
mf1
#mf2 <- mf1 + geom_smooth(method="loess", aes(colour=factor(Sex))) + xlim(0,50) + ylim(90,300) +
#  labs(x="age", y="length \n", title="Bottlenose dolphins (Male and Females) - loess") +
#  mytheme   # methods= lm, glm, gam, loess, rlm.
#mf2

  # with von Bertalanffy

mf2.1 <- mf1 + geom_line(data=newVB, colour="black")
#mf2.1
newVB.M <- data.frame(age=seq(0,50,length.out=51)) 
newVB.M$length <- predict(vbM,newdata=newVB.M)  
mf2.2 <- mf2.1 + geom_line(data=newVB.M, colour="deepskyblue")
#mf2.2
newVB.F <- data.frame(age=seq(0,50,length.out=51)) 
newVB.F$length <- predict(vbF,newdata=newVB.F)  
mf2.3 <- mf2.2 + geom_line(data=newVB.F, colour="orangered")
#mf2.3
mf <- mf2.3 + ylab("length (cm) \n") + xlab("age") + xlim(0,50) + ylim(90,300) + 
  ggtitle("von Bertalanffy growth model (males and females) data from Wells & Scott 1999") +
  mytheme
png("../plots/TTR_vbSex-Wells&Scott.png", 700, 600)
print(mf)
dev.off()


  # with Gompertz

mf2.1 <- mf1 + geom_line(data=newGZ, colour="black")
#mf2.1
newGZ.M <- data.frame(age=seq(0,50,length.out=500)) 
newGZ.M$length <- predict(gzM,newdata=newGZ.M)  
mf2.2 <- mf2.1 + geom_line(data=newGZ.M, colour="deepskyblue")
#mf2.2
newGZ.F <- data.frame(age=seq(0,50,length.out=500)) 
newGZ.F$length <- predict(gzF,newdata=newGZ.F)  
mf2.3 <- mf2.2 + geom_line(data=newGZ.F, colour="orangered")
#mf2.3
mf <- mf2.3 + ylab("length (cm) \n") + xlab("age") + xlim(0,50) + ylim(90,300) +  
  ggtitle("Gompertz growth model (males and females) data from Wells & Scott 1999") +
  mytheme
png("../plots/TTR_gzSex-Wells&Scott.png", 700, 600)
print(mf)
dev.off()


