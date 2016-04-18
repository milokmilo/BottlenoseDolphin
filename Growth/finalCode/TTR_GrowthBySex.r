####################################################################################################
#                    GROWTH (MALES AND FEMALES) - BOTTLENOSE DOLPHIN 
#          von Bertalanffy and Gompertz growth models for males and females
#               following Wells and Scott, 1999 and with our own data 
#                  C.Saavedra (camilo.saavedra@vi.ieo.es) 10/04/2015
####################################################################################################

# inputs
# "../../RData/TTR-AgeLength.Males.txt"
# "../../RData/TTR-AgeLength.Females.txt"
# "../../RData/CEMMA.csv"

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


######## Growth model from our own data (Fiona's) #########

table <- read.csv("../../RData/CEMMA.csv")

## Data for 8.4.2 Weighted Sum of Squares of Mean Length With Given Standard Deviation

# <year>  <step>  <area>  <age>  <number>  <mean>  <stddev> 

len <- table[table$sp=="TTR" & !is.na(table$length.total),]
min(len$length.total) # 60
max(len$length.total) # 345

ttrLA <- table[table$sp=="TTR" & !is.na(table$age) & !is.na(table$length.total),]
#max(ttrLA$age) # 34
#nrow(ttrLA) #91

ttrLAselect <- ttrLA[,c("year", "quarter", "age", "length.total")]
#ttrLAselect$age <- floor(ttrLAselect$age)
ttrLAselect$age <- ceiling(ttrLAselect$age)

age.num.sd <- merge(with(ttrLAselect, aggregate(length.total ~ age, FUN = mean)),
                    with(ttrLAselect, aggregate(length.total ~ age, FUN = sd)), by="age")
age.num.sd <- merge(with(ttrLAselect, aggregate(length.total ~ age, FUN = length)), 
                    age.num.sd, by="age")
age.num.sd[is.na(age.num.sd[,4]),4] <- mean(age.num.sd[,4], na.rm=T)# include the mean sd when NA

names(age.num.sd) <- c("age", "number", "mean", "stdesv")

#expand.grid(age.num.sd, age=0:35)
#age.num.sd <- age.num.sd[match(c(0:35), age.num.sd$age), ] 
#age.num.sd$age <- 0:35
#age.num.sd[is.na(age.num.sd$number), "number"] <- 0

#age.num.sd <- rbind(age.num.sd[1:22,], # aggregate common dolphin plus group
#                   c(22, sum(age.num.sd[23:28,2], na.rm=T),
#                     mean(age.num.sd[23:28,3]), max(age.num.sd[23:28,4], na.rm=T)))


#### Files for catchstatistics likelihood ###

age.num.sd$age <- paste("age", age.num.sd$age, sep="")

df <- data.frame(year=rep(1982:2014, each=4), 
                 step=rep(1:4, 2014-1982+1),
                 area=rep(1, (2014-1982+1)*4))
df$area <- paste("area", df$area, sep="")

## Amounts

final.df <- merge(df, age.num.sd)
write.table(final.df, "ttr.catch.lik", 
            row.names=FALSE, sep = "\t", na=" ", quote = FALSE)

## Fleet

ttr.catches.fleet <- expand.grid(year=1982:2014, 
                                 step=1:4, area=1, 
                                 fleet="ttr.catches", 
                                 amount=1)

write.table(ttr.catches.fleet, "ttr.catches.fleet", 
            row.names=FALSE, sep = "\t", na=" ", quote = FALSE)


#### Growth models for GADGET ####

ttrLASex <- table[table$sp=="TTR" & 
                    !is.na(table$age) & 
                    !is.na(table$length.total) & 
                    !is.na(table$sex),]  

## Exploratory plot

#ggplot(ttrLASex, aes(age, length.total)) + 
#  geom_point(aes(col=as.factor(sex))) + 
#  geom_smooth() + 
#  geom_smooth(aes(col=as.factor(sex)))


## Fitting growth models

par <- list(K=0.18, to=-5) # Starting values
# Gadget does not use "to" but should be calculated
Linf <- 350

# For Inmatures k parameter
vbAll <- nls(length.total ~ Linf * (1 - exp(-K * (age - to))), 
             start = par, data = ttrLASex,
             lower=c(0,-5), upper=c(1,0), algorithm = "port")      
coef(vbAll)[[1]] # 0.1360274
Allpred <- data.frame(age = 0:35)
Allpred$length.total <- predict(vbAll, newdata = Allpred)
#ggplot(ttrLASex, aes(age, length.total)) + 
#  geom_point(aes(col=as.factor(sex))) + 
#  geom_line(data = Allpred)

# For Males k parameter
vbMale <- nls(length.total~Linf*(1-exp(-K*(age-to))), 
              start = par, data = subset(ttrLASex, ttrLASex$sex == 1))      
coef(vbMale)[[1]] # 0.1276966
Malepred <- data.frame(age = 0.5:35.5)
Malepred$length.total <- predict(vbMale, newdata = Malepred)
#ggplot(ttrLASex, aes(age, length.total)) + 
#  geom_point(aes(col=as.factor(sex))) + 
#  geom_line(data = Malepred, col = "blue")

# For Females k parameter
vbFemale <- nls(length.total~Linf*(1-exp(-K*(age-to))), 
                start = par, data = subset(ttrLASex, ttrLASex$sex == 2))
coef(vbFemale)[[1]] # 0.1040199
Femalepred <- data.frame(age = 0.5:35.5)
Femalepred$length.total <- predict(vbFemale, newdata = Femalepred)
#ggplot(ttrLASex, aes(age, length.total)) + 
#  geom_point(aes(col=as.factor(sex))) + 
#  geom_line(data = Femalepred, col = "red")



######## Growth model following Wells and Scott, 1999 ########

# See files with length/age relationships: "TTR-AgeLength.Males.txt" and "TTR-AgeLength.Females.txt"
#TTR.M <- read.table("../../RData/TTR-AgeLength.Males.txt", comment.char="#", head=T)
#range(TTR.M$Y)
#names(TTR.M) <- c("age", "length")
# [1]  140.7098 282.1363
#TTR.F <- read.table("../../RData/TTR-AgeLength.Females.txt", comment.char="#", head=T)
#range(TTR.F$Y)
#names(TTR.F) <- c("age", "length")
# [1]  134.0188 267.0540

# Starting values for parameters 
#parVB <- list(Linf=290, K=0.18, to=-3)
#parGZ <- list(Linf=290, K=0.5, to=0.1)

# Males
#vbM <- nls(length ~ Linf*(1-exp(-K*(age-to))), start=parVB, data=TTR.M)      
#coef(vbM)
# Linf           K          to 
# 266.1398056   0.1585034  -6.0138670 
#gzM <- nls(length ~ Linf*exp(-K*exp(-age*to)), start=parGZ, data=TTR.M)      
#coef(gzM)
# Linf           K          to 
# 265.5960328   0.4644287   0.1761632 

# Females
#vbF <- nls(length~Linf*(1-exp(-K*(age-to))), start=parVB, data=TTR.F)      
#coef(vbF)
# Linf           K          to 
# 248.7892779   0.2776871  -3.6354046 
#gzF <- nls(length~Linf*exp(-K*exp(-age*to)), start=parGZ, data=TTR.F)      
#coef(gzF)
# Linf           K          to 
# 248.6692910   0.4322174   0.3032412 

# All
#TTR.A <- data.frame(age=c(TTR.M$age, TTR.F$age), 
#                    length=c(TTR.M$length, TTR.F$length), 
#                    Sex=c(rep("Male",length.out=nrow(TTR.M)), 
#                          rep("Female",length.out=nrow(TTR.F))))

# Forcing t0 in order to get a initial length of arround 100cm 
#to <- -1.1 
#vbA <- nls(length~Linf*(1-exp(-K*(age-to))), start=parVB, data=TTR.A) 
#coef(vbA)
# Linf           K          to 
# 254.9330578   0.2227458   -4.4724255 

# Forcing t0 in order to get a initial length of arround 100cm 
#to <- 0.8
#gzA <- nls(length~Linf*exp(-K*exp(-age*to)), start=parGZ, data=TTR.A)      
#coef(gzA)
# Linf           K          to 
# 254.7423232   0.4390624   0.2433028  

# Saving data
#save(file="../../RObjects/vbA.RData", vbA)
#save(file="../../RObjects/vbM.RData", vbM)
#save(file="../../RObjects/vbF.RData", vbF)
#save(file="../../RObjects/gzA.RData", gzA)
#save(file="../../RObjects/gzM.RData", gzM)
#save(file="../../RObjects/gzF.RData", gzF)


#### Ploting data with ggplot ####

#g0 <- ggplot(TTR.A,aes(age,length)) + geom_point()
#g1 <- g0 + geom_smooth(method="loess") + xlim(0,50) + ylim(90,300) +
  #  labs(x="age", y="length \n", title="All bottlenose dolphins - loess \n") +
  #  mytheme        # methods= lm, glm, gam, loess, rlm.
#g1

  # All ages von Bertalanffy

#newVB <- data.frame(age=seq(0,50,length.out=51)) 
#newVB$length <- predict(vbA,newdata=newVB)  # predicting von Bertalanffy growth model
#g2 <- g0 + geom_line(data=newVB) + xlim(0,50) + ylim(90,300) +
  #  labs(x="age", y="length \n", title="All bottlenose dolphins - von Bertalanffy \n") +
  #  mytheme 
#g2

  # All ages Gompertz

#newGZ <- data.frame(age=seq(0,50,length.out=51)) 
#newGZ$length <- predict(gzA,newdata=newGZ)  # predicting Gompertz growth model 
#g3 <- g0 + geom_line(data=newGZ) + xlim(0,50) + ylim(90,300) +
  #  labs(x="age", y="length \n", title="All bottlenose dolphins - Gompertz \n") +
  #  mytheme
#g3


### Males, Females and All ###

#mf1 <- ggplot(TTR.A, aes(age,length)) + geom_point(aes(colour=factor(Sex))) 
#  scale_colour_manual(values=c("#0099cc", "#ff3333"))
#mf1
#mf2 <- mf1 + geom_smooth(method="loess", aes(colour=factor(Sex))) + xlim(0,50) + ylim(90,300) +
  #  labs(x="age", y="length \n", title="Bottlenose dolphins (Male and Females) - loess") +
  #  mytheme   # methods= lm, glm, gam, loess, rlm.
#mf2

  # with von Bertalanffy

#mf2.1 <- mf1 + geom_line(data=newVB, colour="black")
#mf2.1
#newVB.M <- data.frame(age=seq(0,50,length.out=51)) 
#newVB.M$length <- predict(vbM,newdata=newVB.M)  
#mf2.2 <- mf2.1 + geom_line(data=newVB.M, colour="#0099cc")
#mf2.2
#newVB.F <- data.frame(age=seq(0,50,length.out=51)) 
#newVB.F$length <- predict(vbF,newdata=newVB.F)  
#mf2.3 <- mf2.2 + geom_line(data=newVB.F, colour="#ff3333")
#mf2.3
#mf <- mf2.3 + ylab("length (cm) \n") + xlab("age") + xlim(0,50) + ylim(90,300) + 
  #  ggtitle("Von Bertalanffy growth model. Data from Wells & Scott 1999") +
  #  mytheme
#png("../plots/TTR_vbSex-Wells&Scott.png", 600, 500)
#print(mf)
#dev.off()


  # with Gompertz

#mf2.1 <- mf1 + geom_line(data=newGZ, colour="black")
#mf2.1
#newGZ.M <- data.frame(age=seq(0,50,length.out=500)) 
#newGZ.M$length <- predict(gzM,newdata=newGZ.M)  
#mf2.2 <- mf2.1 + geom_line(data=newGZ.M, colour="deepskyblue")
#mf2.2
#newGZ.F <- data.frame(age=seq(0,50,length.out=500)) 
#newGZ.F$length <- predict(gzF,newdata=newGZ.F)  
#mf2.3 <- mf2.2 + geom_line(data=newGZ.F, colour="orangered")
#mf2.3
#mf <- mf2.3 + ylab("length (cm) \n") + xlab("age") + xlim(0,50) + ylim(90,300) +  
  #  ggtitle("Gompertz growth model (males and females) data from Wells & Scott 1999") +
  #  mytheme
#png("../plots/TTR_gzSex-Wells&Scott.png", 700, 600)
#print(mf)
#dev.off()


### Predecimos el modelo para todos los delfines pero fijando linf como 270 ###

#parVB <- list(K=0.18, to=-3) 
#Linf <- 270
# Growth model  with the parameters estimated with non linear least squares
#vbAGadget <- nls(length~Linf*(1-exp(-K*(age-to))), start=parVB, data=TTR.A)  
# Saving data50
#save(file="../../RObjects/vbAGadget.RData", vbAGadget)

#plot(TTR.A$age, TTR.A$length)
#a <- TTR.A$age
#modAllGad <-Linf*(1-exp(-coef(vbAGadget)[[1]]*(a-(coef(vbAGadget)[[2]]))))
#lines(spline(TTR.A$age, modAllGad), col="red")
#modA <- coef(vbA)[[1]]*(1-exp(-coef(vbA)[[2]]*(a-(coef(vbA)[[3]]))))
#lines(spline(TTR.A$age, modA), col="blue")


### Predecimos tallas por edad para el archivo de reclutamiento de Gadget ###

#load("../../RObjects/vbA.RData")
#a <- 0:35
#data.frame(age=a,len=coef(vbA)[[1]]*(1-exp(-coef(vbA)[[2]]*(a-(coef(vbA)[[3]])))))



