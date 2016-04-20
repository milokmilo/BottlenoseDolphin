####################################################################################################
#                    GROWTH (MALES AND FEMALES) - BOTTLENOSE DOLPHIN 
#          von Bertalanffy and Gompertz growth models for males and females
#               following Wells and Scott, 1999 and with our own data 
#             created. C.Saavedra (camilo.saavedra@vi.ieo.es) 10/04/2015
#             modified: C.Saavedra (camilo.saavedra@vi.ieo.es) 18/04/2016
####################################################################################################

# inputs
# "../../RData/TTR_Age-Length_M.WS.txt"
# "../../RData/TTR_Age-Length_F.WS.txt"
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
# Gadget
# "../ttr.catch.lik"
# "../ttr.catches.fleet"

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
write.table(final.df, "../ttr.catch.lik", 
            row.names=FALSE, sep = "\t", na=" ", quote = FALSE)

## Fleet

ttr.catches.fleet <- expand.grid(year=1982:2014, 
                                 step=1:4, area=1, 
                                 fleet="ttr.catches", 
                                 amount=1)

write.table(ttr.catches.fleet, "../ttr.catches.fleet", 
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


# For Males k parameter
vbMale <- nls(length.total~Linf*(1-exp(-K*(age-to))), 
              start = par, data = subset(ttrLASex, ttrLASex$sex == 1))      
coef(vbMale)[[1]] # 0.1276966
Malepred <- data.frame(age = 0.5:35.5)
Malepred$length.total <- predict(vbMale, newdata = Malepred)

# For Females k parameter
vbFemale <- nls(length.total~Linf*(1-exp(-K*(age-to))), 
                start = par, data = subset(ttrLASex, ttrLASex$sex == 2))
coef(vbFemale)[[1]] # 0.1040199
Femalepred <- data.frame(age = 0.5:35.5)
Femalepred$length.total <- predict(vbFemale, newdata = Femalepred)

# Ploting growth models

ttrG <- ggplot(ttrLASex, aes(age, length.total)) + 
            scale_colour_manual(values = c("deepskyblue", "orangered")) +
            geom_point(aes(col=as.factor(sex))) + 
            geom_line(aes(age, length.total), data = Allpred, col = "black") +
            geom_line(data = Malepred, col = "deepskyblue") +
            geom_line(data = Femalepred, col = "orangered") +
            ylab("length (cm) \n") + 
            xlab("age") + xlim(0,35) + ylim(90,350) 

png("../plots/TTR_growth_vb.png", 700, 600)
print(ttrG)
dev.off()


######## Growth model following Wells and Scott, 1999 ########

TTR.M <- read.table("../../RData/TTR_Age-Length_M.WS.txt", comment.char="#", head=T)
names(TTR.M) <- c("age", "length")
range(TTR.M$length)
# [1]  140.7098 282.1363

TTR.F <- read.table("../../RData/TTR_Age-Length_F.WS.txt", comment.char="#", head=T)
names(TTR.F) <- c("age", "length")
range(TTR.F$length)
# [1]  134.0188 267.0540

# Starting values for parameters 
parVB <- list(Linf = 300, K=0.18, to=-3)
parGZ <- list(Linf = 300, K=0.5, to=0.1)

  # Males

vbM <- nls(length ~ Linf*(1-exp(-K*(age-to))), 
           start = parVB, data = TTR.M)      
coef(vbM)
# Linf           K          to 
# 266.1397999   0.1585035  -6.0138646 
gzM <- nls(length ~ Linf*exp(-K*exp(-age*to)), 
           start = parGZ, data = TTR.M)      
coef(gzM)
# Linf           K          to 
# 265.5960615   0.4644284   0.1761629 

  # Females

vbF <- nls(length ~ Linf*(1-exp(-K*(age-to))), 
           start = parVB, data = TTR.F)      
coef(vbF)
# Linf          K         to 
# 248.789280   0.277687  -3.635406 
gzF <- nls(length ~ Linf*exp(-K*exp(-age*to)), 
           start = parGZ, data = TTR.F)      
coef(gzF)
# Linf           K          to 
# 248.6692985   0.4322172   0.3032409 

  # All

TTR.A <- data.frame(age = c(TTR.M$age, TTR.F$age), 
                    length = c(TTR.M$length, TTR.F$length), 
                    Sex = c(rep("Male",length.out = nrow(TTR.M)), 
                          rep("Female",length.out = nrow(TTR.F))))

vbA <- nls(length ~ Linf*(1-exp(-K*(age-to))), 
           start = parVB, data = TTR.A) 
coef(vbA)
# Linf           K          to 
# 254.9330591   0.2227458  -4.4724263  
gzA <- nls(length ~ Linf*exp(-K*exp(-age*to)), 
           start = parGZ, data = TTR.A)      
coef(gzA)
# Linf           K          to 
# 254.7423164   0.4390626   0.2433030 

# Saving data
save(file="../../RObjects/vbA.RData", vbA)
save(file="../../RObjects/vbM.RData", vbM)
save(file="../../RObjects/vbF.RData", vbF)
save(file="../../RObjects/gzA.RData", gzA)
save(file="../../RObjects/gzM.RData", gzM)
save(file="../../RObjects/gzF.RData", gzF)


#### Ploting data with ggplot ####

  # von Bertalanffy

newVB <- data.frame(age = seq(0, 50, length.out = 51)) 
newVB$length <- predict(vbA, newdata = newVB)  

newVB.M <- data.frame(age = seq(0, 50, length.out = 51)) 
newVB.M$length <- predict(vbM, newdata = newVB.M)  

newVB.F <- data.frame(age = seq(0, 50, length.out = 51)) 
newVB.F$length <- predict(vbF, newdata = newVB.F)  

WS_VB <- ggplot(TTR.A, aes(age,length)) + 
            geom_point(aes(colour = factor(Sex))) +
            scale_colour_manual(values = c("#0099cc", "#ff3333")) +
            geom_line(data = newVB, colour = "black") +
            geom_line(data = newVB.M, colour = "#0099cc") +
            geom_line(data = newVB.F, colour = "#ff3333") +
            ggtitle("Von Bertalanffy growth model. Data from Wells & Scott 1999") +
            ylab("length (cm) \n") + 
            xlab("age") + xlim(0,50) + ylim(90,300) 

png("../plots/TTR_vbSex-Wells&Scott.png", 600, 500)
print(WS_VB)
dev.off()

  # Gompertz

newGZ <- data.frame(age = seq(0, 50, length.out = 500)) 
newGZ$length <- predict(gzA, newdata = newGZ)  

newGZ.M <- data.frame(age = seq(0, 50, length.out = 500)) 
newGZ.M$length <- predict(gzM, newdata = newGZ.M)  

newGZ.F <- data.frame(age = seq(0, 50, length.out = 500)) 
newGZ.F$length <- predict(gzF, newdata = newGZ.F)  

WS_G <- ggplot(TTR.A, aes(age,length)) + 
          geom_point(aes(colour = factor(Sex))) +
          scale_colour_manual(values = c("#0099cc", "#ff3333")) +
          geom_line(data = newGZ, colour = "black") +
          geom_line(data = newGZ.M, colour = "deepskyblue") +
          geom_line(data = newGZ.F, colour = "orangered") +
          ggtitle("Gompertz growth model (males and females) data from Wells & Scott 1999") +
          ylab("length (cm) \n") + 
          xlab("age") + xlim(0,50) + ylim(90,300) 
       
png("../plots/TTR_gzSex-Wells&Scott.png", 700, 600)
print(WS_G)
dev.off()


### Predecimos tallas por edad para el archivo de reclutamiento de Gadget ###

load("../../RObjects/vbA.RData")
a <- 0:50
data.frame(age = a,
           len = coef(vbA)[[1]] * 
             (1-exp(-coef(vbA)[[2]] * 
                      (a-(coef(vbA)[[3]])))))



