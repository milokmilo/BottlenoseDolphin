################################################################################################## #
#                    MATURITY SPLITED BY SEX FOR BOTTLENOSE DOLPHIN
#     Three datasets fitted (males, females, females with old immature females removed)    
#                    Bootstrap to calculate the 95% CL of the A50 
#                  created: (camilo.saavedra@vi.ieo.es) 20/04/2015
################################################################################################## #


# FALTAN DATOS OBSERVADOS DE EDAD Y MADUREZ


# inputs

# outputs
# "../../RObjects/TTR_Mat-Wells.RData"

################################################################################################## #


# IMPORTANT: Set working directory (to source file location)
#setwd(".Maturity/finalCode")
#setwd("../../Maturity/finalCode")


# Charge libraries
library(reshape)
library(ggplot2)
mytheme <-   theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
                   legend.title=element_blank(), legend.position="top")


# Age at sexual maturation from Wells et al., 1987

age <- seq(0,50)
matMale <- c(0,0,0,0,0,0,0,0,NA,NA,NA,NA,rep(1,39))
matFemale <- c(0,0,0,0,0,NA,NA,NA,NA,NA,rep(1,41))
Mat <- data.frame(age=age, matMale=matMale, matFemale=matFemale)  
  

glm.male <- glm(matMale ~ age, family="binomial", data=Mat)
p.m <- predict(glm.male, newdata = data.frame(age=seq(0,49,0.1)), "response")
p.male <- data.frame(age=seq(0,49,0.1), matPred=round(p.m,2))

glm.female <- glm(matFemale ~ age, family="binomial", data=Mat)
p.f <- predict(glm.female, newdata = data.frame(age=seq(0,49,0.1)), "response")
p.female <- data.frame(age=seq(0,49,0.1), matPred=round(p.f,2))

m50 <- p.male$age[match(TRUE,p.m>.5)]
f50 <- p.female$age[match(TRUE,p.f>.5)]

# Strings of the Predicted Vectors and A50
A50 <- ls(pattern="(.+)50")



## ggplot ##

MatM <- data.frame(Mat[,-3], sex="Male") 
MatF <- data.frame(Mat[,-2], sex="Female")
names(MatM) <- c("age", "mat", "sex")
names(MatF) <- c("age", "mat", "sex")  
gMat <- rbind(MatM, MatF)

p.male$sex <- "Male"
p.female$sex <- "Female"

ggMat <- rbind(p.male, p.female)


gg1 <- ggplot(gMat, aes(age, mat, colour=sex)) + 
  geom_point() + geom_line(aes(age, matPred, group=sex), data=ggMat) +
  ylab("percentage of maturity \n") + xlab("age") +
  ggtitle("Maturity Ogive for Bottlenose dolphin following Wells et al.,1987") +
  mytheme + 
  annotate("text", x=25, y=0.56, label=paste("Male A50 =", m50, sep=""), col="deepskyblue") + 
  annotate("text", x=25, y=0.5, label=paste("Female A50 =", f50, sep=""), col="orangered")


png("../plots/TTR_MatSex.png", width=600, height=500)
print(gg1)
dev.off()



# Save maturity data
save(ggMat, file="../../RObjects/TTR_Mat-Wells.RData")


### BOOTSTRAP MATURITY OGIVE, A50 AND CI ###

myA50Boot <- function(data){
  srows <- sample(1:nrow(data),nrow(data),TRUE)
  glm.out <- glm(mat ~ age, family=binomial(link=logit), data=data[srows,])
  range <- seq(0,29,0.1) # this must be any substantively meaningful range
  p <- predict(glm.out, newdata = data.frame(age=range), "response")
  return(range[match(TRUE,p>.5)]) # predicted probability of 50% maturity
}
nboot <- 1000

## CI for A50 Males ##
bootdist <- replicate(nboot, myA50Boot(data=MatM)) # your distribution
quantile(unlist(bootdist),c(.025,.5,.975)) # 95% CI
# 2.5%   50% 97.5% 
# 8.1   9.6  11.1 

## CI for A50 Females ##
bootdist <- replicate(nboot, myA50Boot(data=MatF)) # your distribution
quantile(unlist(bootdist),c(.025,.5,.975)) # 95% CI
# 2.5%   50% 97.5% 
# 5.6   7.1   8.6 







####################################################################################################











# inputs
# "../../RData/CEMMA.csv"

# outputs
# "../../RObjects/TTR_MatFema.RData"
# "../../RObjects/TTR_MatFemC.RData"
# "../../RObjects/TTR_MatMale.RData"
# "../plots/TTR_MatSex.png"




# Read data
CEMMA <- read.csv("../../RData/CEMMA.csv")

# HOUSE KEEPING #
matAge <- CEMMA[CEMMA$sp == "TTR", c("sex", "maturity", "age")]
c <- matAge[complete.cases(matAge),]
# Changing age values (0.5 and 1.5 for 0 and 1)
matAge$age[matAge$age == 0.5] <- 0
matAge$age[matAge$age == 1.5] <- 1

# Computing 3 datasets (males, females and females with old immature removed) 
matMale <- matAge[matAge$sex == 1,c("maturity", "age")]
matFemale <- matAge[matAge$sex == 2,c("maturity", "age")]
# Removing immature dolfins older than 10 years
matFemCorr <- matFemale[!(matFemale$age>=11 & matFemale$maturity==0),] # matriz corregida (eliminamos hembras inmaduras de más de 10 años), asumimos que son animales enfermos con fallo reproductivo


## MATUATION OGIVE ~ AGE (mature proportion) ##

df <- c("matMale", "matFemCorr", "matFemale")

# Frequencies 
apply(table(matMale),1,sum)
apply(table(matFemCorr),1,sum)
apply(table(matFemale),1,sum)

### Fitting Bionomial GLM model and predicting A50 ###

for (i in 1:length(df)){
  glm.out <- glm(maturity ~ age, family="binomial", data=get(df[i]))
  range <-seq(0,29,0.1) # this could be any substantively meaningful range
  p <- predict(glm.out, newdata = data.frame(age=range), "response")
  assign(paste(df[i],"Pred",sep=""),data.frame(age=range,matP=p))
  assign(paste(df[i],"A50",sep=""), range[match(TRUE,p>.5)]) # predicted probability of 50% maturity
}
ls(pattern="Pred")

### Percentage of mature-at-age ###

for (i in 1:length(df)){
  attach(get(df[i]))
  total <- tapply(maturity, list(age), length)
  total <- as.data.frame(total)
  total$age <- as.numeric(rownames(total))
  mature <- tapply(maturity, list(age), sum)
  mature <- as.data.frame(mature)
  mature$age <- as.numeric(rownames(mature))
  matrix <- merge(mature,total)
  assign(paste("Per", df[i], sep=""), 
         data.frame(age=matrix$age, mat=matrix$mature/matrix$total))
  detach(get(df[i]))
}


## Plotting Maturity Ogive ##

# Strings of the Predicted Vectors and A50
A50 <- ls(pattern="(.+)A50")
#A50 <- rev(A50)
Pred <- ls(pattern="^mat(.*)Pred")
#Pred <- rev(Pred)
dfPer <-  ls(pattern="^Per(.*)")
#dfPer <- rev(dfPer)

### ggPlotting ###

# Computing data
ages<- data.frame(age=seq(0,29,0.1))
ggMat <- data.frame(age=NULL, mat=NULL, sex=NULL)
for (i in 1:length(dfPer)){
  assign(dfPer[i], merge(get(dfPer[i]), ages, by="age", all=T))
  assign(dfPer[i], `[[<-`(get(dfPer[i]), "sex", value=dfPer[i])) 
  assign(dfPer[i], merge(get(dfPer[i]), get(Pred[i]), by="age", all=T))
  ggMat <- rbind(ggMat, get(dfPer[i]))
}

# ggplot

ggMat
gg1 <- ggplot(ggMat, aes(age, mat, group=sex, colour=sex)) + 
  geom_point() + geom_line(aes(age, matP, group=sex)) +
  ylab("percentage of maturity") + xlab("age") +
  ggtitle("Maturity Ogive (males and females)") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top")


for(j in 1:length(A50)){
assign(paste("gg",j+1,sep=""), get(paste("gg",j,sep="")) + 
        annotate("text", x=18, y=0.6-0.05*j, 
        label = paste(substr(A50[j],nchar(A50[j])-2,nchar(A50[j])), 
              "=", round(get(A50[j]),2), sep=""), size=4, colour=j+1))
}


png("../plots/DDE_MatSex.png", width=600, height=400)
print(get(paste("gg", length(A50)+1, sep="")))
dev.off()

# Sex maturity data
ag <- 0:29
for (i in 1:length(Pred)){
  Mat <- subset(get(Pred[i]), age %in% ag, select=matP)
  Mat <- Mat[,1]
  save(file=paste("../../RObjects/", substr(Pred[i],1,7), ".RData", sep=""), Mat)
}

### BOOTSTRAP MATURITY OGIVE, A50 AND CI ###

myA50Boot <- function(data){
  srows <- sample(1:nrow(data),nrow(data),TRUE)
  glm.out <- glm(maturity ~ age, family=binomial(link=logit), data=data[srows,])
  range <- seq(0,29,0.1) # this must be any substantively meaningful range
  p <- predict(glm.out, newdata = data.frame(age=range), "response")
  return(range[match(TRUE,p>.5)]) # predicted probability of 50% maturity
}
nboot <- 1000

## CI for A50 Males ##
bootdist <- replicate(nboot, myA50Boot(data=matMale)) # your distribution
quantile(unlist(bootdist),c(.025,.5,.975)) # 95% CI

## CI for A50 Females ##
bootdist <- replicate(nboot, myA50Boot(data=matFemCorr)) # your distribution
quantile(unlist(bootdist),c(.025,.5,.975)) # 95% CI
