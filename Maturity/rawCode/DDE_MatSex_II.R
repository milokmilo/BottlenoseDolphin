####################################################################################################
#                               MATURITY SPLITED BY SEX
#     Three datasets fitting (males, females, females with old immature females removed)    
#                    Bootstrap to calculate the 95% CL of the A50 
#                  created: (camilo.saavedra@vi.ieo.es) 01/11/2013
#                  modified: (camilo.saavedra@vi.ieo.es) 13/11/2013
####################################################################################################

# inputs
# "../../RData/CEMMA.csv"

# outputs
# "../../RObjects/maturity/MatFema.RData"
# "../../RObjects/maturity/MatFemC.RData"
# "../../RObjects/maturity/MatMale.RData"
# "../plots/DDE_MatSex.png"

# IMPORTANT: Set working directory (to source file location)

# Charge libraries
#library(ggplot2)

# Read data
CEMMA <- read.csv("../../RData/CEMMA.csv")

# HOUSE KEEPING #
matAge <- CEMMA[CEMMA$sp == "DDE", c("sex", "maturity", "age")]
matAge <- matAge[complete.cases(matAge),]
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
  range <- seq(0,29,0.1) # this could be any substantively meaningful range
  p <- predict(glm.out, newdata = data.frame(age=range), "response")
  assign(paste(df[i],"Pred",sep=""),data.frame(age=range,mat=p))
  assign(paste(df[i],"A50",sep=""), range[match(TRUE,p>.5)]) # predicted probability of 50% maturity
}


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
A50 <- rev(A50)
Pred <- ls(pattern="^mat(.*)Pred")
dfPer <-  ls(pattern="^Per(.*)")
dfPer <- rev(dfPer)

png("../plots/DDE_MatSex.png")
for (i in 1:length(dfPer)){
  if (i == 1) {plot(get(dfPer[i])[,1],get(dfPer[i])[,2], xlim=c(0,29), col=2,
                    ylab="maturity", xlab="age", main="Maturity Ogive (males and females)")
               legend(14,0.3,legend=c("Males","Females (3 removed)", "Females"), 
                      lty=c(1,1), col=c("red", "green", "blue"), cex=0.8)
  } else { points(get(dfPer[i])[,1]+i/5-0.1, get(dfPer[i])[,2], cex=1, col=i+1)} # +0.2 para no plotear hembras sobre machos
  lines(get(paste(df[i],"Pred",sep="")), col=i+1)
  for(j in 1:length(A50)){
    text(x=18,y=0.6-0.05*j, 
         labels= paste(substr(A50[j],nchar(A50[j])-2,nchar(A50[j])), 
                       "=", round(get(A50[j]),2), sep=""), cex=0.8, col=j+1)
  }
}
dev.off()

# Sex maturity data
ag <- 0:29
for (i in 1:length(Pred)){
  Mat <- subset(get(Pred[i]), age %in% ag, select=mat)
  Mat <- Mat[,1]
  save(file=paste("../../RObjects/maturity/", substr(Pred[i],1,7), ".RData", sep=""), Mat)
}

### BOOTSTRAP MATURITY OGIVE, A50 AND CI ###

# NOTE that is done only for females 

## For the Maturity Ogive ##
#myMatBoot <- function(){
#  srows <- sample(1:nrow(matFemCorr),nrow(matFemCorr),TRUE)
#  glm.out <- glm(maturity ~ age, family=binomial(link=logit), data=matFemCorr[srows,])
#  range <- seq(0,29,0.1) # this could be any substantively meaningful range
#  return(predict(glm.out, newdata = data.frame(age=range), "response"))
#}
#nboot <- 1000
#preddist <- replicate(nboot, myMatBoot()) # your distribution
#ages <- (as.numeric(rownames(preddist))-1)/10
#plot(ages,rep(1,length(ages)) ,ylim=c(0,1), type="n", axes=T, 
#     xlab="age", ylab="maturity percentage", 
#     main="Bootstrap -Maturity Ogive")
#for (i in 1:nboot){
#  lines(x=ages, y=preddist[,i], col=i+1)
#}

## A only vector as a mean of the bootstrap
#ag <- 0:29
#dfBoot <- apply(preddist,1,mean)
#dfBoot <- data.frame(mat=dfBoot)
#dfBoot$age <- (as.numeric(rownames(dfBoot))-1)/10 
#matBoot <- subset(dfBoot, age %in% ag)

## Comparing estimates
#plot(dfBoot$age, dfBoot$mat) # 300 values bootstrap
#lines(matBoot$age, matBoot$mat, col="red") # 30 values bootstrap
#lines(ag, Mat$mat, col="blue") # 30 values NO bootstrap (asegurarse que Mat es el de hembras Corr)



## For the A50 and CI ##
myA50Boot <- function(){
  srows <- sample(1:nrow(matFemCorr),nrow(matFemCorr),TRUE)
  glm.out <- glm(maturity ~ age, family=binomial(link=logit), data=matFemCorr[srows,])
  range <- seq(0,29,0.1) # this could be any substantively meaningful range
  p <- predict(glm.out, newdata = data.frame(age=range), "response")
  return(range[match(TRUE,p>.5)]) # predicted probability of 50% maturity
}
nboot <- 1000
bootdist <- replicate(nboot, myA50Boot()) # your distribution
quantile(unlist(bootdist),c(.025,.5,.975)) # 95% CI



