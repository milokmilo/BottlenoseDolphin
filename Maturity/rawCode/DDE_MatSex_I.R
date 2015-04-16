####################################################################################################
#                               MATURITY SPLITED BY SEX
#           
#                  created: (camilo.saavedra@vi.ieo.es) 01/11/2013
####################################################################################################

# inputs
# "../../RData/CEMMA.csv"

# outputs


# IMPORTANT: Set working directory (to source file location)

# Charge libraries
#library(ggplot2)

# Read data
CEMMA <- read.csv("../../RData/CEMMA.csv")

# HOUSEKEEPING #
matAge <- CEMMA[CEMMA$sp == "DDE", c( "sex", "maturity", "age")]
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

df <- c("matMale", "matFemale", "matFemCorr")

## Computing data. Percentage of mature and maturity matrix ##
for (i in 1:length(df)){
  attach(get(df[i]))
  #ages <- sort(unique(matCorr$age))
  total <- tapply(maturity, list(age), length)
  total <- as.numeric(total)
  mature <- tapply(maturity, list(age), sum)
  mature <- as.numeric(mature)
  matProp <- mature/total
  matProp <- as.data.frame(matProp[])
  assign(df[i], data.frame(age=c(0,seq(1,(nrow(matProp)-1))), 
                           mat=matProp[,1]))
  matureMatrix <- cbind(mature, total-mature)
  ages <- get(df[i])[,1]
  
  ## Fitting GLM binomial model ##
  glmBinom <- glm(matureMatrix ~ ages, family=binomial)
  # For males: algorithm not converge
  a <- coef(glmBinom)[[1]]
  b <- coef(glmBinom)[[2]]
  ages <- seq(0, 29)
  glmFunc <- 1/(1+(exp(-(a+b*ages))))
  assign(paste(df[i],"Pred",sep=""),data.frame(ages, glmFunc))
  summary(glmBinom)
#  par(mfcol=c(2,2))
#  plot(glmBinom)
#  dev.off()
  
  # Taking logarithms to clear x for calculate the age in wich 50% dolphins are mature
  y = 0.5
  assign(paste(df[i],"A50",sep=""), (log(y)-log(1-y)-a)/b)
  
  detach(get(df[i]))
}


## Strings of the Predicted Vectors and A50 ##

A50 <- ls(pattern="A50")
A50 <- rev(A50)
Pred <- ls(pattern="^mat(.*)Pred")

## Plotting Maturity Ogive ##

png("../plots/DDE_MatOgSex.png")
#for (i in 1:length(df)){
  if (i == 1) {plot(get(df[i])[,1],get(df[i])[,2], xlim=c(0,29), col=i+1,
                    ylab="maturity", xlab="age", main="Maturity Ogive (males and females)")
               legend(15,0.5,legend=c("Males","Females"), 
                      lty=c(1,1), col=c("red", "green"))
  } else { points(get(df[i])[,1]+0.2, get(df[i])[,2], cex=1, col=i+1)} # +0.2 para no plotear hembras sobre machos
  lines(get(paste(df[i],"Pred",sep="")), col=i+1)
  for(j in 1:length(A50)){
    text(x=18,y=0.5+0.05*j, 
         labels= paste(substr(A50[j],nchar(A50[j])-2,nchar(A50[j])), "=", round(get(A50[j]),2), 
                       sep=""), cex=0.8, col=j+1)
  }
}
#dev.off()


# For corrected maturity data
MatFemale <- round(get(Pred[1]),2)$glmFunc
save(file="../../data/RObjects/maturity/MatFem.RData", MatFemale)
MatMale <- round(get(Pred[2]),2)$glmFunc
save(file="../../data/RObjects/maturity/MatMale.RData", MatMale)


