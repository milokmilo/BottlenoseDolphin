####################################################################################################
#                                       MATURITY
#           
#                  C.Saavedra (camilo.saavedra@vi.ieo.es) 27/09/2013
####################################################################################################

# IMPORTANT: Set working directory (to source file location)

# Charge libraries
library(ggplot2)

# Read data
CEMMA <- read.csv("../../data/RData/CEMMA.csv")
Mat <- CEMMA[CEMMA$sp == "DDE",c("month", "year", "season", "length.real", "length",
          "sex", "cod", "age", "maturity", "reprod.status")]

## Proportion of Matures ~ Age ##

MatAge <- Mat[,c( "sex", "maturity", "age")]
MatAge <- MatAge[complete.cases(MatAge),]
# Changing age values (0.5 and 1.5 for 0 and 1)
MatAge$age[MatAge$age == 0.5] <- 0
MatAge$age[MatAge$age == 1.5] <- 1
unique(ddeMatAge$maturity)

# Calculating percentage of matures #
total <- tapply(MatAge$maturity, list(MatAge$age), length)
total <- as.numeric(total)
matures <- tapply(MatAge$maturity, list(MatAge$age), sum)
matures <- as.numeric(matures)
matProp <- matures/total
matProp <- as.data.frame(matProp[])
matProp <- data.frame(age=c(0,seq(1,(nrow(matProp)-1))), mat=matProp)
plot(matProp$age, matProp$mat, xlim=c(0,29))

## Fitting GLM binomial model ##

maturity <- cbind(matures, total-matures)
glmBinom <- glm(maturity ~ matProp$age, family=binomial)

ages <- seq(0, 29)
#predMat <- predict(glmBinom, list("matProp$age" = ages), type = "response")
#lines(ages, predMat, col=2)
#lines(glmBinom$fitted)

a <- coef(glmBinom)[[1]]
b <- coef(glmBinom)[[2]]
glmFunc <- 1/(1+(exp(-(a+b*ages))))
pred <- data.frame(ages, glmFunc)
lines(pred, col=2)

# Sacando logaritmos despejamos x para calcular la edad a la que y = 50%
y = 0.5
A50 <- (log(y)-log(1-y)-a)/b

########################################
MaturityVector <- pred$glmFunc
round(MaturityVector,2)
A50
########################################

summary(glmBinom)
par(mfcol=c(2,2))
plot(glmBinom)
dev.off()













## Proportion of mature ~ Real length ##

ddeMatLenR <- ddeMat[,c( "sex", "maturity", "length.real")]
ddeMatLenR <- ddeMatLenR[complete.cases(ddeMatLenR),]
#ddeMatLen <- ddeMat[,c( "sex", "maturity", "length")] # Usando length solo ganamos dos muestras
#ddeMatLen <- ddeMatLen[complete.cases(ddeMatLen),]
#max(ddeMatLen$length.real)
#min(ddeMatLen$length.real)
ddeMatLen$rg <- cut(ddeMatLen$length.real,breaks=seq(90,240,by=10)) # Creating ranges

total <- tapply(ddeMatLen$maturity, list(ddeMatLen$sex, ddeMatLen$rg), length) # Count total data by sex and range
matures <- tapply(ddeMatLen$maturity, list(ddeMatLen$sex, ddeMatLen$rg), sum)  # Count matures by sex and range
total[is.na(total)] <- 0 # Remove NAs  
matures[is.na(matures)] <- 0 # Remove NAs

# Males
m <- round((matures[1,]/total[1,]),2) # Proportion of mature males by length class
sum(matures[1,])# total mature males
sum(total[1,]) # total males
# Females
f <- round((matures[2,]/total[2,]),2) # Proportion of mature females by length class 
sum(matures[2,])# total mature females
sum(total[2,]) # total females 
# Data frame
df <- as.data.frame(f) # dataframe with female data
df$m <- m # adding males
len <- substr(rownames(df),2,4)  # construct length vector
df$len <- c(substr(len[1],1,2), len[2:length(len)]) # solve firts length problem
df$len <- as.numeric(df$len)

# Females plot
gML <- ggplot(df, aes(x=len)) + geom_point(aes(x=len,y=f)) + geom_line(aes(x=len,y=f), colour="red") 

dev.off()


