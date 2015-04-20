####################################################################################################
#                    ENERGY MODELS ANALYSIS - BOTTLENOSE DOLPHIN DIET
#                      Plotting all energy model and an average model
#                          Plot of the length weight distribution
#                    created: (camilo.saavedra@vi.ieo.es) 04/12/2013
####################################################################################################

# inputs
# "../../RObjects/DDE_AllDietWeight.RData"
# "../../RData/KjCDGal.csv"
# "../../RData/KjCDPt.csv"

# outputs
# "../plots/DDE_EnergMod.png"
# "../plots/DDE_EnergMod0.png"
# "../plots/DDE_LenWeiDistr.png"

# IMPORTANT: Set working directory (to source file location)

# Charge libraries
library(ggplot2)
library(reshape)
library(ggthemes)
library(boot)
library(grid)

# Read data
load("../../RObjects/DDE_AllDietWeight.RData")

### Data information ###

# Years range
s <- substr(dde$Date, 7,10)
s <- as.numeric(s)
range(s, na.rm=T)
# Number of dolphins analysed
length(unique(dde$No)) # total analised
length(unique(dde[!is.na(dde$Length),"No"])) # analised with length data
length(unique(dde[!is.na(dde$Sex),"No"])) # analised with sex data
# Ranges
range(dde$Length, na.rm=T) # length range
range(dde$W, na.rm=T) # weight range
avDolph <- mean(dde$W, na.rm=T); avDolph # mean weight (average dolphin)

### ggPlotting length dolphins distribution ###

wei <- tapply(dde$W, dde$No, mean)
wei <- round(wei)
weiFreq <- table(wei)
weiFreq <- as.data.frame(weiFreq)
weiFreq$wei <- as.numeric(as.character(weiFreq$wei))
weiFreqRg <- data.frame(weiFreq, 
                        rg=cut(weiFreq$wei, breaks=seq(20,140,5)))
ggData1 <- tapply(weiFreqRg$Freq, weiFreqRg$rg, sum)
ggData1 <- data.frame(Freq= ggData1[], rg=names(ggData1))
val <- gsub("^\\(", "", ggData1$rg)
val <- gsub(",+[0-9]+\\]","", val)
ggData1$val <- as.numeric(val)+2.5

gg1 <- ggplot(ggData1, aes(val, Freq)) + geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(20,140,5), labels=seq(20,140,5)) +
  ylab("frequency") + xlab("weight (kg)") + 
  ggtitle("common dolphin - estimated weight distribution") + 
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top")

len <- tapply(dde$Length, dde$No, mean)
len <- round(len)
lenFreq <- table(len)
lenFreq <- as.data.frame(lenFreq)
lenFreq$len <- as.numeric(as.character(lenFreq$len))
lenFreqRg <- data.frame(lenFreq, 
                        rg=cut(lenFreq$len, breaks=seq(120,240,5)))
ggData2 <- tapply(lenFreqRg$Freq, lenFreqRg$rg, sum)
ggData2 <- data.frame(Freq= ggData2[], rg=names(ggData2))
val <- gsub("^\\(", "", ggData2$rg)
val <- gsub(",+[0-9]+\\]","", val)
ggData2$val <- as.numeric(val)+2.5

gg2 <- ggplot(ggData2, aes(val, Freq)) + geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(120,240,5), labels=seq(120,240,5)) +
  ylab("frequency") + xlab("length (cm)") + 
  ggtitle("common dolphin - length distribution") + 
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top")

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_LenWeiDistr.png", width=600, height=400)
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1))) # 5 rows, 1 column
print(gg1, vp = vplayout(1, 1))
print(gg2, vp = vplayout(2, 1))
dev.off()
#```


############################ Energy Indices ########################################

df <- data.frame(w=wei[!is.na(wei)])

# number of dolphins with measured length
nrow(df)

##### Kastelein et al. (2012) #####

# Kastelein linear increase between 60 and 100kg
y = c(0.12,0.06)
x = c(60,100)
Kaslm <- lm (y ~ x )
a <- coef(Kaslm)[[1]]
b <- coef(Kaslm)[[2]]

# Daily food intake estimation
for ( i in 1:nrow(df)){
if (df$w[i] <=60){
  df$Kas[i] <- df$w[i] * 0.12   
  } else {
    if (60 < df$w[i] & df$w[i] < 100) {
     df$Kas[i] <- (a+b*df$w[i]) * df$w[i] 
    #df$w[i]*(0.06+((100-df$w[i])/100)*0.06) # miscalculation - original paper
     0.12-(100-df$w[i])*0.06
    } else {
      if (df$w[i] >= 100) {
        df$Kas[i] <- df$w[i] * 0.06     
      } else {
        stop("something wrong")
      }
    } 
  }
}

# Bootstrap for an average dolphin (for the mean) with Kastelein index
meanFuncKast <- function(data, d){
  sum(na.omit(data[d])/length(na.omit(data[d])))
}
bootKas  <- boot(data=df$Kas, meanFuncKast, R=1000)
bootCIKas  <- boot.ci(bootKas, type=c("perc")) 


##### Innes et al. (1987) #####

# Daily food intake estimation
df$In <- 0.258 * (df$w)^0.69

# Bootstrap for an average dolphin (for the mean) with Innes index
meanFuncInnes <- function(data, d){
  sum(na.omit(data[d]))/length(na.omit(data[d]))
}
bootIn <- boot(data=df$In, meanFuncInnes, R=1000)
bootCIIn <- boot.ci(bootIn, type=c("perc"))



##### Kleiber (1947) #####

# Daily food intake estimation
KjGal <- read.csv("../../RData/KjCDGal.csv")
KjPt <- read.csv("../../RData/KjCDPt.csv")

KjKgCDGal <- sum(KjGal[,4] * KjGal[,5] * 10)
KjKgCDPt <- sum(KjPt[,4] * KjPt[,5] * 10)
df$KleG <-  (293*df$w^0.75)/KjKgCDGal/0.925*4
df$KleP <-  (293*df$w^0.75)/KjKgCDPt/0.925*4

# Bootstrap for an average dolphin (for the mean) with kleiber index
meanFuncKleiber <- function(data, d){
  sum(na.omit(data[d]))/length(na.omit(data[d]))
}
bootInG <- boot(data=df$KleG, meanFuncKleiber, R=1000)
bootCIInG <- boot.ci(bootInG, type=c("perc"))
bootInP <- boot(data=df$KleP, meanFuncKleiber, R=1000)
bootCIInP <- boot.ci(bootInP, type=c("perc"))


### Fitted a new model with all indices for Galicia and Portugal ###

allEnerg <- melt(df, id.vars="w")
energMod <- lm(allEnerg$value ~ allEnerg$w)

# Forcing the intercept to be 0
#lm(y ~ x -1)
#lm(y ~ 0 + x)
energMod0 <- lm(allEnerg$value ~ allEnerg$w - 1)
#energMod0 <- lm(allEnerg$value ~ 0 + allEnerg$w) # same result

#############################
A <- coef(energMod)[[1]]  ###
B <- coef(energMod)[[2]]  ###
consEst <- A + B * df$w   ###
#############################
df$mod <- consEst
#############################
A <- coef(energMod0)[[1]] ###
consEst0 <- A * df$w      ###
#############################
df$mod0 <- consEst0

# Bootstrap for an average dolphin (for the mean) with the new model
meanFuncNewMod <- function(data, d){
  sum(na.omit(data[d]))/length(na.omit(data[d]))
}
bootNewMod <- boot(data=df$mod, meanFuncNewMod, R=1000)
bootCINewMod <- boot.ci(bootNewMod, type=c("perc"))

# Kg of food per kg of dolphin consumed daily
bootNewMod[[1]] / avDolph
bootCINewMod[[4]][[4]] / avDolph
bootCINewMod[[4]][[5]] / avDolph

A + B * 70.97149 
### ggPloting ###

ggData <- melt(df, id.vars="w")

dat1 <- ggData[!(ggData$variable=="mod" | ggData$variable=="mod0"),]

gg1 <- ggplot(dat1, aes(w, value, group=variable)) + 
        geom_point(aes(colour=variable), pch=1) +
        ggtitle("Energy models and new fitted model") + 
        scale_x_continuous(breaks=seq(20,140,20)) +
        scale_y_continuous(breaks=seq(0,10,2), limits=c(0,10.5)) +
        ylab("daily food intake (kg)") + xlab("dolphin weight (kg)") +
        theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
              legend.title=element_blank(), legend.position="top")

dat2 <- ggData[ggData$variable=="mod",]
dat3 <- ggData[ggData$variable=="mod0",]

gg2 <- gg1 + geom_line(data=dat2, aes(w, value), cex=1)
gg3 <- gg2 + geom_line(data=dat3, aes(w, value), cex=1, lty=2)

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_EnergMod.png", width=600, height=400)
print(gg2)
dev.off()
#```

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_EnergMod0.png", width=600, height=400)
print(gg3)
dev.off()
#```

#########################################################################

# Only Galicia

energModG <- lm(allEnerg[!allEnerg$variable=="KleP","value"] ~ allEnerg[!allEnerg$variable=="KleP","w"])
A <- coef(energModG)[[1]]
B <- coef(energModG)[[2]]

consEstG <- A + B * df$w

#lines(df$w, consEstG)
mean(consEstG)

# Only Portugal

energModP <- lm(allEnerg[!allEnerg$variable=="KleG","value"] ~ allEnerg[!allEnerg$variable=="KleG","w"])
A <- coef(energModP)[[1]]
B <- coef(energModP)[[2]]

consEstP <- A + B * df$w

#lines(df$w, consEstP)
mean(consEst)






