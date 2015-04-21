################################################################################################## #
#                    ENERGY MODELS ANALYSIS - BOTTLENOSE DOLPHIN DIET
#                      Plotting all energy model and an average model
#                          Plot of the length weight distribution
#                    created: (camilo.saavedra@vi.ieo.es) 21/04/2015
################################################################################################## #

# inputs
# "../../RObjects/TTR_AllDietWeight.RData"
# "../../RData/KjBDGal.csv"
# "../../RData/KjBDPt.csv"
# outputs
# "../plots/TTR_EnergMod.png"
# "../plots/TTR_EnergMod0.png"
# "../plots/TTR_LenWeiDistr.png"

################################################################################################## #


# IMPORTANT: Set working directory (to source file location)
#setwd("./Diet/finalCode")
#setwd("../../Diet/finalCode")


# Charge libraries
library(ggplot2)
library(reshape)
library(boot)
library(grid)
mytheme <-  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
                  legend.title=element_blank(), legend.position="top")


# Read data
load("../../RObjects/TTR_AllDietWeight.RData")


### Data information ###

# Years range
s <- substr(ttrDiet$Date, 7,10)
s <- as.numeric(s)
range(s, na.rm=T)
# Number of dolphins analysed
length(unique(ttrDiet$No)) # total samples 81
length(unique(ttrDiet[!is.na(ttrDiet$Length),"No"])) # samples with length data 65
length(unique(ttrDiet[!ttrDiet$Sex == 0, "No"])) # samples with sex data 74
# Ranges
range(ttrDiet$Length, na.rm=T) # length range 179 320
range(ttrDiet$W, na.rm=T) # weight range 73.45677 579.30071
avDolph <- mean(ttrDiet$W, na.rm=T); avDolph # mean weight (average dolphin) 308.5955

### ggPlotting length dolphins distribution ###

wei <- tapply(ttrDiet$W, ttrDiet$No, mean)
wei <- round(wei)
weiFreq <- table(wei)
weiFreq <- as.data.frame(weiFreq)
weiFreq$wei <- as.numeric(as.character(weiFreq$wei))
weiFreqRg <- data.frame(weiFreq, 
                        rg=cut(weiFreq$wei, breaks=seq(70,600,20)))
ggData1 <- tapply(weiFreqRg$Freq, weiFreqRg$rg, sum)
ggData1 <- data.frame(Freq= ggData1[], rg=names(ggData1))
val <- gsub("^\\(", "", ggData1$rg)
val <- gsub(",+[0-9]+\\]","", val)
ggData1$val <- as.numeric(val)+2.5

gg1 <- ggplot(ggData1, aes(val, Freq)) + geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(70,600,20), labels=seq(70,600,20)) +
  ylab("frequency") + xlab("weight (kg)") + 
  ggtitle("bottlenose dolphin - estimated weight distribution") + 
  mytheme

len <- tapply(ttrDiet$Length, ttrDiet$No, mean)
len <- round(len)
lenFreq <- table(len)
lenFreq <- as.data.frame(lenFreq)
lenFreq$len <- as.numeric(as.character(lenFreq$len))
lenFreqRg <- data.frame(lenFreq, 
                        rg=cut(lenFreq$len, breaks=seq(160,320,10)))
ggData2 <- tapply(lenFreqRg$Freq, lenFreqRg$rg, sum)
ggData2 <- data.frame(Freq= ggData2[], rg=names(ggData2))
val <- gsub("^\\(", "", ggData2$rg)
val <- gsub(",+[0-9]+\\]","", val)
ggData2$val <- as.numeric(val)+2.5

gg2 <- ggplot(ggData2, aes(val, Freq)) + geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(160,320,10), labels=seq(160,320,10)) +
  ylab("frequency") + xlab("length (cm)") + 
  ggtitle("bottlenose dolphin - length distribution") + 
  mytheme

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/TTR_LenWeiDistr.png", width=600, height=400)
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
nrow(df) # 65


##### Innes et al. (1987) #####

# Daily food intake estimation
df$In <- 0.258 * (df$w)^0.69

# Bootstrap for an average dolphin (for the mean) with Innes index
meanFuncInnes <- function(data, d){
  sum(na.omit(data[d]))/length(na.omit(data[d]))
}
bootIn <- boot(data=df$In, meanFuncInnes, R=1000)
bootCIIn <- boot.ci(bootIn, type=c("perc"))



##### Yeates and Houser (2008) #####

mlO2MinKg <- 4.4
lO2 <- mlO2MinKg*20/1000 
dayMin <- 24*60
AMRmultHPBD <- 3 # AMR multiplicador para marsopa o delfin mular

# Daily food intake estimation
KjGal <- read.csv("../../RData/KjBDGal.csv")
KjAst <- read.csv("../../RData/KjBDAst.csv")

KjKgBDGal <- sum(KjGal[,4] * KjGal[,5] * 10)
KjKgBDAst <- sum(KjAst[,4] * KjAst[,5] * 10)

df$YHGal <- lO2*dayMin*df$w*AMRmultHPBD/KjKgBDGal*0.925
df$YHAst <- lO2*dayMin*df$w*AMRmultHPBD/KjKgBDAst*0.925

# Bootstrap for an average dolphin (for the mean) with Yeates and Houser index
meanFuncYeatHous <- function(data, d){
  sum(na.omit(data[d]))/length(na.omit(data[d]))
}
bootYHG <- boot(data=df$YHG, meanFuncYeatHous, R=1000)
bootCIYHG <- boot.ci(bootYHG, type=c("perc"))
bootYHA <- boot(data=df$YHA, meanFuncYeatHous, R=1000)
bootCIYHA <- boot.ci(bootYHA, type=c("perc"))



##### Kleiber (1947) #####

df$KleG <-  (293*df$w^0.75)/KjKgBDGal/0.925*4
df$KleAst <-  (293*df$w^0.75)/KjKgBDAst/0.925*4

# Bootstrap for an average dolphin (for the mean) with kleiber index
meanFuncKleiber <- function(data, d){
  sum(na.omit(data[d]))/length(na.omit(data[d]))
}

bootKlG <- boot(data=df$KleG, meanFuncKleiber, R=1000)
bootCIKlG <- boot.ci(bootKlG, type=c("perc"))
bootKlA <- boot(data=df$KleA, meanFuncKleiber, R=1000)
bootCIKlA <- boot.ci(bootKlA, type=c("perc"))


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

A + B * avDolph # 18.50334

### ggPloting ###

ggData <- melt(df, id.vars="w")

dat1 <- ggData[!(ggData$variable=="mod" | ggData$variable=="mod0"),]

gg1 <- ggplot(dat1, aes(w, value, group=variable)) + 
        geom_point(aes(colour=variable), pch=1) +
        ggtitle("Energy models for Bottlenose dolphin and new fitted models") + 
        scale_x_continuous(breaks=seq(50,500,50), limits=c(90,500)) +
        scale_y_continuous(breaks=seq(0,30,2), limits=c(4,30)) +
        ylab("daily food intake (kg)") + xlab("dolphin weight (kg)") +
        mytheme

dat2 <- ggData[ggData$variable=="mod",]
dat3 <- ggData[ggData$variable=="mod0",]

gg2 <- gg1 + geom_line(data=dat2, aes(w, value), cex=1)
gg3 <- gg2 + geom_line(data=dat3, aes(w, value), cex=1, lty=2)

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/TTR_EnergMod.png", width=600, height=400)
print(gg2)
dev.off()
#```

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/TTR_EnergMod0.png", width=600, height=400)
print(gg3)
dev.off()
#```

#########################################################################

# Kleiber Only Galicia

KlModG <- lm(allEnerg[!allEnerg$variable=="KleG","value"] ~ allEnerg[!allEnerg$variable=="KleG","w"])
A <- coef(KlModG)[[1]]
B <- coef(KlModG)[[2]]

KlEstG <- A + B * df$w
mean(KlEstG) # 19.12357


# Kleiber Only Asturias

KlModA <- lm(allEnerg[!allEnerg$variable=="KleA","value"] ~ allEnerg[!allEnerg$variable=="KleA","w"])
A <- coef(KlModA)[[1]]
B <- coef(KlModA)[[2]]

KlEstA <- A + B * df$w
mean(KlEstA) # 18.96999


# Yeates and Houser Only Galicia

YHModG <- lm(allEnerg[!allEnerg$variable=="YHGal","value"] ~ allEnerg[!allEnerg$variable=="YHGal","w"])
A <- coef(YHModG)[[1]]
B <- coef(YHModG)[[2]]

YHEstG <- A + B * df$w
mean(YHEstG) # 18.42509


# Yeates and Houser Only Asturias

YHModA <- lm(allEnerg[!allEnerg$variable=="YHAst","value"] ~ allEnerg[!allEnerg$variable=="YHAst","w"])
A <- coef(YHModA)[[1]]
B <- coef(YHModA)[[2]]

YHEstA <- A + B * df$w
mean(YHEstA) # 17.93537




