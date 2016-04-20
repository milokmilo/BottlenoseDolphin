################################################################################################## #
#                    EXPLORATORY ANALYSIS - BOTTLENOSE DOLPHIN DIET (Sardine)
#              Length distribution (predator and prey), outliers and linear model
#                    created: (camilo.saavedra@vi.ieo.es) 21/04/2015
################################################################################################## #

# inputs
# "../../RData/TTR_AllDiet.csv"
# outputs
# "../plots/TTR_DolphSarLen.png"
# "../plots/TTR_DolphSarLenQrt.png"
# "../plots/TTR_SarLenSel.png"

################################################################################################## #


# IMPORTANT: Set working directory (to source file location)
#setwd("./Diet/finalCode")
#setwd("../../Diet/finalCode")


# Charge libraries
library(ggplot2)
library(nlme)
library(mgcv) 
library(reshape)
mytheme <- theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
                 legend.position="top")

# Leemos los datos
ttrDiet <- read.csv2("../../RData/TTR_AllDiet.csv")

# Seleccionamos solo los que tienen sardina como presa
ttrSar <- ttrDiet[ttrDiet$Prey=="Sardine",]
ttrSar$Prey.length <- as.numeric(as.character(ttrSar$Prey.length))
ttrSar$Prey.length <- ttrSar$Prey.length*0.1 # Pasamos la sardina a centímetros

# Plot talla predador vs presa
#plot (ttrSar$Length, ttrSar$Prey.length,main="Length Bottlenose dolphin vs Sardine",
#      xlab="Dolphin length (cm)", ylab="Sardine length (cm)")


### Eliminando datos atípicos (valor máximo en la talla de sardina) ###
#ttrSarOut <- ttrSar[!ttrSar$Prey.length==max(ttrSar$Prey.length, na.rm=TRUE),] # removing the last value



##########################################################################

######################### Sardine Lengths ###################################

# Asignamos rangos de talla de los delfines
min <- round(seq(min(ttrSar$Length, na.rm=TRUE),max(ttrSar$Length, na.rm=TRUE), 10))-1
max <- round(seq(min(ttrSar$Length, na.rm=TRUE),max(ttrSar$Length, na.rm=TRUE), 10))-1+10
rg <- paste(min,"-",max,sep="")
# Creamos una matriz con los rangos
ranges <- data.frame(min=min, max=max, rg=rg)
# Generamos una nueva columnan con los rangos asignados 
ttrSar$rg.Length <- cut(ttrSar$Length, 
                        breaks = c(ranges$min[1], ranges$max), 
                        include.lowest = TRUE, 
                        labels = ranges$rg)

# Remove unmearured Sardines
ttrSar <- ttrSar[!ttrSar$Prey.length==0,]

# Box plot, dolphins length distribution vs sardine length
#plot(ttrSar$rg.Length, ttrSar$Prey.length, main="Length Bottlenose dolphin vs Sardine",
#     xlab="Dolphin length ranges (cm)", ylab="Sardine length (cm)")

# Sardine ranges
range(ttrSar$Prey.length) #  16.7037 20.1759
quantile(ttrSar$Prey.length, c(0.025, 0.975)) # 16.89466 19.74621 

# Asignamos rangos de talla de las Sardinas
minH <- round(seq(min(ttrSar$Prey.length, na.rm=TRUE),max(ttrSar$Prey.length, na.rm=TRUE), 1))-1
maxH <- round(seq(min(ttrSar$Prey.length, na.rm=TRUE),max(ttrSar$Prey.length, na.rm=TRUE), 1))+1
rgH <- paste(minH,"-",maxH,sep="")
# Creamos una matriz con los rangos
rangesH <- data.frame(min=minH, max=maxH, rg=rgH)
# Generamos una nueva columnan con los rangos asignados 
ttrSar$rg.Prey.length <- cut(ttrSar$Prey.length, 
                        breaks = c(rangesH$min[1], rangesH$max), 
                        include.lowest = TRUE, 
                        labels = rangesH$rg)
# Plot talla presa vs predador
#plot (ttrSar$rg.Prey.length,ttrSar$Length,  main="Length Sardine vs Bottlenose dolphin",
#      xlab="Sardine length ranges (cm)", ylab="Dolphin length (cm)")

##########################################################################


### Predicting catchability function ###

# Kernel density estimation
v <- c(0, ttrSar$Prey.length , 40)
d <- density(v, adjust = 1)
#hist(ttrSar$Prey.length, probability=TRUE)
#lines(d)
y <- d$y
x <- d$x
g <- gam(y ~ s(x,k=50,fx=TRUE)) # degrees of freedom k ????
# Predict probability of a given Sardine length 
prednew <- function(u) predict(g,newdata=data.frame(x=u)) 


dfa <- data.frame(sarlen = ttrSar$Prey.length)
dfb <- data.frame(sarlen=0:30, pred=prednew(0:30))
# dfb # Catchability

gga <- ggplot(dfa, aes(sarlen, ..density..)) + xlim(0,30) +
  geom_bar(binwidth=0.5) + ggtitle("Sardine length distribution in Bottlenose dolphin stomachs") +
  ylab("frequency") + xlab("Sardine length (cm)") +
  theme( panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
         legend.position="top")

ggb <- gga + geom_line(data=dfb, aes(sarlen, pred), col="orangered")       

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/TTR_DolphSarLen.png", width=600, height=400)
print(ggb)
dev.off()
#```   


########### BY QUARTERS ###############

# House keeping #

# Add new variables 
# Year, Month, Quarter
ttrSar$Year <- substr(ttrSar$Date,7,10)
ttrSar$Month <- substr(ttrSar$Date,4,5)
for (i in 1:nrow(ttrSar)){
  if(ttrSar$Month[i] %in% c("01","02","03")) {
    ttrSar$Quarter[i] <- 1
  } else {
    if(ttrSar$Month[i] %in% c("04","05","06")) {
      ttrSar$Quarter[i] <- 2
    } else { 
      if(ttrSar$Month[i] %in% c("07","08","09")) {
        ttrSar$Quarter[i] <- 3
      } else { 
        if(ttrSar$Month[i] %in% c("10","11","12")) {
          ttrSar$Quarter[i] <- 4
        } else {
          ttrSar$Quarter[i] <- NA 
        }
      } 
    }
  }
}

# Kernel density estimation

qrt <- c(1,2,3,4)

for (i in qrt){
  v <- c(0, ttrSar[ttrSar$Quarter==i,"Prey.length"] , 30)
  d <- density(v, adjust = 1)
  y <- d$y
  x <- d$x
  g <- gam(y ~ s(x,k=50,fx=TRUE)) # degrees of freedom k ????
  # Predict probability of a given Sardine length 
  prednew <- function(u) predict(g,newdata=data.frame(x=u)) 
  assign(paste("df", i, sep=""), data.frame(pred=prednew(0:30)))
}

dfb <- data.frame(sarlen=0:30, pred=prednew(0:30))

dfQrt <- data.frame(sarlen=0:30, "1"=df1[,], "2"=df2[,], "3"=df3[,], "4"=df4[,])
ggdfQrt <- melt(dfQrt, id.vars="sarlen", variable_name="qrt")
ggdfQrt$qrt <- as.numeric(ggdfQrt$qrt)

df <- data.frame(sarlen = ttrSar$Prey.length, qrt = ttrSar$Quarter)

gg1 <- ggplot(df, aes(sarlen, ..density..)) + xlim(0,30) +
  geom_bar(binwidth=0.5) + ggtitle("Sardine length distribution by quarters in Bottlenose dolphin stomachs") +
  facet_grid(qrt ~ .) +
  ylab("frequency") + xlab("Sardine length (cm)") +
  mytheme

gg2 <- gg1 + geom_line(data=ggdfQrt, aes(sarlen, value), col="orangered")

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/TTR_DolphSarLenQrt.png", width=600, height=400)
print(gg2)
dev.off()
#```   
    

# log.normal distribution with mu and sigma from the data

#sigma = raiz (ln(1 + CV^2))
#mu = ln(media) - sigma^2 / 2
m <- mean(ttrSar$Prey.length)
s <- sd(ttrSar$Prey.length)
#hist(rnorm(100, m, s))
sigma <- sqrt(log(1+(s/m)^2))
mu <- log(m)-(sigma^2)/2
#hist(rlnorm(10000, mu, sigma))


# Linear model
lmTot <- lm(Prey.length ~ Length, data=ttrSar)
#summary(lmTot)
#plot (ttrSar$Length, ttrSar$Prey.length, xlim=c(0,240),
#      main=paste("Y =", round(lmTot[[1]][[1]], 2), "+", round(lmTot[[1]][[2]],2),"X + e"),
#      xlab="Dolphin length (cm)", ylab="Sardine length (cm)")
#abline(lmTot, col="red")
# Sardine length prediction of a given dolphin length
lmPred <- function(u) predict(lmTot,newdata=data.frame(Length=u)) 
#lmPred(180) # select dolphin length



### Plot con histogramas, regresión y gráfico de dispersión ###
scatterhist = function(x, y, xlab="", ylab=""){
  par.default <- par(no.readonly=TRUE)
  zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
  xhist = hist(x, plot=FALSE)
  yhist = hist(y, plot=FALSE)
  top = max(c(xhist$counts, yhist$counts))
  par(mar=c(5,5,1,1))
  plot(x,y, pch=1, cex=0.8, xlab=xlab, ylab=ylab)
  #abline(v=quantile(x, na.rm=TRUE)[c(2,4)], lty=3)
  #abline(h=quantile(y, na.rm=TRUE)[c(2,4)], lty=3)
  lmTot <- lm(y ~ x)
  abline(lmTot, col="black")
  par(mar=c(0,5,1,1))
  barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0, col="gray70")
  par(mar=c(5,0,1,1))
  barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE, col="gray70")
  par(oma=c(0,0,2,0))
  mtext("Bottlenose dolphin length vs prey length (sardine)", side=3, line=0.5, outer=TRUE, font=2)
  par=par(par.default)
}


# Plot general

# ttrSarOut <- ttrSarOut[!(ttrSarOut$Length<140),] # What will happen if small dolphins are removed (outlier)?

png("../plots/TTR_SarLenSel.png")
scatterhist (ttrSar$Length, ttrSar$Prey.length, xlab="dolphin length (cm)", 
             ylab="Sardine length (cm)")
dev.off()


# Distribución de tallas de delfines y sardinas

xhistD = hist(ttrSar$Length, plot=FALSE)
rgMinD <- xhistD$mids-5
rgMaxD <- xhistD$mids+5
nD <- xhistD$counts
# Bottlenose dolphin lengths frequencies
frecD <- data.frame(rgMinD, rgMaxD, nD); frecD 

yhistH = hist(ttrSar$Prey.length, plot=FALSE)
rgMinH <- yhistH$mids-2.5
rgMaxH <- yhistH$mids+2.5
nH <- yhistH$counts
# Sardine lengths frequencies
frecH <- data.frame(rgMinH, rgMaxH, nH); frecH 

# Close opened plots
#dev.off()

# Detach packages
detach(package:mgcv)


