####################################################################################################
#                    EXPLORATORY ANALYSIS - COMMON DOLPHIN DIET (HAKE)
#              Length distribution (predator and prey), outliers and linear model
#                    created: (camilo.saavedra@vi.ieo.es) 26/08/2013
#                    modified: (camilo.saavedra@vi.ieo.es) 03/12/2013
####################################################################################################

# inputs
# "../../RData/DDE_AllDiet.csv"

# outputs
# "../plots/DDE_DolphHkeLen.png"
# "../plots/DDE_DolphHkeLenQrt.png"
# "../plots/DDE_LenWeiDistr.png"

# IMPORTANT: Set working directory (to source file location)

# Charge libraries
library(ggplot2)
library(mgcv) 

# Leemos los datos
dde <- read.csv("../../RData/DDE_AllDiet.csv")

# fix wrong values
dde[dde$No=="DD104", "Sex"] <- 2
dde[dde$No=="DD134", "Sex"] <- 1
dde[dde$No=="DD562", "Sex"] <- 1

# Seleccionamos solo los que tienen merluza como presa
ddeHke <- dde[dde$Prey=="Hake",]
ddeHke$Prey.length <- ddeHke$Prey.length*0.1 # Pasamos la merluza a centímetros

# Plot talla predador vs presa
plot (ddeHke$Length, ddeHke$Prey.length,main="Length Common dolphin vs European hake",
#      xlab="Dolphin length (cm)", ylab="Hake length (cm)")


### Eliminando datos atípicos (valor máximo en la talla de merluza) ###
ddeHkeOut <- ddeHke[!ddeHke$Prey.length==max(ddeHke$Prey.length, na.rm=TRUE),] # removing the last value



##########################################################################

######################### Hake Lengths ###################################

# Asignamos rangos de talla de los delfines
min <- round(seq(min(ddeHke$Length, na.rm=TRUE),max(ddeHke$Length, na.rm=TRUE), 10))-1
max <- round(seq(min(ddeHke$Length, na.rm=TRUE),max(ddeHke$Length, na.rm=TRUE), 10))-1+10
rg <- paste(min,"-",max,sep="")
# Creamos una matriz con los rangos
ranges <- data.frame(min=min, max=max, rg=rg)
# Generamos una nueva columnan con los rangos asignados 
ddeHke$rg.Length <- cut(ddeHke$Length, 
                        breaks = c(ranges$min[1], ranges$max), 
                        include.lowest = TRUE, 
                        labels = ranges$rg)

# Box plot, dolphins length distribution vs hake length
plot(ddeHke$rg.Length, ddeHke$Prey.length, main="Length Common dolphin vs European hake",
     xlab="Dolphin length ranges (cm)", ylab="Hake length (cm)")

# Ranges
range(ddeHke$Prey.length) # 2.502927 60.841200
quantile(ddeHke$Prey.length, c(0.025, 0.975)) # 6.108626 34.568800 

# Asignamos rangos de talla de las merluzas
minH <- round(seq(min(ddeHke$Prey.length, na.rm=TRUE),max(ddeHke$Prey.length, na.rm=TRUE), 5))-3
maxH <- round(seq(min(ddeHke$Prey.length, na.rm=TRUE),max(ddeHke$Prey.length, na.rm=TRUE), 5))-3+5
rgH <- paste(minH,"-",maxH,sep="")
# Creamos una matriz con los rangos
rangesH <- data.frame(min=minH, max=maxH, rg=rgH)
# Generamos una nueva columnan con los rangos asignados 
ddeHke$rg.Prey.length <- cut(ddeHke$Prey.length, 
                        breaks = c(rangesH$min[1], rangesH$max), 
                        include.lowest = TRUE, 
                        labels = rangesH$rg)
# Plot talla presa vs predador
plot (ddeHke$rg.Prey.length,ddeHke$Length,  main="Length European hake vs Common dolphin",
      xlab="Hake length ranges (cm)", ylab="Dolphin length (cm)")

##########################################################################


### Predicting catchability function ###

# Kernel density estimation
v <- c(ddeHke$Prey.length , 120)
d <- density(v, adjust = 1)
y <- d$y
x <- d$x
g <- gam(y ~ s(x,k=30,fx=TRUE)) # degrees of freedom k ????
# Predict probability of a given hake length 
prednew <- function(u) predict(g,newdata=data.frame(x=u)) 

dfa <- data.frame(hklen = ddeHke$Prey.length)
dfb <- data.frame(hklen=0:120, pred=prednew(0:120))
# dfb # Catchability

gga <- ggplot(dfa, aes(hklen, ..density..)) + xlim(0,120) +
  geom_bar(binwidth=1) + ggtitle("Hake length distribution") +
  ylab("frequency") + xlab("hake length (cm)") +
  theme( panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
         legend.position="none")
ggb <- gga + geom_line(data=dfb, aes(hklen, pred), col="orangered")       

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_DolphHkeLen.png", width=600, height=400)
print(ggb)
dev.off()
#```   

########### BY QUARTERS ###############

# House keeping #

# Add new variables 
# Year, Month, Quarter
ddeHke$Year <- substr(ddeHke$Date,7,10)
ddeHke$Month <- substr(ddeHke$Date,4,5)
for (i in 1:nrow(ddeHke)){
  if(ddeHke$Month[i] %in% c("01","02","03")) {
    ddeHke$Quarter[i] <- 1
  } else {
    if(ddeHke$Month[i] %in% c("04","05","06")) {
      ddeHke$Quarter[i] <- 2
    } else { 
      if(ddeHke$Month[i] %in% c("07","08","09")) {
        ddeHke$Quarter[i] <- 3
      } else { 
        if(ddeHke$Month[i] %in% c("10","11","12")) {
          ddeHke$Quarter[i] <- 4
        } else {
          ddeHke$Quarter[i] <- NA 
        }
      } 
    }
  }
}

# Kernel density estimation

qrt <- c(1,2,3,4)

for (i in qrt){
  v <- c(ddeHke[ddeHke$Quarter==i,"Prey.length"] , 120)
  d <- density(v, adjust = 1)
  y <- d$y
  x <- d$x
  g <- gam(y ~ s(x,k=30,fx=TRUE)) # degrees of freedom k ????
  # Predict probability of a given hake length 
  prednew <- function(u) predict(g,newdata=data.frame(x=u)) 
  assign(paste("df", i, sep=""), data.frame(pred=prednew(0:120)))
}

dfb <- data.frame(hklen=0:120, pred=prednew(0:120))

dfQrt <- data.frame(hklen=0:120, "1"=df1[,], "2"=df2[,], "3"=df3[,], "4"=df4[,])
ggdfQrt <- melt(dfQrt, id.vars="hklen", variable_name="qrt")
ggdfQrt$qrt <- as.numeric(ggdfQrt$qrt)

df <- data.frame(hklen = ddeHke$Prey.length, qrt = ddeHke$Quarter)

gg1 <- ggplot(df, aes(hklen, ..density..)) + xlim(0,120) +
  geom_bar(binwidth=1) + ggtitle("Hake length distribution by quarters") +
  ylab("frequency") + xlab("hake length (cm)") +
  theme( panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
         legend.position="none") + facet_grid(qrt ~ .)

gg2 <- gg1 + geom_line(data=ggdfQrt, aes(hklen, value), col="orangered")

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_DolphHkeLenQrt.png", width=600, height=400)
print(gg2)
dev.off()
#```   
    



### using data without outliers but is the same
#tOut <- table(round(ddeHkeOut$Prey.length))
#pOut <- t / length(ddeHkeOut$Prey.length)
#nOut <-  as.numeric(names(p))
#vOut <- ddeHkeOut$Prey.length
#dOut <- density((v))
#yOut <- dOut$y
#xOut <- dOut$x
#gOut <- gam(yOut ~ s(xOut,k=20,fx=TRUE)) 
#lines(xOut, predict(gOut))
# Exit probability of a given hake length 
#prednew <- function(u) predict(g,newdata=data.frame(x=u)) 
#prednew(seq(1,61)) # select value = hake length
#AIC(g)
#AIC(gOut)


# log.normal distribution with mu and sigma from the data

#sigma = raiz (ln(1 + CV^2))
#mu = ln(media) - sigma^2 / 2
m <- mean(ddeHke$Prey.length)
s <- sd(ddeHke$Prey.length)
hist(rnorm(100, m, s))
sigma <- sqrt(log(1+(s/m)^2))
mu <- log(m)-(sigma^2)/2
hist(rlnorm(10000, mu, sigma))




# Linear model
lmTot <- lm(Prey.length ~ Length, data=ddeHke)
summary(lmTot)
plot (ddeHke$Length, ddeHke$Prey.length, xlim=c(0,240),
      main=paste("Y =", round(lmTot[[1]][[1]], 2), "+", round(lmTot[[1]][[2]],2),"X + e"),
      xlab="Dolphin length (cm)", ylab="Hake length (cm)")
abline(lmTot, col="red")
# Hake length prediction of a given dolphin length
lmPred <- function(u) predict(lmTot,newdata=data.frame(Length=u)) 
lmPred(180) # select dolphin length

# Corrected data (no outliers)
lmTotOut <- lm(Prey.length ~ Length, data=ddeHkeOut)
summary(lmTotOut)
plot (ddeHkeOut$Length, ddeHkeOut$Prey.length, xlim=c(0,240),
      main=paste("Y =", round(lmTotOut[[1]][[1]], 2), "+", round(lmTotOut[[1]][[2]],2),"X + e"),
      xlab="Dolphin length (cm)", ylab="Hake length (cm)")
abline(lmTotOut, col="red")
# Hake length prediction of a given dolphin length
lmPredOut <- function(u) predict(lmTotOut,newdata=data.frame(Length=u)) 
lmPredOut(180) # select dolphin length


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
  mtext("dolphin length vs prey length (hake)", side=3, line=0.5, outer=TRUE, font=2)
  par=par(par.default)
}


# Plot general

# ddeHkeOut <- ddeHkeOut[!(ddeHkeOut$Length<140),] # What will happen if small dolphins are removed (outlier)?

#png("../../plots/DDE_lengthSelect.png")
scatterhist (ddeHke$Length, ddeHke$Prey.length, xlab="dolphin length (cm)", 
             ylab="hake length (cm)")
# Corrected data
scatterhist (ddeHkeOut$Length, ddeHkeOut$Prey.length, xlab="dolphin length (cm)", 
             ylab="hake length (cm)")
#dev.off()


# Distribución de tallas de delfines y merluzas

xhistD = hist(ddeHkeOut$Length, plot=FALSE)
rgMinD <- xhistD$mids-5
rgMaxD <- xhistD$mids+5
nD <- xhistD$counts
# Common dolphin lengths frequencies
frecD <- data.frame(rgMinD, rgMaxD, nD); frecD 

yhistH = hist(ddeHkeOut$Prey.length, plot=FALSE)
rgMinH <- yhistH$mids-2.5
rgMaxH <- yhistH$mids+2.5
nH <- yhistH$counts
# Hake lengths frequencies
frecH <- data.frame(rgMinH, rgMaxH, nH); frecH 


