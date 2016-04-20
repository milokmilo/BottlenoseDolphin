################################################################################################## #
#                    EXPLORATORY ANALYSIS - BOTTLENOSE DOLPHIN DIET (Blue Whiting)
#              Length distribution (predator and prey), outliers and linear model
#                    created: (camilo.saavedra@vi.ieo.es) 21/04/2015
################################################################################################## #

# inputs
# "../../RData/TTR_AllDiet.csv"
# outputs
# "../plots/TTR_DolphBWLen.png"
# "../plots/TTR_DolphBWLenQrt.png"
# "../plots/TTR_BWLenSel.png"

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


# Seleccionamos solo los que tienen lirio como presa
ttrBW <- ttrDiet[ttrDiet$Prey=="Blue whiting",]
ttrBW$Prey.length <- as.numeric(as.character(ttrBW$Prey.length))
ttrBW$Prey.length <- ttrBW$Prey.length*0.1 # Pasamos la lirio a cent??metros

# Plot talla predador vs presa
#plot (ttrBW$Length, ttrBW$Prey.length,main="Length Bottlenose dolphin vs Blue Whiting",
#      xlab="Dolphin length (cm)", ylab="Blue Whiting length (cm)")


### Eliminando datos at??picos (valor máximo en la talla de lirio) ###
#ttrBWOut <- ttrBW[!ttrBW$Prey.length==max(ttrBW$Prey.length, na.rm=TRUE),] # removing the last value



##########################################################################

######################### Blue Whiting Lengths ###################################

# Asignamos rangos de talla de los delfines
min <- round(seq(min(ttrBW$Length, na.rm=TRUE),max(ttrBW$Length, na.rm=TRUE), 10))-1
max <- round(seq(min(ttrBW$Length, na.rm=TRUE),max(ttrBW$Length, na.rm=TRUE), 10))-1+10
rg <- paste(min,"-",max,sep="")
# Creamos una matriz con los rangos
ranges <- data.frame(min=min, max=max, rg=rg)
# Generamos una nueva columnan con los rangos asignados 
ttrBW$rg.Length <- cut(ttrBW$Length, 
                        breaks = c(ranges$min[1], ranges$max), 
                        include.lowest = TRUE, 
                        labels = ranges$rg)

# Remove unmearured Blue Whitings
ttrBW <- ttrBW[!ttrBW$Prey.length==0,]

# Box plot, dolphins length distribution vs Blue Whiting length
#plot(ttrBW$rg.Length, ttrBW$Prey.length, main="Length Bottlenose dolphin vs Blue Whiting",
#     xlab="Dolphin length ranges (cm)", ylab="Blue Whiting length (cm)")

# Blue Whiting ranges
range(ttrBW$Prey.length) #  5.4079 41.8568
quantile(ttrBW$Prey.length, c(0.025, 0.975)) # 13.78357 29.23640 
quantile(ttrBW$Prey.length, c(0.05, 0.9)) # 15.3746 26.3647


# Asignamos rangos de talla de las BWdinas
minH <- round(seq(min(ttrBW$Prey.length, na.rm=TRUE),max(ttrBW$Prey.length, na.rm=TRUE), 1))-1
maxH <- round(seq(min(ttrBW$Prey.length, na.rm=TRUE),max(ttrBW$Prey.length, na.rm=TRUE), 1))+1
rgH <- paste(minH,"-",maxH,sep="")
# Creamos una matriz con los rangos
rangesH <- data.frame(min=minH, max=maxH, rg=rgH)
# Generamos una nueva columnan con los rangos asignados 
ttrBW$rg.Prey.length <- cut(ttrBW$Prey.length, 
                        breaks = c(rangesH$min[1], rangesH$max), 
                        include.lowest = TRUE, 
                        labels = rangesH$rg)
# Plot talla presa vs predador
#plot (ttrBW$rg.Prey.length,ttrBW$Length,  main="Length Blue Whiting vs Bottlenose dolphin",
#      xlab="Blue Whiting length ranges (cm)", ylab="Dolphin length (cm)")

##########################################################################


### Predicting catchability function ###

# Kernel density estimation
v <- c(0, ttrBW$Prey.length , 40)
d <- density(v, adjust = 1)
#hist(ttrBW$Prey.length, probability=TRUE)
#lines(d)
y <- d$y
x <- d$x
g <- gam(y ~ s(x,k=50,fx=TRUE)) # degrees of freedom k ????
# Predict probability of a given Blue Whiting length 
prednew <- function(u) predict(g,newdata=data.frame(x=u)) 


dfa <- data.frame(BWlen = ttrBW$Prey.length)
dfb <- data.frame(BWlen=0:40, pred=prednew(0:40))
# dfb # Catchability

gga <- ggplot(dfa, aes(BWlen, ..density..)) + xlim(0,40) +
  geom_bar(binwidth=1) + ggtitle("Blue Whiting length distribution in Bottlenose dolphin stomachs") +
  ylab("frequency") + xlab("Blue Whiting length (cm)") +
  mytheme

ggb <- gga + geom_line(data=dfb, aes(BWlen, pred), col="orangered")       

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/TTR_DolphBWLen.png", width=600, height=400)
print(ggb)
dev.off()
#```   

########### BY QUARTERS ###############

# House keeping #

# Add new variables 
# Year, Month, Quarter
ttrBW$Year <- substr(ttrBW$Date,7,10)
ttrBW$Month <- substr(ttrBW$Date,4,5)
for (i in 1:nrow(ttrBW)){
  if(ttrBW$Month[i] %in% c("01","02","03")) {
    ttrBW$Quarter[i] <- 1
  } else {
    if(ttrBW$Month[i] %in% c("04","05","06")) {
      ttrBW$Quarter[i] <- 2
    } else { 
      if(ttrBW$Month[i] %in% c("07","08","09")) {
        ttrBW$Quarter[i] <- 3
      } else { 
        if(ttrBW$Month[i] %in% c("10","11","12")) {
          ttrBW$Quarter[i] <- 4
        } else {
          ttrBW$Quarter[i] <- NA 
        }
      } 
    }
  }
}

# Kernel density estimation

qrt <- c(1,2,3,4)

for (i in qrt){
  v <- c(0, ttrBW[ttrBW$Quarter==i,"Prey.length"] , 40)
  d <- density(v, adjust = 1)
  y <- d$y
  x <- d$x
  g <- gam(y ~ s(x,k=50,fx=TRUE)) # degrees of freedom k ????
  # Predict probability of a given Blue Whiting length 
  prednew <- function(u) predict(g,newdata=data.frame(x=u)) 
  assign(paste("df", i, sep=""), data.frame(pred=prednew(0:40)))
}

dfb <- data.frame(BWlen=0:40, pred=prednew(0:40))

dfQrt <- data.frame(BWlen=0:40, "1"=df1[,], "2"=df2[,], "3"=df3[,], "4"=df4[,])
ggdfQrt <- melt(dfQrt, id.vars="BWlen", variable_name="qrt")
ggdfQrt$qrt <- as.numeric(ggdfQrt$qrt)

df <- data.frame(BWlen = ttrBW$Prey.length, qrt = ttrBW$Quarter)

gg1 <- ggplot(df, aes(BWlen, ..density..)) + xlim(0,40) +
  geom_bar(binwidth=0.5) + ggtitle("Blue Whiting length distribution by quarters in Bottlenose dolphin stomachs") +
  ylab("frequency") + xlab("Blue Whiting length (cm)") +
  theme( panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
         legend.position="top") + facet_grid(qrt ~ .)

gg2 <- gg1 + geom_line(data=ggdfQrt, aes(BWlen, value), col="orangered")

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/TTR_DolphBWLenQrt.png", width=600, height=400)
print(gg2)
dev.off()
#```   
    

# log.normal distribution with mu and sigma from the data

#sigma = raiz (ln(1 + CV^2))
#mu = ln(media) - sigma^2 / 2
m <- mean(ttrBW$Prey.length)
s <- sd(ttrBW$Prey.length)
#hist(rnorm(100, m, s))
sigma <- sqrt(log(1+(s/m)^2))
mu <- log(m)-(sigma^2)/2
#hist(rlnorm(10000, mu, sigma))



# Linear model
lmTot <- lm(Prey.length ~ Length, data=ttrBW)
#summary(lmTot)
#plot (ttrBW$Length, ttrBW$Prey.length, xlim=c(0,240),
#      main=paste("Y =", round(lmTot[[1]][[1]], 2), "+", round(lmTot[[1]][[2]],2),"X + e"),
#      xlab="Dolphin length (cm)", ylab="Blue Whiting length (cm)")
#abline(lmTot, col="red")
# Blue Whiting length prediction of a given dolphin length
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
  mtext("dolphin length vs prey length (Blue Whiting)", side=3, line=0.5, outer=TRUE, font=2)
  par=par(par.default)
}


# Plot general

# ttrBWOut <- ttrBWOut[!(ttrBWOut$Length<140),] # What will happen if small dolphins are removed (outlier)?

png("../plots/TTR_BWLenSel.png")
scatterhist (ttrBW$Length, ttrBW$Prey.length, xlab="dolphin length (cm)", 
             ylab="Blue Whiting length (cm)")
dev.off()


# Distribución de tallas de delfines y lirios

xhistD = hist(ttrBW$Length, plot=FALSE)
rgMinD <- xhistD$mids-5
rgMaxD <- xhistD$mids+5
nD <- xhistD$counts
# Bottlenose dolphin lengths frequencies
frecD <- data.frame(rgMinD, rgMaxD, nD); frecD 

yhistH = hist(ttrBW$Prey.length, plot=FALSE)
rgMinH <- yhistH$mids-2.5
rgMaxH <- yhistH$mids+2.5
nH <- yhistH$counts
# Blue Whiting lengths frequencies
frecH <- data.frame(rgMinH, rgMaxH, nH); frecH 

# Close opened plots
#dev.off()

# Detach packages
detach(package:mgcv)


