####################################################################################################
#                    EXPLORATORY ANALYSIS - COMMON DOLPHIN DIET (Blue Whiting)
#              Length distribution (predator and prey), outliers and linear model
#                    created: (camilo.saavedra@vi.ieo.es) 26/08/2013
#                    modified: (camilo.saavedra@vi.ieo.es) 03/12/2013
#                    modified: (camilo.saavedra@vi.ieo.es) 12/11/2014
####################################################################################################

# inputs
# "../../RData/DDE_AllDiet.csv"

# outputs
# "../plots/DDE_DolphBWLen.png"
# "../plots/DDE_DolphBWLenQrt.png"
# "../plots/DDE_BWLenSel.png"

# Set working directory (to source file location)

# Charge libraries
library(ggplot2)
library(mgcv) 
library(reshape)

# Leemos los datos
dde <- read.csv("../../RData/DDE_AllDiet.csv")

# fix wrong values
dde[dde$No=="DD104", "Sex"] <- 2
dde[dde$No=="DD134", "Sex"] <- 1
dde[dde$No=="DD562", "Sex"] <- 1

# Seleccionamos solo los que tienen merluza como presa
ddeBW <- dde[dde$Prey=="Blue whiting",]
ddeBW$Prey.length <- ddeBW$Prey.length*0.1 # Pasamos la merluza a cent�?metros

# Plot talla predador vs presa
#plot (ddeBW$Length, ddeBW$Prey.length,main="Length Common dolphin vs Blue Whiting",
#      xlab="Dolphin length (cm)", ylab="Blue Whiting length (cm)")


### Eliminando datos at�?picos (valor máximo en la talla de merluza) ###
#ddeBWOut <- ddeBW[!ddeBW$Prey.length==max(ddeBW$Prey.length, na.rm=TRUE),] # removing the last value



##########################################################################

######################### Blue Whiting Lengths ###################################

# Asignamos rangos de talla de los delfines
min <- round(seq(min(ddeBW$Length, na.rm=TRUE),max(ddeBW$Length, na.rm=TRUE), 10))-1
max <- round(seq(min(ddeBW$Length, na.rm=TRUE),max(ddeBW$Length, na.rm=TRUE), 10))-1+10
rg <- paste(min,"-",max,sep="")
# Creamos una matriz con los rangos
ranges <- data.frame(min=min, max=max, rg=rg)
# Generamos una nueva columnan con los rangos asignados 
ddeBW$rg.Length <- cut(ddeBW$Length, 
                        breaks = c(ranges$min[1], ranges$max), 
                        include.lowest = TRUE, 
                        labels = ranges$rg)

# Remove unmearured Blue Whitings
ddeBW <- ddeBW[!ddeBW$Prey.length==0,]

# Box plot, dolphins length distribution vs Blue Whiting length
#plot(ddeBW$rg.Length, ddeBW$Prey.length, main="Length Common dolphin vs Blue Whiting",
#     xlab="Dolphin length ranges (cm)", ylab="Blue Whiting length (cm)")

# Blue Whiting ranges
range(ddeBW$Prey.length) #  3.523628 30.676700
quantile(ddeBW$Prey.length, c(0.025, 0.975)) # 8.7656 23.4930
quantile(ddeBW$Prey.length, c(0.05, 0.9)) # 10.5615 21.3370 

# Asignamos rangos de talla de las BWdinas
minH <- round(seq(min(ddeBW$Prey.length, na.rm=TRUE),max(ddeBW$Prey.length, na.rm=TRUE), 1))-1
maxH <- round(seq(min(ddeBW$Prey.length, na.rm=TRUE),max(ddeBW$Prey.length, na.rm=TRUE), 1))+1
rgH <- paste(minH,"-",maxH,sep="")
# Creamos una matriz con los rangos
rangesH <- data.frame(min=minH, max=maxH, rg=rgH)
# Generamos una nueva columnan con los rangos asignados 
ddeBW$rg.Prey.length <- cut(ddeBW$Prey.length, 
                        breaks = c(rangesH$min[1], rangesH$max), 
                        include.lowest = TRUE, 
                        labels = rangesH$rg)
# Plot talla presa vs predador
#plot (ddeBW$rg.Prey.length,ddeBW$Length,  main="Length Blue Whiting vs Common dolphin",
#      xlab="Blue Whiting length ranges (cm)", ylab="Dolphin length (cm)")

##########################################################################


### Predicting catchability function ###

# Kernel density estimation
v <- c(0, ddeBW$Prey.length , 30)
d <- density(v, adjust = 1)
#hist(ddeBW$Prey.length, probability=TRUE)
#lines(d)
y <- d$y
x <- d$x
g <- gam(y ~ s(x,k=50,fx=TRUE)) # degrees of freedom k ????
# Predict probability of a given Blue Whiting length 
prednew <- function(u) predict(g,newdata=data.frame(x=u)) 


dfa <- data.frame(BWlen = ddeBW$Prey.length)
dfb <- data.frame(BWlen=0:30, pred=prednew(0:30))
# dfb # Catchability

gga <- ggplot(dfa, aes(BWlen, ..density..)) + xlim(0,30) +
  geom_bar(binwidth=0.5) + ggtitle("Blue Whiting length distribution") +
  ylab("frequency") + xlab("Blue Whiting length (cm)") +
  theme( panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
         legend.position="top")
ggb <- gga + geom_line(data=dfb, aes(BWlen, pred), col="orangered")       

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_DolphBWLen.png", width=600, height=400)
print(ggb)
dev.off()
#```   

########### BY QUARTERS ###############

# House keeping #

# Add new variables 
# Year, Month, Quarter
ddeBW$Year <- substr(ddeBW$Date,7,10)
ddeBW$Month <- substr(ddeBW$Date,4,5)
for (i in 1:nrow(ddeBW)){
  if(ddeBW$Month[i] %in% c("01","02","03")) {
    ddeBW$Quarter[i] <- 1
  } else {
    if(ddeBW$Month[i] %in% c("04","05","06")) {
      ddeBW$Quarter[i] <- 2
    } else { 
      if(ddeBW$Month[i] %in% c("07","08","09")) {
        ddeBW$Quarter[i] <- 3
      } else { 
        if(ddeBW$Month[i] %in% c("10","11","12")) {
          ddeBW$Quarter[i] <- 4
        } else {
          ddeBW$Quarter[i] <- NA 
        }
      } 
    }
  }
}

# Kernel density estimation

qrt <- c(1,2,3,4)

for (i in qrt){
  v <- c(0, ddeBW[ddeBW$Quarter==i,"Prey.length"] , 30)
  d <- density(v, adjust = 1)
  y <- d$y
  x <- d$x
  g <- gam(y ~ s(x,k=50,fx=TRUE)) # degrees of freedom k ????
  # Predict probability of a given Blue Whiting length 
  prednew <- function(u) predict(g,newdata=data.frame(x=u)) 
  assign(paste("df", i, sep=""), data.frame(pred=prednew(0:30)))
}

dfb <- data.frame(BWlen=0:30, pred=prednew(0:30))

dfQrt <- data.frame(BWlen=0:30, "1"=df1[,], "2"=df2[,], "3"=df3[,], "4"=df4[,])
ggdfQrt <- melt(dfQrt, id.vars="BWlen", variable_name="qrt")
ggdfQrt$qrt <- as.numeric(ggdfQrt$qrt)

df <- data.frame(BWlen = ddeBW$Prey.length, qrt = ddeBW$Quarter)

gg1 <- ggplot(df, aes(BWlen, ..density..)) + xlim(0,30) +
  geom_bar(binwidth=0.5) + ggtitle("Blue Whiting length distribution by quarters") +
  ylab("frequency") + xlab("Blue Whiting length (cm)") +
  theme( panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
         legend.position="top") + facet_grid(qrt ~ .)

gg2 <- gg1 + geom_line(data=ggdfQrt, aes(BWlen, value), col="orangered")

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_DolphBWLenQrt.png", width=600, height=400)
print(gg2)
dev.off()
#```   
    

# log.normal distribution with mu and sigma from the data

#sigma = raiz (ln(1 + CV^2))
#mu = ln(media) - sigma^2 / 2
m <- mean(ddeBW$Prey.length)
s <- sd(ddeBW$Prey.length)
#hist(rnorm(100, m, s))
sigma <- sqrt(log(1+(s/m)^2))
mu <- log(m)-(sigma^2)/2
hist(rlnorm(10000, mu, sigma))



# Linear model
lmTot <- lm(Prey.length ~ Length, data=ddeBW)
summary(lmTot)
#plot (ddeBW$Length, ddeBW$Prey.length, xlim=c(0,240),
#      main=paste("Y =", round(lmTot[[1]][[1]], 2), "+", round(lmTot[[1]][[2]],2),"X + e"),
#      xlab="Dolphin length (cm)", ylab="Blue Whiting length (cm)")
#abline(lmTot, col="red")
# Blue Whiting length prediction of a given dolphin length
lmPred <- function(u) predict(lmTot,newdata=data.frame(Length=u)) 
lmPred(180) # select dolphin length



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

# ddeBWOut <- ddeBWOut[!(ddeBWOut$Length<140),] # What will happen if small dolphins are removed (outlier)?

png("../plots/DDE_BWLenSel.png")
scatterhist (ddeBW$Length, ddeBW$Prey.length, xlab="dolphin length (cm)", 
             ylab="Blue Whiting length (cm)")
dev.off()


# Distribución de tallas de delfines y merluzas

xhistD = hist(ddeBW$Length, plot=FALSE)
rgMinD <- xhistD$mids-5
rgMaxD <- xhistD$mids+5
nD <- xhistD$counts
# Common dolphin lengths frequencies
frecD <- data.frame(rgMinD, rgMaxD, nD); frecD 

yhistH = hist(ddeBW$Prey.length, plot=FALSE)
rgMinH <- yhistH$mids-2.5
rgMaxH <- yhistH$mids+2.5
nH <- yhistH$counts
# Blue Whiting lengths frequencies
frecH <- data.frame(rgMinH, rgMaxH, nH); frecH 

# Close opened plots
dev.off()

# Detach packages
detach(package:mgcv)

