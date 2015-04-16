####################################################################################################
#                    EXPLORATORY ANALYSIS - COMMON DOLPHIN DIET (HAKE)
#              Length distribution (predator and prey), outliers and linear model
#                   C.Saavedra (camilo.saavedra@vi.ieo.es) 26/08/2013
####################################################################################################


# Set working directory (to source file location)

# Leemos los datos
dde <- read.csv("../../data/RData/DDE_AllDiet.csv")

# Seleccionamos solo los que tienen merluza como presa
ddeHke <- dde[dde$Prey=="Hake",]
ddeHke$Prey.length <- ddeHke$Prey.length*0.1 # Pasamos la merluza a centímetros

# Plot talla predador vs presa
plot (ddeHke$Length, ddeHke$Prey.length, main="Length Delphinus delphis vs Silver hake",
      xlab="Dolphin length (cm)", ylab="Hake length (cm)")
# Añadimos los cuartiles al plot
qt <- quantile(ddeHke$Prey.length); qt
abline(h=qt[2:4], col="red")

# Asignamos rangos de talla de los delfines
min <- round(seq(min(ddeHke$Length, na.rm=TRUE),max(ddeHke$Length, na.rm=TRUE), 10))
max <- round(seq(min(ddeHke$Length, na.rm=TRUE),max(ddeHke$Length, na.rm=TRUE), 10))+10
rg <- paste(min,"-",max,sep="")
# Creamos una matriz con los rangos
ranges <- data.frame(min=min, max=max, rg=rg)
# Generamos una nueva columnan con los rangos asignados 
ddeHke$rg.Length <- cut(ddeHke$Length, 
                        breaks = c(ranges$min[1], ranges$max), 
                        include.lowest = TRUE, 
                        labels = ranges$rg)

plot(ddeHke$rg.Length, ddeHke$Prey.length, main="Length Delphinus delphis vs Silver hake",
     xlab="Dolphin length ranges (cm)", ylab="Hake length (cm)")

# Histograma y funcion de densidad
hist(ddeHke$Prey.length, prob=TRUE, main="Hake Length",
     xlab="Hake length (cm)", ylab="Probability")
lines(density(ddeHke$Prey.length, adjust=2), col="red")

# Modelo lineal
lmTot <- lm(Prey.length ~ Length, data=ddeHke); lmTot
plot (ddeHke$Length, ddeHke$Prey.length, 
      main=paste("Y =", round(lmTot[[1]][[1]], 2), "+", round(lmTot[[1]][[2]],2),"X + e"),
      xlab="Dolphin length (cm)", ylab="Hake length (cm)")
abline(lmTot, col="red")


### Eliminando datos atípicos (valor máximo en la talla de merluza) ###

ddeHkeOut <- ddeHke[!ddeHke$Prey.length==max(ddeHke$Prey.length, na.rm=TRUE),]

# Plot talla predador vs presa
plot (ddeHkeOut$Length, ddeHkeOut$Prey.length, main="Length Delphinus delphis vs Silver hake",
      xlab="Dolphin length (cm)", ylab="Hake length (mm)")
# Añadimos los cuartiles al plot
qt <- quantile(ddeHkeOut$Prey.length); qt
abline(h=qt[2:4], col="blue")

# Histograma y funcion de densidad
hist(ddeHkeOut$Prey.length, prob=TRUE, main="Hake Length",
     xlab="Hake length (mm)", ylab="Probability")
lines(density(ddeHkeOut$Prey.length, adjust=2), col="blue")

# Modelo lineal
lmTotOut <- lm(Prey.length ~ Length, data=ddeHkeOut)
summary(lmTotOut)
plot (ddeHkeOut$Length, ddeHkeOut$Prey.length,
      main=paste("Y =", round(lmTotOut[[1]][[1]], 2), "+", round(lmTotOut[[1]][[2]],2),"X + e"),
      xlab="Dolphin length (cm)", ylab="Hake length (cm)")
abline(lmTotOut, col="blue")


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

# Plot general (dato atípico eliminado)
#png("../../plots/DDE_lengthSelect.png")
scatterhist (ddeHkeOut$Length, ddeHkeOut$Prey.length, xlab="dolphin length (cm)", 
             ylab="hake length (cm)")
#dev.off()

# Resultados del ajuste de la regresión
summary(lmTot)

# Distribución de tallas de delfines y merluzas

xhistD = hist(ddeHkeOut$Length, plot=FALSE)
rgMinD <- xhistD$mids-5
rgMaxD <- xhistD$mids+5
nD <- xhist$counts
# Common dolphin lengths frequencies
frecD <- data.frame(rgMinD, rgMaxD, nD); frecD 

yhistH = hist(ddeHkeOut$Prey.length, plot=FALSE)
rgMinH <- yhistH$mids-2.5
rgMaxH <- yhistH$mids+2.5
nH <- yhist$counts
# Hake lengths frequencies
frecH <- data.frame(rgMinH, rgMaxH, nH); frecH 


