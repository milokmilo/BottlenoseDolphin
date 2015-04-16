####################################################################################################
#               Previous analysis - Common Dolphin diet (hake sizes)
#               
#                 modified: (camilo.saavedra@vi.ieo.es) 01/10/2014
####################################################################################################

# Set working directory (to source file location)

dde <- read.csv("../../RData/DDE_AllDiet.csv")

# seleccionamos solo los que tienen merluza como presa
ddeHke <- dde[dde$Prey=="Hake",]
ddeHkeMale <- dde[dde$Prey=="Hake" & dde$Sex==1,]
ddeHkeFemale <- dde[dde$Prey=="Hake" & dde$Sex==2,]

# cogemos el peso de cada merluza medida en un estómago
str(ddeHke$Prey.length)
#ddeHke$Prey.weight <- as.numeric(as.character(ddeHke$Prey.weight))
#ddeHke <- ddeHke[!is.na(ddeHke$Prey.weight),]

totMale <- as.numeric(as.character(ddeHkeMale$Prey.weight))
totMale <- totMale[!is.na(totMale)]
totFemale <- as.numeric(as.character(ddeHkeFemale$Prey.weight))
totFemale <- totFemale[!is.na(totFemale)]

# transformamos el peso a talla medainte una regresión


# hacemos un histograma
hist(ddeHke$Prey.length)
quantile(ddeHke$Prey.length)
boxplot(ddeHke$Prey.length)
mean(ddeHke$Prey.length)


var(ddeHke$Prey.length)

hist(totMale)
hist(totFemale)
# otra información
boxplot(totMale)
mean(totMale)
max(totMale)
min(totMale)
boxplot(totFemale)
mean(totFemale)
max(totFemale)
min(totFemale)


### linear model ###

lmTot <- lm(ddeHke$Prey.length ~ Length, data=ddeHke)
plot (ddeHke$Length, ddeHke$Prey.length)
abline(lmTot)

hist(ddeHke$Prey.length, prob=TRUE)
lines(density(ddeHke$Prey.length, adjust=2))


# Eliminando datos atípicos (dato máximo de talla de merluza)
ddeHkeOut <- ddeHke[!ddeHke$Prey.length==max(ddeHke$Prey.length, na.rm=TRUE),]

lmTotOut <- lm(ddeHkeOut$Prey.length ~ Length, data=ddeHkeOut)

source("scatterhist.r")
scatterhist (ddeHkeOut$Length, ddeHkeOut$Prey.length, xlab="dolphin length (cm)", ylab="hake length (mm)")


abline(lmTotOut)
x=ddeHkeOut$Length
y=ddeHkeOut$Prey.length
hist(ddeHke$Prey.length)
lines(density(ddeHkeOut$Prey.length, adjust=2))


hist(ddeHke$Length, prob=TRUE)
lines(density(ddeHkeOut$Length, na.rm=TRUE, adjust=2))
##### NO RANGOS #########

## Asignamos rangos de talla de los delfines
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

plot(ddeHke$rg.Length, ddeHke$Prey.length)

  
  

par.default <- par(no.readonly=TRUE)
par(mar=c(3,3,1,1))
plot(ddeHke$rg.Length, ddeHke$Prey.length)
par(mar=c(0,3,1,1))
hist(ddeHke$Prey.length)
par(mar=c(3,0,1,1))
hist(ddeHke$Prey.length)
par(oma=c(3,3,0,0))
par=par(par.default)









  

