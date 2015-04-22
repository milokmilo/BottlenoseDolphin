################################################################################################## #
#                    BOX PLOT BY QUARTER - BOTTLENOSE DOLPHIN DIET (HAKE)
#             Box plot of number and weight prey by quarter, length distribution of the diet
#                    created: (camilo.saavedra@vi.ieo.es) 22/04/2015
################################################################################################## #

# inputs
# "../../RData/TTR_AllDiet.csv"
# outputs
# "../plots/TTR_hkeLen.png"

################################################################################################## #


# IMPORTANT: Set working directory (to source file location)
#setwd("./Diet/finalCode")
#setwd("../../Diet/finalCode")

# Leemos los datos
ttr <- read.csv2("../../RData/TTR_AllDiet.csv")

# Seleccionamos solo los que tienen merluza como presa
### OJO! No estamos incluyendo los que no tienen merluza pero si otro alimento ###
ttrHke <- ttr[ttr$Prey=="Hake",]
ttrHke$Prey.length <- as.numeric(as.character(ttrHke$Prey.length))
ttrHke$Prey.length <- ttrHke$Prey.length*0.1 # Pasamos la merluza a centímetros

# Separamos la fecha en tres columnas y creamos otra con los trimestres
ttrHke$Date <- as.Date(ttrHke$Date, "%d/%m/%Y")
ttrHke <- data.frame(ttrHke[,1:3],
                     year = as.numeric(format(ttrHke$Date, format = "%Y")),
                     month = as.numeric(format(ttrHke$Date, format = "%m")),
                     day = as.numeric(format(ttrHke$Date, format = "%d")),
                     ttrHke[,7:ncol(ttrHke)])
 
# Creamos un vector con los trimestres y lo añadimos al data.frame
quarter <- numeric()
for (i in 1:nrow(ttrHke)){
  if (ttrHke$month[i] %in% c(1,2,3)) {
    quarter <- c(quarter,1)
    }else{
      if (ttrHke$month[i] %in% c(4,5,6)){
        quarter <- c(quarter,2)
      }else{
        if(ttrHke$month[i] %in% c(7,8,9)){
          quarter <- c(quarter,3)
        }else{
          if(ttrHke$month[i] %in% c(10,11,12)){
            quarter <- c(quarter,4)
          }else{
            stop("error: month value doesn't exist")
          }
        }
      }
    } 
}

ttrHke <- data.frame(ttrHke[,1:6],
                     Quarter = quarter,
                     ttrHke[,8:ncol(ttrHke)])

# Hacemos una tabla con el número de merluzas en el estómado de cada delfín
dh <- data.frame(No=numeric(0), Count=numeric(0), Weight=numeric(0), Quarter=numeric(0))
for(i in unique(ttrHke$No)){
  n <- sum(as.numeric(unique(subset(ttrHke, No==i, select=Count))[,1]))/2 #si queremos el numero de merluzas
  q <- unique(subset(ttrHke, No==i, select=Quarter))[,1]
  w <- subset(ttrHke, No==i, select=c(Weighting,Prey.weight))
  w <- round(sum(as.numeric(as.character(w$Weighting)) * as.numeric(as.character(w$Prey.weight))),3)
  dh[nrow(dh)+1,] <- c(i,n,w,q) # El [1] hay que ponerlo para que en el DD722 coja el primer valor 12 y no el 1 (error DataEase)
}
dh$Count <- as.numeric(dh$Count)
dh$Weight <- as.numeric(dh$Weight)



## Ploteamos Número de otolitos y distribución de tallas juntos y añadimos peso de contenido de merluza por estómago ##

par.default <- par(no.readonly=TRUE)
png("../plots/TTR_hkeLen.png", height=600, width=900)
par(oma=c(3,3,3,3))  
zones=matrix(c(1,0,2,3,4,0,5,6,7,0,8,9,10,0,11,12), ncol=2, byrow=TRUE)
layout(zones, widths=c(4/5,1/5), heights=rep(c(1/16, 2/16), 4))
par(mar=c(0,3,2,1))
hist(subset(ttrHke, Quarter==1, select=Prey.length)[,1], prob=TRUE, main="Quarter 1", ylab="", xlim=c(0,60), ylim=c(0,0.08), breaks=15)
lines(density(subset(ttrHke, Quarter==1, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,1))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==1), horizontal=TRUE, ylim=c(0,max(dh$Count, na.rm=T)))
par(mar=c(2,0,0,0))
boxplot(Weight ~ Quarter, data=subset(dh, Quarter==1), horizontal=TRUE, ylim=c(0,max(dh$Weight, na.rm=T)))
mtext("Total Hake Weight", side=3, line=2.5, font=2, cex=.8)
par(mar=c(0,3,2,1))
hist(subset(ttrHke, Quarter==2, select=Prey.length)[,1], prob=TRUE, main="Quarter 2", ylab="", xlim=c(0,60), ylim=c(0,0.08), breaks=30)
lines(density(subset(ttrHke, Quarter==2, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,1))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==2), horizontal=TRUE,ylim=c(0,max(dh$Count, na.rm=T)))
par(mar=c(2,0,0,0))
boxplot(Weight ~ Quarter, data=subset(dh, Quarter==2), horizontal=TRUE, ylim=c(0,max(dh$Weight, na.rm=T)))
par(mar=c(0,3,2,1))
hist(subset(ttrHke, Quarter==3, select=Prey.length)[,1], prob=TRUE, main="Quarter 3", ylab="", xlim=c(0,60), ylim=c(0,0.08), breaks=15)
lines(density(subset(ttrHke, Quarter==3, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,1))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==3), horizontal=TRUE,ylim=c(0,max(dh$Count, na.rm=T)))
par(mar=c(2,0,0,0))
boxplot(Weight ~ Quarter, data=subset(dh, Quarter==3), horizontal=TRUE, ylim=c(0,max(dh$Weight, na.rm=T)))
par(mar=c(0,3,2,1))
hist(subset(ttrHke, Quarter==4, select=Prey.length)[,1], prob=TRUE, main="Quarter 4", ylab="", xlim=c(0,60), ylim=c(0,0.08), breaks=30)
lines(density(subset(ttrHke, Quarter==4, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,1))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==4), horizontal=TRUE,ylim=c(0,max(dh$Count, na.rm=T)))
par(mar=c(2,0,0,0))
boxplot(Weight ~ Quarter, data=subset(dh, Quarter==4), horizontal=TRUE, ylim=c(0,max(dh$Weight, na.rm=T)))
par(oma=c(0,0,2,0))
mtext("Hake number, length distribution and total weight", side=3, line=0.5, outer=TRUE, font=2)
dev.off()
par=par(par.default)


