####################################################################################################
#                    BOX PLOT BY QUARTER - COMMON DOLPHIN DIET (HAKE)
#              
#                    created: (camilo.saavedra@vi.ieo.es) 27/08/2013
#                    modified: (camilo.saavedra@vi.ieo.es) 03/12/2013
####################################################################################################

# inputs
# "../../RData/DDE_AllDiet.csv"

# outputs
# "../plots/DDE_hkeLen.png"

# IMPORTANT: Set working directory (to source file location)

# Leemos los datos
dde <- read.csv("../../RData/DDE_AllDiet.csv")

# Seleccionamos solo los que tienen merluza como presa
### OJO! No estamos incluyendo los que no tienen merluza pero si otro alimento ###
ddeHke <- dde[dde$Prey=="Hake",]
ddeHke$Prey.length <- ddeHke$Prey.length*0.1 # Pasamos la merluza a centímetros

# Separamos la fecha en tres columnas y creamos otra con los trimestres
ddeHke$Date <- as.Date(ddeHke$Date, "%d/%m/%Y")
ddeHke <- data.frame(ddeHke[,1:3],
                     year = as.numeric(format(ddeHke$Date, format = "%Y")),
                     month = as.numeric(format(ddeHke$Date, format = "%m")),
                     day = as.numeric(format(ddeHke$Date, format = "%d")),
                     ddeHke[,7:ncol(ddeHke)])
 
# Creamos un vector con los trimestres y lo añadimos al data.frame
quarter <- numeric()
for (i in 1:nrow(ddeHke)){
  if (ddeHke$month[i] %in% c(1,2,3)) {
    quarter <- c(quarter,1)
    }else{
      if (ddeHke$month[i] %in% c(4,5,6)){
        quarter <- c(quarter,2)
      }else{
        if(ddeHke$month[i] %in% c(7,8,9)){
          quarter <- c(quarter,3)
        }else{
          if(ddeHke$month[i] %in% c(10,11,12)){
            quarter <- c(quarter,4)
          }else{
            stop("error: month value doesn't exist")
          }
        }
      }
    } 
}

ddeHke <- data.frame(ddeHke[,1:6],
                     Quarter = quarter,
                     ddeHke[,8:ncol(ddeHke)])

# Hacemos una tabla con el número de merluzas en el estómado de cada delfín
dh <- data.frame(No=numeric(0), Count=numeric(0), Weight=numeric(0), Quarter=numeric(0))
for(i in unique(ddeHke$No)){
  n <- sum(as.numeric(unique(subset(ddeHke, No==i, select=Count))[,1]))/2 #si queremos el numero de merluzas
  q <- unique(subset(ddeHke, No==i, select=Quarter))[,1]
  w <- subset(ddeHke, No==i, select=c(Weighting,Prey.weight))
  w <- round(sum(w$Weighting * as.numeric(as.character(w$Prey.weight))),3)
  dh[nrow(dh)+1,] <- c(i,n,w,q) # El [1] hay que ponerlo para que en el DD722 coja el primer valor 12 y no el 1 (error DataEase)
}
dh$Count <- as.numeric(dh$Count)
dh$Weight <- as.numeric(dh$Weight)


## Ploteamos Número de merluzas y distribución de tallas juntos ##

par.default <- par(no.readonly=TRUE)
par(oma=c(3,3,3,3))  
zones=matrix(1:8, ncol=1, byrow=TRUE)
layout(zones, heights=rep(c(1/16, 2/16), 4))
par(mar=c(0,3,2,2))
#hist(subset(ddeHke, Quarter==1, select=Prey.length)[,1], prob=TRUE, main="Quarter 1", ylab="", xlab="", xlim=c(0,600), ylim=c(0,0.008))
ag <- subset(ddeHke, Quarter==1)[,c("Prey.length", "Weighting")]
a <-aggregate(ag$Weighting, list(cut(ag$Prey.length, breaks=seq(min(ag$Prey.length), 
                                                                  max(ag$Prey.length), 2))), sum)
bp<- barplot(t(as.matrix(a[,c("x")])), main="", axes=F, xlab="")
axis(1,at=bp,labels=a$Group.1)
axis(2,seq(0,max(a$x),5),seq(0,max(a$x),5))
#lines(density(subset(ddeHke, Quarter==1, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,2))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==1), horizontal=TRUE,ylim=c(0,max(dh$Count)), xlab="")
par(mar=c(0,3,2,2))
#hist(subset(ddeHke, Quarter==2, select=Prey.length)[,1], prob=TRUE, main="Quarter 2", ylab="", xlim=c(0,600), ylim=c(0,0.008))
ag <- subset(ddeHke, Quarter==2)[,c("Prey.length", "Weighting")]
a <-aggregate(ag$Weighting, list(cut(ag$Prey.length, breaks=seq(min(ag$Prey.length), 
                                                                max(ag$Prey.length), 2))), sum)
bp<- barplot(t(as.matrix(a[,c("x")])), main="", axes=F, xlab="")
axis(1,at=bp,labels=a$Group.1)
axis(2,seq(0,max(a$x),5),seq(0,max(a$x),5))
#lines(density(subset(ddeHke, Quarter==2, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,2))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==2), horizontal=TRUE,ylim=c(0,max(dh$Count)))
par(mar=c(0,3,2,2))
#hist(subset(ddeHke, Quarter==3, select=Prey.length)[,1], prob=TRUE, main="Quarter 3", ylab="", xlim=c(0,600), ylim=c(0,0.008))
ag <- subset(ddeHke, Quarter==3)[,c("Prey.length", "Weighting")]
a <-aggregate(ag$Weighting, list(cut(ag$Prey.length, breaks=seq(min(ag$Prey.length), 
                                                                max(ag$Prey.length), 2))), sum)
bp<- barplot(t(as.matrix(a[,c("x")])), main="", axes=F, xlab="")
axis(1,at=bp,labels=a$Group.1)
axis(2,seq(0,max(a$x),5),seq(0,max(a$x),5))
#lines(density(subset(ddeHke, Quarter==3, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,2))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==3), horizontal=TRUE,ylim=c(0,max(dh$Count)))
par(mar=c(0,3,2,2))
#hist(subset(ddeHke, Quarter==4, select=Prey.length)[,1], prob=TRUE, main="Quarter 4", ylab="", xlim=c(0,600), ylim=c(0,0.008))
ag <- subset(ddeHke, Quarter==4)[,c("Prey.length", "Weighting")]
a <-aggregate(ag$Weighting, list(cut(ag$Prey.length, breaks=seq(min(ag$Prey.length), 
                                                                max(ag$Prey.length), 2))), sum)
bp<- barplot(t(as.matrix(a[,c("x")])), main="", axes=F, xlab="")
axis(1,at=bp,labels=a$Group.1)
axis(2,seq(0,max(a$x),5),seq(0,max(a$x),5))
#lines(density(subset(ddeHke, Quarter==4, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,2))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==4), horizontal=TRUE,ylim=c(0,max(dh$Count)))
par(oma=c(0,0,2,0))
mtext("Hake number and length distribution", side=3, line=0.5, outer=TRUE, font=2)
par=par(par.default)


## Ordenado de tercero a segundo trimestre ##

par.default <- par(no.readonly=TRUE)
par(oma=c(3,3,3,3))  
zones=matrix(1:8, ncol=1, byrow=TRUE)
layout(zones, heights=rep(c(1/16, 2/16), 4))
par(mar=c(0,3,2,2))
hist(subset(ddeHke, Quarter==3, select=Prey.length)[,1], prob=TRUE, main="Quarter 3", ylab="", xlim=c(0,60), ylim=c(0,0.08), breaks=15)
lines(density(subset(ddeHke, Quarter==3, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,2))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==3), horizontal=TRUE,ylim=c(0,max(dh$Count)))
par(mar=c(0,3,2,2))
hist(subset(ddeHke, Quarter==4, select=Prey.length)[,1], prob=TRUE, main="Quarter 4", ylab="", xlim=c(0,60), ylim=c(0,0.08), breaks=30)
lines(density(subset(ddeHke, Quarter==4, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,2))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==4), horizontal=TRUE,ylim=c(0,max(dh$Count)))
par(mar=c(0,3,2,2))
hist(subset(ddeHke, Quarter==1, select=Prey.length)[,1], prob=TRUE, main="Quarter 1", ylab="", xlab="", xlim=c(0,60), ylim=c(0,0.08), breaks=15)
lines(density(subset(ddeHke, Quarter==1, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,2))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==1), horizontal=TRUE,ylim=c(0,max(dh$Count)), xlab="")
par(mar=c(0,3,2,2))
hist(subset(ddeHke, Quarter==2, select=Prey.length)[,1], prob=TRUE, main="Quarter 2", ylab="", xlim=c(0,60), ylim=c(0,0.08), breaks=30)
lines(density(subset(ddeHke, Quarter==2, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,2))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==2), horizontal=TRUE,ylim=c(0,max(dh$Count)))
par(oma=c(0,0,2,0))
mtext("Hake number and length distribution", side=3, line=0.5, outer=TRUE, font=2)
par=par(par.default)


## Ploteamos Número de otolitos y distribución de tallas juntos y añadimos peso de contenido de merluza por estómago ##

par.default <- par(no.readonly=TRUE)
png("../plots/DDE_hkeLen.png", height=600, width=900)
par(oma=c(3,3,3,3))  
zones=matrix(c(1,0,2,3,4,0,5,6,7,0,8,9,10,0,11,12), ncol=2, byrow=TRUE)
layout(zones, widths=c(4/5,1/5), heights=rep(c(1/16, 2/16), 4))
par(mar=c(0,3,2,1))
hist(subset(ddeHke, Quarter==1, select=Prey.length)[,1], prob=TRUE, main="Quarter 1", ylab="", xlim=c(0,60), ylim=c(0,0.08), breaks=15)
lines(density(subset(ddeHke, Quarter==1, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,1))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==1), horizontal=TRUE, ylim=c(0,max(dh$Count)))
par(mar=c(2,0,0,0))
boxplot(Weight ~ Quarter, data=subset(dh, Quarter==1), horizontal=TRUE, ylim=c(0,max(dh$Weight)))
mtext("Total Hake Weight", side=3, line=2.5, font=2, cex=.8)
par(mar=c(0,3,2,1))
hist(subset(ddeHke, Quarter==2, select=Prey.length)[,1], prob=TRUE, main="Quarter 2", ylab="", xlim=c(0,60), ylim=c(0,0.08), breaks=30)
lines(density(subset(ddeHke, Quarter==2, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,1))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==2), horizontal=TRUE,ylim=c(0,max(dh$Count)))
par(mar=c(2,0,0,0))
boxplot(Weight ~ Quarter, data=subset(dh, Quarter==2), horizontal=TRUE, ylim=c(0,max(dh$Weight)))
par(mar=c(0,3,2,1))
hist(subset(ddeHke, Quarter==3, select=Prey.length)[,1], prob=TRUE, main="Quarter 3", ylab="", xlim=c(0,60), ylim=c(0,0.08), breaks=15)
lines(density(subset(ddeHke, Quarter==3, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,1))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==3), horizontal=TRUE,ylim=c(0,max(dh$Count)))
par(mar=c(2,0,0,0))
boxplot(Weight ~ Quarter, data=subset(dh, Quarter==3), horizontal=TRUE, ylim=c(0,max(dh$Weight)))
par(mar=c(0,3,2,1))
hist(subset(ddeHke, Quarter==4, select=Prey.length)[,1], prob=TRUE, main="Quarter 4", ylab="", xlim=c(0,60), ylim=c(0,0.08), breaks=30)
lines(density(subset(ddeHke, Quarter==4, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,1))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==4), horizontal=TRUE,ylim=c(0,max(dh$Count)))
par(mar=c(2,0,0,0))
boxplot(Weight ~ Quarter, data=subset(dh, Quarter==4), horizontal=TRUE, ylim=c(0,max(dh$Weight)))
par(oma=c(0,0,2,0))
mtext("Hake number, length distribution and total weight", side=3, line=0.5, outer=TRUE, font=2)
dev.off()
par=par(par.default)


## Ordenado de tercero a segundo trimestre #

par.default <- par(no.readonly=TRUE)
par(oma=c(3,3,3,3))  
zones=matrix(c(1,0,2,3,4,0,5,6,7,0,8,9,10,0,11,12), ncol=2, byrow=TRUE)
layout(zones, widths=c(4/5,1/5), heights=rep(c(1/16, 2/16), 4))
par(mar=c(0,3,2,1))
hist(subset(ddeHke, Quarter==3, select=Prey.length)[,1], prob=TRUE, main="Quarter 3", ylab="", xlim=c(0,60), ylim=c(0,0.08), breaks=15)
lines(density(subset(ddeHke, Quarter==3, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,1))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==3), horizontal=TRUE,ylim=c(0,max(dh$Count)))
par(mar=c(2,0,0,0))
boxplot(Weight ~ Quarter, data=subset(dh, Quarter==3), horizontal=TRUE, ylim=c(0,max(dh$Weight)))
par(mar=c(0,3,2,1))
mtext("Total Hake Weight", side=3, line=2.5, font=2, cex=.8)
hist(subset(ddeHke, Quarter==4, select=Prey.length)[,1], prob=TRUE, main="Quarter 4", ylab="", xlim=c(0,60), ylim=c(0,0.08), breaks=30)
lines(density(subset(ddeHke, Quarter==4, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,1))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==4), horizontal=TRUE,ylim=c(0,max(dh$Count)))
par(mar=c(2,0,0,0))
boxplot(Weight ~ Quarter, data=subset(dh, Quarter==4), horizontal=TRUE, ylim=c(0,max(dh$Weight)))
par(mar=c(0,3,2,1))
hist(subset(ddeHke, Quarter==1, select=Prey.length)[,1], prob=TRUE, main="Quarter 1", ylab="", xlim=c(0,60), ylim=c(0,0.08), breaks=15)
lines(density(subset(ddeHke, Quarter==1, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,1))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==1), horizontal=TRUE, ylim=c(0,max(dh$Count)))
par(mar=c(2,0,0,0))
boxplot(Weight ~ Quarter, data=subset(dh, Quarter==1), horizontal=TRUE, ylim=c(0,max(dh$Weight)))
par(mar=c(0,3,2,1))
hist(subset(ddeHke, Quarter==2, select=Prey.length)[,1], prob=TRUE, main="Quarter 2", ylab="", xlim=c(0,60), ylim=c(0,0.08), breaks=30)
lines(density(subset(ddeHke, Quarter==2, select=Prey.length)[,1], adjust=2), lty="dotted")
par(mar=c(2,3,0,1))
boxplot(Count ~ Quarter, data=subset(dh, Quarter==2), horizontal=TRUE,ylim=c(0,max(dh$Count)))
par(mar=c(2,0,0,0))
boxplot(Weight ~ Quarter, data=subset(dh, Quarter==2), horizontal=TRUE, ylim=c(0,max(dh$Weight)))
par(oma=c(0,0,2,0))
mtext("Hake number, length distribution and total weight", side=3, line=0.5, outer=TRUE, font=2)
par=par(par.default)


