####################################################################################################
#                           MORTALITY - SURVIVORSHIP CURVE
#            
#                  C.Saavedra (camilo.saavedra@vi.ieo.es) 26/09/2013
####################################################################################################

# IMPORTANT: Set working directory (to source file location)

# Charge libraries
library(ggplot2)

# Read data 
CEMMA <- read.csv("../../data/RData/CEMMA.csv")
ddePop <- CEMMA[CEMMA$sp == "DDE",c("age")]
ddePop <- ddePop[complete.cases(ddePop)]
# Changing age values (0.5 and 1.5 for 0 and 1)
ddePop[ddePop == 0.5] <- 0
ddePop[ddePop == 1.5] <- 1

MortAge <- table(ddePop)
MortAge <-data.frame(age = rownames(MortAge), n = MortAge[])


############ MORTALITY ####################

lifeTab <- data.frame(year=MortAge[,"age"], ageCl=MortAge[,"age"]+0.5, M=MortAge[,"n"]) # mortality table
# correcting lack of young animals (ages 0 and 1) 40 strandings assumed in each age
lifeTab$M[c(1,2,3)] <- c(60,50, 40)
ages <- data.frame(ageCl=seq(1:30)) # Create a dataframe with all the ages
lifeTab <- merge(ages, lifeTab, by="ageCl", all.x=TRUE) # Merge dataframes
lifeTab$year <- lifeTab$year[[1]]:lifeTab$year[[nrow(lifeTab)]] # Complete year column with all
lifeTab[is.na(lifeTab)] <- 0 # Replace to zeros
d <- sum(lifeTab$M) # sum of deaths
for (i in 1:nrow(lifeTab)){
  lifeTab$S[[i]] <- d-sum(lifeTab$M[1:i])  
} # Creating survivorship column
S0 <- lifeTab$M[[1]]+lifeTab$S[[1]]
lifeTab <- rbind(c(0,0,0, S0, 1000), lifeTab) # adding first age class
lifeTab$Nx <- round((lifeTab$S/d)*1000,1) # Standardized Survivorship
dx<-c(0)
for (i in 2:nrow(lifeTab)) {
  d <- round(lifeTab$Nx[i-1]-lifeTab$Nx[i],3)
  dx <- c(dx,d)
}
lifeTab$dx <- dx # dolphins death-at-age N(x-1) - Nx
qx<-c(0)
for (i in 2:nrow(lifeTab)) {
  q <- round(lifeTab$dx[i]/lifeTab$Nx[i-1],3)
  qx <- c(qx,q)
}
lifeTab$qx <- qx # death-at-age probability dx / N(x-1)
lifeTab$lx <- lifeTab$Nx/lifeTab$Nx[[1]] # survivorship-at-age percent Nx / N0
ex<-vector("numeric")
for (i in 1:nrow(lifeTab)) {
  e <- round(sum(lifeTab$lx[i:nrow(lifeTab)])/lifeTab$lx[i],3)
  ex <- c(ex,e)
}
lifeTab$ex <- ex # Life expentancy at age ex = ∑ly/lx
lifeTab

mean(lifeTab$qx) # Mortalidad media

## Z (Natural mortality + Fishing mortality) ##
Z <- round(-log(lifeTab$lx)/lifeTab$year,3)
Mort <- data.frame(year=lifeTab$year,Z)[c(-1,-nrow(lifeTab)),]
# Linear model
#lm <- lm(Z ~ year, data = Mort)
#summary(lm)
# Coefficients
#a <- round(summary(lm)$coefficients[1, 1], 4); a
#b <- round(summary(lm)$coefficients[2, 1], 4); b

gM <- ggplot(Mort,aes(year,Z)) + geom_point()  +
  ylab("Mortality-at-age") + xlab("Years") + 
  ggtitle("DDE Z-Mortality")
#line <- data.frame(year=seq(0,30,length.out=300)) 
#line$Z <- predict(lm,newdata=line) 
#gMf <- gM + geom_line(data=line, colour="red") + 
#  geom_text(data = NULL, x = 15, y = 0.1, label = paste("y=", a, "+", b,"x",sep=""), 
#            aes(family=c("mono")), hjust=0)
gM 

png("../../plots/ddeMort_ygr.png")
print(gM)
dev.off()


# Predicted values for Z
#Zval <- data.frame(year=seq(0,30)) 
#Zval$Z <- round(predict(lm,newdata=Zval),2) 
#FM <- 0.4
#Zval$F <- Zval$Z * FM # Fishing mortality: assuming 40% of fishing mortality
#Zval$M <- Zval$Z * (1-FM) # Natural mortality
#Zval # Nt=No*exp(-(F+M))

## Fitting survirvorship curve ##

m.e <- nls(Nx ~ I(exp(1)^(a + b * ageCl)), data = lifeTab, start = list(a = 0, b = 1), trace = T)
# Coefficients
summary(m.e)
a <- round(summary(m.e)$coefficients[1, 1], 4); a
b <- round(summary(m.e)$coefficients[2, 1], 4); b

gS <- ggplot(lifeTab,aes(ageCl,Nx)) + geom_point()  +
  ylab("Number of dolphins alive") + xlab("Years") + 
  ggtitle("DDE Survirvorship curve")
pframeA <- data.frame(ageCl=seq(0,30,length.out=300)) 
pframeA$Nx <- predict(m.e,newdata=pframeA) 
gSf <- gS + geom_line(data=pframeA, colour="red") + 
  geom_text(data = NULL, x = 0, y = 0, label = paste("y=e^(",a,"+(",b,")*x)",sep=""), 
            aes(family=c("mono")), hjust=0)
gSf


png("../../plots/ddeSurv_ygr.png")
print(gSf)
dev.off()


## M approaches comparison ## 
tmax <- 30

# Life table
ltZ <- mean(Zval$Z); ltZ 
ltM <- mean(Zval$M); ltM 
ltF <- mean(Zval$F); ltF 

## Hewit, D.A. & Hoenig, J.M. 2005

# Hoening (1983) 
H <- exp(1.44-0.982*log(tmax)); H
# aprox
Haprox <- 4.22/tmax; Haprox

# rule-of-thumb M=-ln(P)/tmax
rt <- -log(1/tmax)/tmax; rt #¿?
# FAO aprox (Sparre & Venema, 1998; Cadima, 2003)
rtaprox <- 3/tmax; rtaprox

# Fiona's value of 13% deaths-per-year
s <- 1-0.13
Fi <- -log(s); Fi
  
# Cubilos, L.A. (2003)  t*= to+(1/K)ln((3K/M)+1)
# puesto que M=3K/(exp((Kt*)-1))  y   t*=-(1/K)ln(1-w)  ya que w=1-exp(-Kt*)  entonces  M=3K(1-w)/w 
# OJO, NO funciona: Se calcula directamente de la k de von Bertalanffy si asumimos w como 0.62 pero si la K está mal estimada la M también
load("../../../DDE Growth/data/vbAll.RData")
#linf <- summary(vbAll)$parameters[1]
K <- summary(vbAll)$parameters[2]
to <- summary(vbAll)$parameters[3]
w <- 0.62 # se podría calcular un w específico para cetáceos conociendo Linf y L*
# with "to"
(3*K*exp(to)*(1-w))/(1-exp(to)+w*exp(to))
# without "to"
(3*K*(1-w))/w
1.839*K
