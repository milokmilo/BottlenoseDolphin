####################################################################################################
#                                      LIFE TABLES
#
#                   created:  (camilo.saavedra@vi.ieo.es) 21/10/2013
####################################################################################################

# IMPORTANT: Set working directory (to source file location)

# Call libraries 
library(ggplot2)

# Read data 
CEMMA <- read.csv("../../data/RData/CEMMA.csv")
ddePop <- CEMMA[CEMMA$sp == "DDE",c("age")]
ddePop <- ddePop[complete.cases(ddePop)]
# Changing age values (0.5 and 1.5 for 0 and 1)
ddePop[ddePop == 0.5] <- 0
ddePop[ddePop == 1.5] <- 1
# Calculating stranding frequencies by age 
Str <- table(ddePop)
Str <-data.frame(age = as.numeric(rownames(Str)), n = Str[])
ages <- data.frame(age=c(0,seq(1:29))) # Creates a dataframe with all the ages
Str <- merge(ages, Str, by="age", all.x=TRUE) # Merges dataframes
Str[is.na(Str)] <- 0 # Replaces to zeros
GLM <- Str

# Reading all the models we have for strandings data fitted with an exponential model
StrMod <- list.files("../../data/RObjects/StrMod", pattern="^Exp")
for (i in 1:length(StrMod)){
  load(paste("../../data/RObjects/StrMod/", StrMod[i], sep=""))
  modName <- substr(StrMod[i], 1, nchar(StrMod[i])-6)
  modVar <- substr(modName, 4, nchar(modName))
  assign("Str", `[[<-`(Str, modVar, value=round(predict(get(modName), 
                         list(age=Str$age), type="response"),2)))   
}
Str # Age and real strandings data, and the predicted with exponential models

# Reading all the models we have for strandings data fitted with an GLM Poisson model
GLMMod <- list.files("../../data/RObjects/StrMod", pattern="^glm")
for (i in 1:length(GLMMod)){
  load(paste("../../data/RObjects/StrMod/", GLMMod[i], sep=""))
  modName <- substr(GLMMod[i], 1, nchar(GLMMod[i])-6)
  modVar <- substr(modName, 8, nchar(modName))
  assign("GLM", `[[<-`(GLM, modVar, value=round(predict(get(modName), 
                        list(age=GLM$age), type="response"),2)))   
}
GLM # Age and actual strandings data, and the predicted GLM values 

######################################## LIFE TABLE ################################################

# age: real age of the dolphin
# M: Stranded-at-age
# S: Survivors-at-age
# Nx: Standardized survivors (by n=1000)
# dx: Death-at-age (standarized stranded dolphins) [N(x-1) - Nx] 
# qx: Death-at-age probability (probability of death by age) [dx / N(x-1)]
# lx: Survivors-at-age percent (how many survivors remain in each age) [Nx / N0]
# ex: Life expentancy at age (average years of life in each age) [∑ly/lx]
# Z: Total mortality (Natural mortality-M + Fishing mortality-F) [(-log(S)/(S-1))/t] 
#--------------------------------------------------------------------------------------------------#

for (i in 2:length(Str)){
    lifeTab <- data.frame(age=Str[,1], ageCl=Str[,1]+1, M=Str[,i]) # mortality table
    lifeTab <- rbind(c(NA, 0, 0), lifeTab) 
    n <- 1000
    # S - Creating survivorship column
    d <- sum(lifeTab$M) # sum of deaths
    for (j in 1:nrow(lifeTab)){
      lifeTab$S[[j]] <- d-sum(lifeTab$M[1:j])  
    } 
    # Nx - Standardizing survivors
    lifeTab$Nx <- round((lifeTab$S/d)*n,1) 
    # dx - Dolphins death-at-age N(x-1) - Nx
    #dx<-c(n-lifeTab$Nx[1])
    #for (j in 2:nrow(lifeTab)) {
    #  d <- round(lifeTab$Nx[j-1]-lifeTab$Nx[j],3)
    #  dx <- c(dx,d)
    #}
    #lifeTab$dx <- dx
    lifeTab$dx <- round((lifeTab$M/d)*n,1) # shorter alternative (we know the number of deaths by age)
    # qx - Death-at-age probability dx / N(x-1)
    qx <- c(lifeTab$dx[1]/n)
    for (j in 2:nrow(lifeTab)) {
      q <- round(lifeTab$dx[j]/lifeTab$Nx[j-1],3)
      qx <- c(qx,q)
    }
    lifeTab$qx <- qx 
    # lx - Survivorship-at-age percent Nx / N0
    lifeTab$lx <- lifeTab$Nx/n 
    # ex - Life expentancy at age ex = ∑ly/lx
    ex <- vector("numeric")
    for (j in 1:nrow(lifeTab)) {
      e <- round(sum(lifeTab$lx[j:nrow(lifeTab)])/lifeTab$lx[j],3)
      ex <- c(ex,e)
    }
    lifeTab$ex <- ex
    # Z - Total mortality-at-age -L(Nt/No)/t
    Z <- c(NA)
    for (j in 2:nrow(lifeTab)){
      z <- round(-log(lifeTab$S[j]/lifeTab$S[j-1])/1,2)
      Z <- c(Z,z)
    }
    lifeTab$Z <- Z
    assign(paste("LifeTab", names(Str)[i], sep=""),lifeTab)
}

# Printing life tables 
lifeTabs <- ls(pattern="^LifeTab")
for (i in 1:length(lifeTabs)){
  print(get(lifeTabs[i]))
}

####################################################################################################


## Plotting Z mortality ##

#tit <- c("real stranding data", "GLM fitted", "Fitted removing two ages", 
#         "Fitted removing three ages", "Fitted removing four ages")
#for (i in 1:length(lifeTabs)){
#  gM <- ggplot(get(lifeTabs[i]),aes(ageCl,Z)) + geom_line(col=2) + ylim(0, 1) +
#    geom_line(aes(ageCl,Z*0.4), lty=2, col=2) + geom_line(aes(ageCl,Z*0.6), lty=3, col=2) +
#    geom_line(aes(ageCl,lx), col=4) +
#    ylab("Mortality-at-age / Survivorship") + xlab("Years") + 
#    ggtitle(paste("DDE Z-Mortality", tit[i]))
#  print(gM)   
#}

for (i in 1:length(lifeTabs)){
  assign(lifeTabs[i], cbind(get(lifeTabs[i]), mod=rep(lifeTabs[i], nrow(lifeTab)))) 
  if (i == 1) {GGlifeTab <- get(lifeTabs[i])}
  GGlifeTab <- rbind(GGlifeTab, get(lifeTabs[i]))
}

gM <- ggplot(GGlifeTab,aes(ageCl,Z, group=mod)) + geom_line(col=2) + ylim(0, 1) +
  facet_grid(mod ~ .) + geom_line(aes(ageCl,Z*0.4), lty=2, col=2) + 
  geom_line(aes(ageCl,Z*0.6), lty=3, col=2) + geom_line(aes(ageCl,lx), col=4) +
  ylab("Mortality-at-age / Survivorship") + xlab("Years") + 
  ggtitle("DDE Survivorship (blue line) and Mortality: Z (red line), F (dotted line), M (dashed line)")
#```{r smallplot, fig.width=16, fig.height=8}
print(gM)
#```
png("../plots/DDE_SurvMort.png", width=1000, height=800)
print(gM)
dev.off()



##################################### Other M approaches ########################################### 

                  #_____________________# REPASAR #_____________________#


tmax <- 30

# Life table
apply(Mort[,-1],2,mean)
#lifeTab
-log(0.5)/4.8
apply(MortYngr[,-1],2,mean)
#lifeTabYgr
-log(0.5)/4.5

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



