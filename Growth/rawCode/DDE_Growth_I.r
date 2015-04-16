####################################################################################################
#                             GROWTH - COMMON DOLPHIN 
#              
#               created: (camilo.saavedra@vi.ieo.es)
#               modified: (camilo.saavedra@vi.ieo.es) 09/11/2013
####################################################################################################

# inputs
# "../../RData/DDE_AllFiona.csv"
# "../../RData/DDE_MalesFiona.csv"
# "../../RData/DDE_FemalesFiona.csv"

# outputs
# "../../RObjects/growth/vbAll.RData"
# "../../RObjects/growth/vbMale.RData"
# "../../RObjects/growth/vbFemale.RData"

# IMPORTANT: Set working directory (to source file location)

# Charge libraries
library(ggplot2)

# Reading data
ddeAll <- read.csv("../../RData/DDE_AllFiona.csv")
ddeMales <- read.csv("../../RData/DDE_MalesFiona.csv")
ddeFemales <- read.csv("../../RData/DDE_FemalesFiona.csv")

#plot(ddeMales)
#plot(ddeFemales)

#FSA for 3.0 R version (para calcular condiciones iniciales)
#( svCom <- vbStarts(Lt_obs,t_age,data=ddeAll)) 
#( svGen <- lapply(svCom,rep,2) ) 

# These are the starting values for parameters 
par<-list(Linf=219, K=0.18, to=-3) 
# This is the growth model  with the parameters estimated with non linear least 
vb<-nls(Lt_obs~Linf*(1-exp(-K*(t_age-to))), start=par, data=ddeAll)      
#vb<-nls(Lt_obs~Linf*(1-exp(-exp(-K)*(t_age-to))), start=par, data=ddeAll) #SSasympOff Asym*(1-exp(-exp(lrc)*(input - c0)))

# Plotting
#plot(ddeAll)
#x <- ddeAll$t_age
#mod <- 207.6987*(1-exp(-0.2213*(x-(-3.8257))))
#lines(spline(x, mod))

### ggPlotting ###

#g0 <- ggplot(ddeAll,aes(t_age,Lt_obs))+geom_point()
#g2 <- g0 + geom_smooth(method="loess") # methods= lm, glm, gam, loess, rlm.
#(g2 <- g0 + geom_smooth(se=FALSE,method="auto",span=2)) # No sé cómo lo ajusta
#n1 <- nls(Lt_obs~SSasympOff(t_age,Asym,lrc,c0),data=ddeAll) 
#pframe <- data.frame(t_age=seq(1,30,length.out=300)) 

#Generamos una predicción con el modelo creado

#pframe$Lt_obs <- predict(vb,newdata=pframe)  # usando von Bertalanffy manualmente (sin SSasympOff )
#pframe$Lt_obs <- predict(n1,newdata=pframe) 
#(g1 <- g0+geom_line(data=pframe)) 



### Males, Females and Total dolphins ###

ddeAll <- data.frame(ddeAll, sex="All")
ddeMales <- data.frame(ddeMales, sex="Male")
ddeFemales <- data.frame(ddeFemales, sex="Female")
dde <- rbind(ddeAll, ddeMales, ddeFemales)

# These are the starting values for parameters 
par<-list(Linf=219, K=0.18, to=-3) 
# This is the growth model  with the parameters estimated with non linear least 
vbAll<-nls(Lt_obs~Linf*(1-exp(-K*(t_age-to))), start=par, data=subset(dde, dde$sex=="All"))
vbMale<-nls(Lt_obs~Linf*(1-exp(-K*(t_age-to))), start=par, data=subset(dde, dde$sex=="Male"))      
vbFemale<-nls(Lt_obs~Linf*(1-exp(-K*(t_age-to))), start=par, data=subset(dde, dde$sex=="Female"))      

# Saving data
save(file="../../RObjects/growth/vbAll.RData", vbAll)
save(file="../../RObjects/growth/vbMale.RData", vbMale)
save(file="../../RObjects/growth/vbFemale.RData", vbFemale)


### ggPloting ###

#g0 <- ggplot(dde,aes(t_age,Lt_obs))+geom_point(aes(colour=factor(sex)))
#g2 <- g0 + geom_smooth(aes(colour=factor(sex)))
#pframeA <- data.frame(t_age=seq(1,30,length.out=300)) 
#pframeA$Lt_obs <- predict(vbAll,newdata=pframeA)  # usando von Bertalanffy manualmente (sin SSasympOff )
#g1.1 <- g0+geom_line(data=pframeA, colour="red")
#pframeM <- data.frame(t_age=seq(1,30,length.out=300)) 
#pframeM$Lt_obs <- predict(vbMale,newdata=pframeM)  # usando von Bertalanffy manualmente (sin SSasympOff )
#g1.2 <- g1.1+geom_line(data=pframeM, colour="green")
#pframeF <- data.frame(t_age=seq(1,30,length.out=300)) 
#pframeF$Lt_obs <- predict(vbFemale,newdata=pframeF)  # usando von Bertalanffy manualmente (sin SSasympOff )
#(g1.3 <- g1.2+geom_line(data=pframeF, colour="blue"))

#vbAll
#vbMale
#vbFemale



