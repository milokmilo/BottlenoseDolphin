####################################################################################################
#                    GROWTH (MALES AND FEMALES) - BOTTLENOSE DOLPHIN 
#          von Bertalanffy and Gompertz growth models for males and females
#               following Wells and Scott, 1999 and with our own data 
#                  C.Saavedra (camilo.saavedra@vi.ieo.es) 10/04/2015
####################################################################################################

# inputs
# "../../RData/DDE_MalesFiona.csv"
# "../../RData/DDE_FemalesFiona.csv"

# outputs
# "../../RObjects/vbA.RData"
# "../../RObjects/vbM.RData"
# "../../RObjects/vbF.RData"
# "../../RObjects/gzA.RData"
# "../../RObjects/gzM.RData"
# "../../RObjects/gzF.RData"
# "../../plots/TTR_vbSex-Wells&Scott.png"
# "../../plots/TTR_gzSex-Wells&Scott.png"


# IMPORTANT: Set working directory (to source file location)

library(ggplot2)


######## Growth model following Wells and Scott, 1999 ########
# See files with length/age relationships: "TTR-AgeLength.Males.txt" and "TTR-AgeLength.Females.txt"
TTR.M <- read.table("../../RData/TTR-AgeLength.Males.txt", comment.char="#", head=T)
range(TTR.M$Y)
[1]  140.7098 282.1363
TTR.F <- read.table("../../RData/TTR-AgeLength.Females.txt", comment.char="#", head=T)
range(TTR.F$Y)
[1]  134.0188 267.0540


# Starting values for parameters 
parVB <- list(Linf=290, K=0.18, to=-3) 
parGZ <- list(Linf=290, K=0.5, to=0.1) 

# Males
vbM <- nls(Y~Linf*(1-exp(-K*(X-to))), start=parVB, data=TTR.M)      
gzM <- nls(Y~Linf*exp(-K*exp(-X*to)), start=parGZ, data=TTR.M)      

# Females
vbF <- nls(Y~Linf*(1-exp(-K*(X-to))), start=parVB, data=TTR.F)      
gzF <- nls(Y~Linf*exp(-K*exp(-X*to)), start=parGZ, data=TTR.F)      

# All
TTR.A <- data.frame(X=c(TTR.M$X, TTR.F$X), Y=c(TTR.M$Y, TTR.F$Y), 
                    Sex=c(rep("M",length.out=nrow(TTR.M)), rep("F",length.out=nrow(TTR.F))))
vbA <- nls(Y~Linf*(1-exp(-K*(X-to))), start=parVB, data=TTR.A)      
gzA <- nls(Y~Linf*exp(-K*exp(-X*to)), start=parGZ, data=TTR.A)      

# Saving data
save(file="../../RObjects/vbA.RData", vbA)
save(file="../../RObjects/vbM.RData", vbM)
save(file="../../RObjects/vbF.RData", vbF)
save(file="../../RObjects/gzA.RData", gzA)
save(file="../../RObjects/gzM.RData", gzM)
save(file="../../RObjects/gzF.RData", gzF)


#### Ploting data with ggplot ####

# All ages loess smooth (polinomial)
g0 <- ggplot(TTR.A,aes(X,Y)) + geom_point()
g1 <- g0 + geom_smooth(method="loess") # methods= lm, glm, gam, loess, rlm.
#g1

# All ages von Bertalanffy
newVB <- data.frame(X=seq(1,50,length.out=500)) 
newVB$Y <- predict(vbA,newdata=newVB)  # predicting von Bertalanffy growth model
g2 <- g0 + geom_line(data=newVB) + labs(title="All bottlenose dolphins - von Bertalanffy \n")
#g2

# All ages Gompertz
newGZ <- data.frame(X=seq(1,50,length.out=500)) 
newGZ$Y <- predict(gzA,newdata=newGZ)  # predicting Gompertz growth model 
g3 <- g0 + geom_line(data=newGZ) + labs(title="All bottlenose dolphins - Gompertz \n")
#g3

# Males, Females and All 

mf1 <- ggplot(TTR.A, aes(X,Y)) + geom_point(aes(colour=factor(Sex))) +
  scale_colour_manual(values=c("orangered", "deepskyblue"))
#mf1
mf2 <- mf1 + geom_smooth(method="loess", aes(colour=factor(Sex))) # methods= lm, glm, gam, loess, rlm.
#mf2

# with von Bertalanffy

mf2.1 <- mf1 + geom_line(data=newVB, colour="black")
#mf2.1
newVB.M <- data.frame(X=seq(0,50,length.out=500)) 
newVB.M$Y <- predict(vbM,newdata=newVB.M)  
mf2.2 <- mf2.1 + geom_line(data=newVB.M, colour="deepskyblue")
#mf2.2
newVB.F <- data.frame(X=seq(0,50,length.out=500)) 
newVB.F$Y <- predict(vbF,newdata=newVB.F)  
mf2.3 <- mf2.2 + geom_line(data=newVB.F, colour="orangered")
#mf2.3
mf <- mf2.3 + ylab("length (cm) \n") + xlab("age") + 
  ggtitle("von Bertalanffy growth model (males and females) data from Wells & Scott 1999") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top")
png("../plots/TTR_vbSex-Wells&Scott.png", 700, 600)
print(mf)
dev.off()

# with Gompertz

mf2.1 <- mf1 + geom_line(data=newGZ, colour="black")
#mf2.1
newGZ.M <- data.frame(X=seq(0,50,length.out=500)) 
newGZ.M$Y <- predict(gzM,newdata=newGZ.M)  
mf2.2 <- mf2.1 + geom_line(data=newGZ.M, colour="deepskyblue")
#mf2.2
newGZ.F <- data.frame(X=seq(0,50,length.out=500)) 
newGZ.F$Y <- predict(gzF,newdata=newGZ.F)  
mf2.3 <- mf2.2 + geom_line(data=newGZ.F, colour="orangered")
#mf2.3
mf <- mf2.3 + ylab("length (cm) \n") + xlab("age") + 
  ggtitle("Gompertz growth model (males and females) data from Wells & Scott 1999") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top")
png("../plots/TTR_gzSex-Wells&Scott.png", 700, 600)
print(mf)
dev.off()







#############################################################################

#########   Own data  #########

# Reda data
ddeMales <- read.csv("../../RData/DDE_MalesFiona.csv")
ddeFemales <- read.csv("../../RData/DDE_FemalesFiona.csv")

# Add "sex" column and bind data
ddeMales <- data.frame(ddeMales, sex="Male")
ddeFemales <- data.frame(ddeFemales, sex="Female")
dde <- rbind(ddeMales, ddeFemales)

#FSA for 3.0 R version (para calcular condiciones iniciales)
#( svCom <- vbStarts(Lt_obs,t_age,data=ddeAll)) 
#( svGen <- lapply(svCom,rep,2) ) 

# Starting values for parameters 
par<-list(Linf=219, K=0.18, to=-3) 
# Growth model  with the parameters estimated with non linear least squares
vbAll<-nls(Lt_obs~Linf*(1-exp(-K*(t_age-to))), start=par, data=dde)      
vbMale<-nls(Lt_obs~Linf*(1-exp(-K*(t_age-to))), start=par, data=subset(dde, dde$sex=="Male"))      
vbFemale<-nls(Lt_obs~Linf*(1-exp(-K*(t_age-to))), start=par, data=subset(dde, dde$sex=="Female"))      

# Saving data
save(file="../../RObjects/vbAll.RData", vbAll)
save(file="../../RObjects/vbMale.RData", vbMale)
save(file="../../RObjects/vbFemale.RData", vbFemale)

# ploting model
#plot(dde$t_age, dde$Lt_obs)#
#a <- dde$t_age
#modAll <- coef(vbAll)[[1]]*(1-exp(-coef(vbAll)[[2]]*(a-(coef(vbAll)[[3]]))))
#lines(spline(dde$t_age, modAll), col="red")
# Other option
#pframe <- data.frame(t_age=seq(1,30,length.out=300)) 
#pframe$Lt_obs <- predict(vbAll,newdata=pframe) 
#lines(spline(pframe), col="red")
#modMale <- coef(vbMale)[[1]]*(1-exp(-coef(vbMale)[[2]]*(a-(coef(vbMale)[[3]]))))
#lines(spline(dde$t_age, modMale), col="green")
#modFemale <- coef(vbFemale)[[1]]*(1-exp(-coef(vbFemale)[[2]]*(a-(coef(vbFemale)[[3]]))))
#lines(spline(dde$t_age, modFemale), col="blue")

# Ploting with ggplot
g0 <- ggplot(dde,aes(t_age,Lt_obs))+geom_point()
g2 <- g0 + geom_smooth(method="loess") # methods= lm, glm, gam, loess, rlm.
#g2
pframe <- data.frame(t_age=seq(1,30,length.out=300)) 
pframe$Lt_obs <- predict(vbAll,newdata=pframe)  # usando von Bertalanffy manualmente (sin SSasympOff )
g1 <- g0+geom_line(data=pframe)
#g1

### Males, females and total ###
mf1 <- ggplot(dde,aes(t_age,Lt_obs))+geom_point(aes(colour=factor(sex)))
#mf1
mf2 <- mf1 + geom_smooth(method="loess", aes(colour=factor(sex))) # methods= lm, glm, gam, loess, rlm.
#mf2
pframeA <- data.frame(t_age=seq(0,30,length.out=300)) 
pframeA$Lt_obs <- predict(vbAll,newdata=pframeA)  # usando von Bertalanffy manualmente (sin SSasympOff )
mf2.1 <- mf1 + geom_line(data=pframeA, colour="#333333")
#mf2.1
pframeM <- data.frame(t_age=seq(0,30,length.out=300)) 
pframeM$Lt_obs <- predict(vbMale,newdata=pframeM)  # usando von Bertalanffy manualmente (sin SSasympOff )
mf2.2 <- mf2.1 + geom_line(data=pframeM, colour="#ff3333")
#mf2.2
pframeF <- data.frame(t_age=seq(0,30,length.out=300)) 
pframeF$Lt_obs <- predict(vbFemale,newdata=pframeF)  # usando von Bertalanffy manualmente (sin SSasympOff )
mf2.3 <- mf2.2 + geom_line(data=pframeF, colour="#0099cc")
#mf2.3
mf <- mf2.3 + ylab("length (cm)") + xlab("age") + 
  ggtitle("von Bertalanffy growth model (males and females)") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top")

png("../plots/vbSex.png")
print(mf)
dev.off()

# Estimated params, error and pValue
coef(summary(vbAll))
coef(summary(vbMale))
coef(summary(vbFemale))


### Comparison between males and females curves [Chen et al (1992) in Haddon, 2001]  ###

# Calculamos la longitud estimada para machos y hembras por separado y para el ajuste con todos juntos
ddeComp <- dde
ddeComp$Lt_estAll <- predict(vbAll, newdata=ddeComp$Lt_age)
ddeComp$Lt_est <- c(predict(vbMale, newdata=ddeComp[ddeComp$sex=="Male",]$Lt_age), 
                predict(vbFemale, newdata=ddeComp[ddeComp$sex=="Female",]$Lt_age))

# Calculamos los res�?duos para Male, Female y todos juntos
ddeComp$resAll <- (ddeComp$Lt_obs-ddeComp$Lt_estAll)^2
ddeComp$res <- (ddeComp$Lt_obs-ddeComp$Lt_est)^2

# Suma de cuadrados y grados de libertad
RSSAll <- sum(ddeComp$resAll)
RSSMale <- sum(ddeComp$res[ddeComp$sex=="Male"])
RSSFemale <- sum(ddeComp$res[ddeComp$sex=="Female"])
RSSmf <- RSSMale + RSSFemale
DFAll <- nrow(ddeComp)-length(coef(vbAll))
DFMale <- nrow(ddeComp[ddeComp$sex=="Male",])-length(coef(vbMale))
DFFemale <- nrow(ddeComp[ddeComp$sex=="Female",])-length(coef(vbFemale))
DFmf <- DFMale + DFFemale

F1 <- (RSSAll-RSSmf)/(DFAll-DFmf)/(RSSmf/DFmf)
F2 <- (RSSAll-RSSmf)/3*(2-1)/(RSSmf/(nrow(ddeComp)-(3*2))) 

### Distribución F ###
# Estad�?stico F, Grados de libertad numerador = Nº parametros estimados (3), Grados de libertad denominador = DFmf

pVal <- df(F1, length(coef(vbAll)), DFmf); pVal

# Test significativo - Las curvas son diferentes
pVal < 0.05

### Predecimos con el modelo la talla a cada edad (con el modelo general de todos los delfines) ###

pred <- data.frame(t_age=seq(0.5,29.5)) # Para las edades de 0 a 30
pred$Lt_est <- predict(vbAll, newdata=pred)
# Calculamos la desviación estándar por edad (solo para los valores observados)
sd <- sqrt(tapply(ddeComp$resAll, ddeComp$t_age, mean))
sd <- as.data.frame(sd)
sdbind <- data.frame(t_age=rownames(sd), sd=as.vector(round(sd$sd,1)))
#plot(sdbind$t_age, sdbind$sd)
popInit <- merge(pred, sdbind, by="t_age", all.x=TRUE)
popInit$Lt_est <- round(popInit$Lt_est,1) 
# Para introducir en las condiciones iniciales de la población
popInit ## INITIAL CONDITIONS ##
