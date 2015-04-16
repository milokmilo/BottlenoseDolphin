####################################################################################################
#                    GROWTH (MALES AND FEMALES) - COMMON DOLPHIN 
#             von Bertalanffy gorwth model and comparison between males and females
#              using only males and females, no the indeterminate dolphins 
#                  C.Saavedra (camilo.saavedra@vi.ieo.es) 03/09/2013
####################################################################################################

# inputs
# "../../RData/DDE_MalesFiona.csv"
# "../../RData/DDE_FemalesFiona.csv"

# outputs
# "../../RObjects/growth/vbAll.RData"
# "../../RObjects/growth/vbMale.RData"
# "../../RObjects/growth/vbFemale.RData"

# IMPORTANT: Set working directory (to source file location)

library(ggplot2)

# Rea data
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
save(file="../../RObjects/growth/vbAll.RData", vbAll)
save(file="../../RObjects/growth/vbMale.RData", vbMale)
save(file="../../RObjects/growth/vbFemale.RData", vbFemale)

# ploting model
plot(dde$t_age, dde$Lt_obs)
a <- dde$t_age
modAll <- coef(vbAll)[[1]]*(1-exp(-coef(vbAll)[[2]]*(a-(coef(vbAll)[[3]]))))
lines(spline(dde$t_age, modAll), col="red")
# Other option
#pframe <- data.frame(t_age=seq(1,30,length.out=300)) 
#pframe$Lt_obs <- predict(vbAll,newdata=pframe) 
#lines(spline(pframe), col="red")
modMale <- coef(vbMale)[[1]]*(1-exp(-coef(vbMale)[[2]]*(a-(coef(vbMale)[[3]]))))
lines(spline(dde$t_age, modMale), col="green")
modFemale <- coef(vbFemale)[[1]]*(1-exp(-coef(vbFemale)[[2]]*(a-(coef(vbFemale)[[3]]))))
lines(spline(dde$t_age, modFemale), col="blue")

# Ploting with ggplot
g0 <- ggplot(dde,aes(t_age,Lt_obs))+geom_point()
g2 <- g0 + geom_smooth(method="loess"); g2 # methods= lm, glm, gam, loess, rlm.
pframe <- data.frame(t_age=seq(1,30,length.out=300)) 
pframe$Lt_obs <- predict(vbAll,newdata=pframe)  # usando von Bertalanffy manualmente (sin SSasympOff )
g1 <- g0+geom_line(data=pframe); g1 


### Males, females and total ###
mf1 <- ggplot(dde,aes(t_age,Lt_obs))+geom_point(aes(colour=factor(sex))); mf1
mf2 <- mf1 + geom_smooth(method="loess", aes(colour=factor(sex))); mf2 # methods= lm, glm, gam, loess, rlm.
pframeA <- data.frame(t_age=seq(0,30,length.out=300)) 
pframeA$Lt_obs <- predict(vbAll,newdata=pframeA)  # usando von Bertalanffy manualmente (sin SSasympOff )
mf2.1 <- mf1 + geom_line(data=pframeA, colour="#333333"); mf2.1
pframeM <- data.frame(t_age=seq(0,30,length.out=300)) 
pframeM$Lt_obs <- predict(vbMale,newdata=pframeM)  # usando von Bertalanffy manualmente (sin SSasympOff )
mf2.2 <- mf2.1 + geom_line(data=pframeM, colour="#ff3333"); mf2.2
pframeF <- data.frame(t_age=seq(0,30,length.out=300)) 
pframeF$Lt_obs <- predict(vbFemale,newdata=pframeF)  # usando von Bertalanffy manualmente (sin SSasympOff )
mf2.3 <- mf2.2 + geom_line(data=pframeF, colour="#0099cc"); mf2.3
mf <- mf2.3 + ylab("length (cm)") + xlab("age") + ggtitle("von Bertalanffy growth model (males and females)")

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

# Calculamos los resíduos para Male, Female y todos juntos
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
# Estadístico F, Grados de libertad numerador = Nº parametros estimados (3), Grados de libertad denominador = DFmf

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
