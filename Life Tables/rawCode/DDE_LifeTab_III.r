####################################################################################################
#                                   LIFE TABLES
#                 Life tables constructed for all fitted models 
#             (exponential, Poisson GLM and second-degree polynomial)
#               created: (camilo.saavedra@vi.ieo.es) 21/10/2013
#               modified: (camilo.saavedra@vi.ieo.es) 11/12/2013
####################################################################################################

# inputs
# "../../RData/CEMMA.csv"
# "../../RObjects/StrMod/ExpStr0.RData"
# "../../RObjects/StrMod/ExpStr1.RData"
# "../../RObjects/StrMod/ExpStr2.RData"
# "../../RObjects/StrMod/ExpStr3.RData"
# "../../RObjects/StrMod/glmPoisStr0.RData"
# "../../RObjects/StrMod/glmPoisStr1.RData"
# "../../RObjects/StrMod/glmPoisStr2.RData"
# "../../RObjects/StrMod/glmPoisStr3.RData"
# "../../RObjects/StrMod/IIPolStr0.RData"
# "../../RObjects/StrMod/IIPolStr1.RData"
# "../../RObjects/StrMod/IIPolStr2.RData"
# "../../RObjects/StrMod/IIPolStr3.RData"

# outputs
# "../../RObjects/lifeTables/LifeTabExpStr0.RData"
# "../../RObjects/lifeTables/LifeTabExpStr1.RData"
# "../../RObjects/lifeTables/LifeTabExpStr2.RData"
# "../../RObjects/lifeTables/LifeTabExpStr3.RData"
# "../../RObjects/lifeTables/LifeTabglmPoisStr0.RData"
# "../../RObjects/lifeTables/LifeTabglmPoisStr1.RData"
# "../../RObjects/lifeTables/LifeTabglmPoisStr2.RData"
# "../../RObjects/lifeTables/LifeTabglmPoisStr3.RData"
# "../../RObjects/lifeTables/LifeTabIIPolStr0.RData"
# "../../RObjects/lifeTables/LifeTabIIPolStr1.RData"
# "../../RObjects/lifeTables/LifeTabIIPolStr2.RData"
# "../../RObjects/lifeTables/LifeTabIIPolStr3.RData"
# "../../RObjects/lifeTables/LifeTabn.RData"
# "../plots/DDE_MortAge.png"
# "../plots/DDE_MortMod.png"
# "../../RObjects/lifeTables/ggDataStrnd.RData"


# IMPORTANT: Set working directory (to source file location)

# Call libraries 
library(ggplot2)

# Read data 
CEMMA <- read.csv("../../RData/CEMMA.csv") # only necesary to create real strandings life table
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
#Str1 <- Str
Str2 <- Str

# Listing all GLM models we have for strandings data
#StrMod <- list.files("../../RObjects/StrMod", pattern="^glm")
#for (i in 1:length(StrMod)){
#  load(paste("../../RObjects/StrMod/", StrMod[i], sep=""))
#  modName <- substr(StrMod[i], 1, nchar(StrMod[i])-6)
#  modVar <- substr(modName, 8, nchar(modName))
#  assign("Str1", `[[<-`(Str1, modVar, value=round(predict(get(modName), 
#                         list(age=Str1$age), type="response"),2)))   
#}

# Listing all the models we have for strandings data
StrMod <- list.files("../../RObjects/StrMod", pattern="[0-9]")
for (i in 1:length(StrMod)){
  load(paste("../../RObjects/StrMod/", StrMod[i], sep=""))
  modName <- substr(StrMod[i], 1, nchar(StrMod[i])-6)
  assign("Str2", `[[<-`(Str2, modName, value=round(predict(get(modName), 
                                list(age=Str2$age), type="response"),2)))   
}

#Str2 # Age and real strandings data, and the predicted with the Poisson GLM

######################################## LIFE TABLE ################################################

# age: real age of the dolphin
# M: Stranded at age x
# S: Survivors at the start of the age x
# nx: Standardized survivors (by n=1000) [(S/∑M) * 1000]
# dx: Death-at-age (standarized stranded dolphins) [nx - n(x+1)]
# qx: Death-at-age probability (probability of death by age) [dx / nx]
# lx: Survivors-at-age percent (how many survivors remain in each age) [nx / n]
# ex: Life expentancy at age (average years of life in each age) [∑ly/lx]
# Z: Total mortality (Natural mortality-M + Fishing mortality-F) [(-log(S)/(S-1))/t] 
#--------------------------------------------------------------------------------------------------#

for (i in 2:length(Str2)){
    lifeTab <- data.frame(ageCl=Str2[,1], M=Str2[,i]) # mortality table
    #lifeTab <- rbind(c(NA, 0, 0), lifeTab) 
    n <- 1000
    # S - Creating survivorship column
    d <- sum(lifeTab$M) # sum of deaths
    for (j in 1:nrow(lifeTab)){
      lifeTab$S[[j]] <- d-sum(lifeTab$M[1:j-1])} 
    # nx - Standardizing survivors [(N/∑M) * 1000]
    lifeTab$nx <- round((lifeTab$S/d)*n,1) 
    # dx - Dolphins death-at-age [nx - n(x+1)]
    for (j in 1:nrow(lifeTab)) {
      if (j == 1) {dx <- vector("numeric")}
      d <- round(lifeTab$nx[j]-lifeTab$nx[j+1],3)
      if (j == nrow(lifeTab)) {d <- round(lifeTab$nx[j]-0,3)} 
      dx <- c(dx,d)}
    lifeTab$dx <- dx
    # qx - Death-at-age probability [dx / nx]
    for (j in 1:nrow(lifeTab)) {
      if (j == 1) {qx <- vector("numeric")}
      q <- round(lifeTab$dx[j]/lifeTab$nx[j],3)
      qx <- c(qx,q) }
    lifeTab$qx <- qx 
    # lx - Survivorship-at-age percent [nx / n]
    lifeTab$lx <- lifeTab$nx/n 
    # ex - Life expentancy at age ex = ∑ly/lx
    ex <- vector("numeric")
    for (j in 1:nrow(lifeTab)) {
      e <- round(sum(lifeTab$lx[j:nrow(lifeTab)])/lifeTab$lx[j],3)
      ex <- c(ex,e) }
    lifeTab$ex <- ex
    # Z - Total mortality-at-age -L(nt/no)/t
    Z <- c(NA)
    for (j in 1:nrow(lifeTab)){
      if (j == 1) {Z <- vector("numeric")}
      z <- round(-log(lifeTab$nx[j+1]/lifeTab$nx[j])/1,2)
      if (j == nrow(lifeTab)) {z <- 1.00}
      Z <- c(Z,z)
    }
    lifeTab$Z <- Z
 #   lifeTab <- lifeTab[-nrow(lifeTab),] 
    assign(paste("LifeTab", names(Str2)[i], sep=""),lifeTab)
}

# Printing life tables 
lifeTabs <- ls(pattern="^LifeTab")
#for (i in 1:length(lifeTabs)){
#  print(get(lifeTabs[i]))
#}

# save life tables
for (i in 1:length(lifeTabs)){
  assign("lifeTab", get(lifeTabs[i]))
  save(file=paste("../../RObjects/lifeTables/", lifeTabs[i], ".RData", sep=""), lifeTab)
}
####################################################################################################

## Plotting Z mortality ##

# Computing ggData

for (i in 1:length(lifeTabs)){
  assign(lifeTabs[i], cbind(get(lifeTabs[i]), mod=rep(lifeTabs[i], nrow(lifeTab)))) 
  if (i == 1) {GGlifeTab <- get(lifeTabs[i])}
  GGlifeTab <- rbind(GGlifeTab, get(lifeTabs[i]))
}

ggData <- GGlifeTab[!GGlifeTab$mod=="LifeTabn",]
ggData$mod <- as.character(ggData$mod)
ggData$mo <- substr(ggData$mod, 1, nchar(ggData$mod)-2)
ggData$ag <- substr(ggData$mod, nchar(ggData$mod), nchar(ggData$mod))

# Saving ggData for performing other plots
save(file="../../RObjects/lifeTables/ggDataLifeTab.RData", ggData)

# ggPloting

tit <- c("real stranding data", "GLM fitted", "Fitted removing two ages", 
         "Fitted removing three ages", "Fitted removing four ages")

gM1 <- ggplot(ggData,aes(ageCl,qx, group=mod)) + 
  geom_line(aes(colour=ag)) + ylim(0, 1) +
  facet_grid(mo ~ .) + 
#  geom_line(aes(ageCl,lx), col=4) +
  ggtitle("DDE Mortality by Models (and Ages Removed)") +
  ylab("mortality-at-age") + xlab("ages") +
  theme(legend.title=element_blank(), legend.position="none")
#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_MortMod.png", width=800, height=600)
print(gM1)
dev.off()
#```


gM1b <- ggplot(ggData,aes(ageCl,qx, group=mod)) + 
  geom_line(aes(colour=mo)) + ylim(0, 1) +
  facet_grid(ag ~ .) + 
  #  geom_line(aes(ageCl,lx), col=4) +
  ggtitle("DDE Mortality by Ages Removed (and Models)") +
  ylab("mortality-at-age") + xlab("ages")  +
  theme(legend.title=element_blank(), legend.position="none")
#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_MortAge.png", width=800, height=600)
print(gM1b)
dev.off()
#```
