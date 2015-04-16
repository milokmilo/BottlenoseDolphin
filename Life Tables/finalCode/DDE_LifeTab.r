####################################################################################################
#                                   LIFE TABLES
#                 Life tables constructed for all fitted models 
#             (exponential, GLM Poisson and second-degree polynomial)
#               created: (camilo.saavedra@vi.ieo.es) 21/10/2013
#               modified: (camilo.saavedra@vi.ieo.es) 11/12/2013
####################################################################################################

# inputs
# "../../RData/CEMMA.csv"
# "../../RObjects/MExpStr0.RData"
# "../../RObjects/MExpStr1.RData"
# "../../RObjects/MExpStr2.RData"
# "../../RObjects/MExpStr3.RData"
# "../../RObjects/MglmPoisStr0.RData"
# "../../RObjects/MglmPoisStr1.RData"
# "../../RObjects/MglmPoisStr2.RData"
# "../../RObjects/MglmPoisStr3.RData"
# "../../RObjects/MIIPolStr0.RData"
# "../../RObjects/MIIPolStr1.RData"
# "../../RObjects/MIIPolStr2.RData"
# "../../RObjects/MIIPolStr3.RData"

# outputs
# "../../RObjects/lifeExp0.RData"
# "../../RObjects/lifeExp1.RData"
# "../../RObjects/lifeExp2.RData"
# "../../RObjects/lifeExp3.RData"
# "../../RObjects/lifeglmPois0.RData"
# "../../RObjects/lifeglmPois1.RData"
# "../../RObjects/lifeglmPois2.RData"
# "../../RObjects/lifeglmPois3.RData"
# "../../RObjects/lifeIIPol0.RData"
# "../../RObjects/lifeIIPol1.RData"
# "../../RObjects/lifeIIPol2.RData"
# "../../RObjects/lifeIIPol3.RData"
# "../../RObjects/lifeN.RData"
# "../plots/DDE_MortAge.png"
# "../plots/DDE_MortMod.png"
# "../../RObjects/ggLife.RData"


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
Str <-data.frame(age = as.numeric(rownames(Str)), N = Str[])
ages <- data.frame(age=c(0,seq(1:29))) # Creates a dataframe with all the ages
Str <- merge(ages, Str, by="age", all.x=TRUE) # Merges dataframes
Str[is.na(Str)] <- 0 # Replaces to zeros

# Listing all GLM models we have for strandings data
#StrMod <- list.files("../../RObjects", pattern="^glm")
#for (i in 1:length(StrMod)){
#  load(paste("../../RObjects/", StrMod[i], sep=""))
#  modName <- substr(StrMod[i], 1, nchar(StrMod[i])-6)
#  modVar <- substr(modName, 8, nchar(modName))
#  assign("Str1", `[[<-`(Str1, modVar, value=round(predict(get(modName), 
#                         list(age=Str1$age), type="response"),2)))   
#}

# Listing all the models we have for strandings data
StrMod <- list.files("../../RObjects/", pattern="M(.*)[0-9]")
for (i in 1:length(StrMod)){
  load(paste("../../RObjects/", StrMod[i], sep=""))
  modName <- substr(StrMod[i], 2, nchar(StrMod[i])-6)
  assign("Str", `[[<-`(Str, modName, value=round(predict(get(modName), 
                                list(age=Str$age), type="response"),2)))   
}

#Str # Age and real strandings data, and the predicted with the Poisson GLM

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

for (i in 2:length(Str)){
    lifeTab <- data.frame(age=Str[,1], M=Str[,i]) # mortality table
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
    assign(paste("life", names(Str)[i], sep=""),lifeTab)
    rm(lifeTab)
}

# Printing life tables 
lifeTabs1 <- ls(pattern="^life(.*)")
# removing lifeN
lifeTabs <- lifeTabs1[-length(lifeTabs1)]

#for (i in 1:length(lifeTabs)){
#  print(get(lifeTabs[i]))
#}

# save life tables
for (i in 1:length(lifeTabs1)){
  save(list=lifeTabs1[i],
  file=paste("../../RObjects/", lifeTabs1[i], ".RData", sep=""))
}
####################################################################################################

## Plotting Z mortality ##

# Computing ggData

for (i in 1:length(lifeTabs)){
  assign(lifeTabs[i], cbind(get(lifeTabs[i]), 
        mod=rep(substr(lifeTabs[i], 
              5, nchar(lifeTabs[i])-1), nrow(lifeN)),
              ag=rep(substr(lifeTabs[i], 
              nchar(lifeTabs[i]), nchar(lifeTabs[i])), nrow(lifeN)))) 
  if (i == 1) {ggLife <- get(lifeTabs[i])
  }else {
    ggLife <- rbind(ggLife, get(lifeTabs[i])) 
  }
}


# Saving ggLife for performing other plots
save(file="../../RObjects/ggLife.RData", ggLife)

# ggPloting

tit <- c("real stranding data", "GLM fitted", "Fitted removing two ages", 
         "Fitted removing three ages", "Fitted removing four ages")

gM1 <- ggplot(ggLife,aes(age,qx, group=ag)) + 
  geom_line(aes(colour=ag)) + ylim(0, 1) +
  facet_grid(mod ~ .) + 
#  geom_line(aes(age,lx), col=4) +
  ggtitle("DDE Mortality by Models (and Ages Removed)") +
  ylab("mortality-at-age") + xlab("ages") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top")

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_MortMod.png", width=800, height=600)
print(gM1)
dev.off()
#```


gM1b <- ggplot(ggLife,aes(age,qx, group=mod)) + 
  geom_line(aes(colour=mod)) + ylim(0, 1) +
  facet_grid(ag ~ .) + 
  #  geom_line(aes(age,lx), col=4) +
  ggtitle("DDE Mortality by Ages Removed (and Models)") +
  ylab("mortality-at-age") + xlab("ages")  +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top")

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_MortAge.png", width=800, height=600)
print(gM1b)
dev.off()
#```
