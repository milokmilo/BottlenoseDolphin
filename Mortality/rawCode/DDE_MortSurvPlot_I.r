

library(reshape)
library(ggplot2)

load("../../RObjects/lifeTables/lifeN.RData")
load("../../RObjects/lifeTables/lifeExp2.RData")
load("../../RObjects/lifeTables/lifeglmPois2.RData")
load("../../RObjects/lifeTables/lifeIIPol2.RData")
load("../../RObjects/lifeTables/lifeSiler2.RData")

l <- ls(pattern="^life")

df <- data.frame(ages=0:29)
for (i in 1:length(l)){
  df <- cbind(df,round(get(l[i])[,c("nx")]/1000,3))
  df <- cbind(df,get(l[i])[,c("qx")])
}

names(df) <- c("ages", rep(l,each=2))
dfS <- df[,c(1,2,4,6,8,10)] 
dfM <- df[,c(1,3,5,7,9,11)] 

ggS <- melt(dfS, id.vars="ages") 
ggM <- melt(dfM, id.vars="ages") 
names(ggS) <- c("ages", "variable", "S")
names(ggM) <- c("ages", "variable", "M")
gg <- merge(ggS, ggM)


gg1 <- ggplot(gg,aes(ages,S, group=variable)) + 
  geom_line(aes(color=variable), lty=2) + 
  ggtitle("Predicted surivorship and mortality removing two age classes") + 
  ylab("survivorship / mortality") + xlab("age") +
  theme(legend.position="none")

gg2 <- gg1 + geom_line(aes(ages, M, color=variable))

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_MortSuv.png", width=800, height=600)
print(gg2)
dev.off()
#```


###########################################################
# Population
for (i in 1:length(l)){
  dfP <- cbind(df,round(get(l[i])[,c("nx")]/1000,3))
}

dfS

# Population structure actual data
nStr <- LifeTabn$nx/sum(LifeTabn$nx) * n
# Population structure GLM (removing 3 age clases)
GLM01Str <- LifeTabStr01$nx/sum(LifeTabStr01$nx) * n
# Population structure Siler
Siler01Str <- lifeSiler01$nx/sum(lifeSiler01$nx) * n
# Population structure Siler No ByCatch
#Siler00NByCStr <- lifeSiler00NByC$nx/sum(lifeSiler00NByC$nx) * n

# Updated life tables (total population = 15000)
LifeTabn$nx <- nStr
LifeTabStr01$nx <- GLM01Str
lifeSiler01$nxS <- Siler01Str
#lifeSiler00NByC$nxS <- Siler00NByCStr

# N-by-age with the different models
age <- c(0,seq(1:29))
popStr <- data.frame (age=age, n=round(nStr,0), 
                      GLM=round(GLM01Str,0), 
                      Siler=round(Siler01Str, 0)
                      #                SilerNByC=round(Siler00NByCStr, 0)
)

# Saving data
save(popStr, file="../../RObjects/popStr/popStr.RData")




