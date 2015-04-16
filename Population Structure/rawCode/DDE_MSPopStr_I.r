

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

# Population Structure

n <- 15000

popStr <- cbind(ages=dfS[,1], 
               round(apply(dfS[,-1], 2, function(x)x/sum(x)*n)))
popStr <- as.data.frame(dfPop)

# Saving data
save(popStr, file="../../RObjects/popStr/popStr.RData")



#############################################################

# Recruitment

# Loading maturity data
load("../../RObjects/maturity/matFemC.RData")

# Total maturing females at the population that could get birth next year (remove last age class)
Mat <- Mat[-length(Mat)]
Mat <- c(0,Mat)
totMatFem <- apply(popStr[,-1], 2, function(x)sum(x/2*Mat))

# Number of births with three different pregnancy rates (25%, 33%, 50%)
rec <- rbind(totMatFem/4, totMatFem/3, totMatFem/2)
rec <- round(rec) 
rownames(rec) <- c("25%","33%","50%")

rec




