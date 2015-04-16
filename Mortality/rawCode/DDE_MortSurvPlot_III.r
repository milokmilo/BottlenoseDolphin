####################################################################################################
#                          MORTALITY SURVIVORSHIP PLOT
#
#               created: (camilo.saavedra@vi.ieo.es) 16/10/2013
####################################################################################################

# inputs
# "../../RObjects/lifeTables/lifeN.RData"
# "../../RObjects/lifeTables/lifeExp2.RData"
# "../../RObjects/lifeTables/lifeglmPois2.RData"
# "../../RObjects/lifeTables/lifeIIPol2.RData"
# "../../RObjects/lifeTables/lifeSiler2.RData"

# outputs
# "../plots/DDE_MortSuv.png"


# IMPORTANT: Set working directory (to source file location)

# Charge libraries
library(reshape)
library(ggplot2)

# Load data
load("../../RObjects/lifeTables/lifeN.RData")
load("../../RObjects/lifeTables/lifeExp2.RData")
load("../../RObjects/lifeTables/lifeglmPois2.RData")
load("../../RObjects/lifeTables/lifeIIPol2.RData")
load("../../RObjects/lifeTables/lifeSiler2.RData")


## House keeping ##

l <- ls(pattern="^life")

df <- data.frame(ages=0:29)
for (i in 1:length(l)){
  df <- cbind(df,round(get(l[i])[,c("nx")]/1000,3))
  df <- cbind(df,get(l[i])[,c("qx")])
}

names(df) <- c("ages", rep(l,each=2))
dfS <- df[,c(1,2,4,6,8,10)] 
dfM <- df[,c(1,3,5,7,9,11)] 

# ggData
ggS <- melt(dfS, id.vars="ages") 
ggM <- melt(dfM, id.vars="ages") 
names(ggS) <- c("ages", "variable", "S")
names(ggM) <- c("ages", "variable", "M")
gg <- merge(ggS, ggM)

# ggPlot
gg1 <- ggplot(gg,aes(ages,S, group=variable)) + 
  geom_line(aes(color=variable), lty=2) + 
  ggtitle("Predicted surivorship and mortality removing two age classes") + 
  ylab("survivorship / mortality") + xlab("age") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top")

gg2 <- gg1 + geom_line(aes(ages, M, color=variable))

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_MortSuv.png", width=800, height=600)
print(gg2)
dev.off()
#```

