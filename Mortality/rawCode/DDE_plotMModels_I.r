####################################################################################################
#                          PLOT SEVERAL MORALITY MODELS
#
#               created: (camilo.saavedra@vi.ieo.es) 16/10/2013
####################################################################################################

# input
# "../../RObjects/ggDataStrnd.RData"
# "../../RObjects/ggDataLifeTab.RData"

# output
# "../plots/DDE_MModels.png"
# "../plots/DDE_MAges.png"

# IMPORTANT: Set working directory (to source file location)

# Charge libraries
library(ggplot2)

# Loading predicted strandings data 
load("../../RObjects/StrMod/ggDataStrnd.RData")

# Plot
tit <- c("all stranding ages","one age removed","two ages removed","three ages removed")
lab <- function(data,value){return(tit[value])}
gg1 <- ggplot(ggData, aes(age, n)) + 
  geom_point(colour = "black", size = 2) +
  facet_grid(data ~ ., labeller=lab) + 
  geom_line(aes(age, pred, colour=mod, group=mod)) +
  ggtitle("Predicted strandings by fitting different models") + 
  ylab("number of strandings") + xlab("age") +
  theme(legend.title=element_blank(), legend.position = "none")
#        theme_bw() # Legend problem (can't remove)

# Including removed values as circle
dat <- subset(ggData, data=="Str0" & mod=="Exp", select=c(age,n))
gg1b <- gg1 +  geom_point(data = dat, colour = "black", size = 1.5, pch=1)

#```{r smallplot, fig.width=16, fig.height=8}
#png("../plots/DDE_predStrd.png", width=800, height=600)
print(gg1b)
#dev.off()
#```

###################################################################################

# Loading life tables data 
load(file="../../RObjects/lifeTables/ggDataLifeTab.RData")
# Loading Siler life table data
lifeS <- list.files("../../RObjects/lifeTables", pattern="^lifeSiler[0-9]")
for (i in 1:length(lifeS)){
  load(file=paste("../../RObjects/lifeTables/", lifeS[i], sep=""))
  assign(substr(lifeS[i], 1, 10), lifeSiler)
  }


# Computing and merging data
ggData <- ggData[,c("ageCl", "qx", "mod", "mo", "ag")]
lifeS <- ls(pattern="^lifeSiler[0-9]")
for (i in 1:length(lifeS)){
  assign(lifeS[i], get(lifeS[i])[,c("ageCl", "qx")])
  assign(lifeS[i], `[[<-` (get(lifeS[i]), "mod", value=lifeS[i]))
  assign(lifeS[i], `[[<-` (get(lifeS[i]), "mo", value=substr(lifeS[i], 1,9)))
  assign(lifeS[i], `[[<-` (get(lifeS[i]), "ag", value=substr(lifeS[i], 10,10)))
}
ggSiler <- lapply(1:4, FUN=function(x) get(lifeS[x]))
ggSiler <- do.call(rbind,lapply(ggSiler, function(df) df[,]))

ggData <- rbind(ggData, ggSiler)

################################ GG-PLOTING ################################

gM1a <- ggplot(ggData,aes(ageCl,qx, group=mod)) + 
  geom_line(aes(colour=ag)) + ylim(0, .55) +
  facet_grid(mo ~ .) + 
  #  geom_line(aes(ageCl,lx), col=4) +
  ggtitle("") +
  ylab("") + xlab("") +
  scale_x_continuous(limits=c(0,10), breaks=c(0,5,10)) +
  theme(legend.position="none",
#      axis.text.x=element_blank(),
#      axis.ticks.x=element_blank(),
      plot.margin=unit(c(1,0,1,1), "cm"),
        strip.text.y = element_blank() , 
        strip.background = element_blank()
        )

labeli <- function(variable, value){
  names_li <- list("lifeSiler" = "Siler",
                   "LifeTabExpStr"="Exponential", 
                   "LifeTabglmPoisStr"="GLM Poisson", 
                   "LifeTabIIPolStr"="II Polynomial")
  return(names_li[value])
}

gM1b <- ggplot(ggData,aes(ageCl,qx, group=mod)) + 
  geom_line(aes(colour=ag)) + ylim(0, 1) + xlim(10,30) +
  facet_grid(mo ~ . , labeller=labeli) + 
  #  geom_line(aes(ageCl,lx), col=4) +
  ggtitle("") +
  ylab("") + xlab("") +
  theme(legend.position="none",
        plot.margin=unit(c(1,1,1,-0.5), "cm"),
        strip.text.y = element_text(size = 12)
        )

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_MModels.png", width=1000, height=600)
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3))) # 1 rows, 3 columns
print(gM1a, vp = vplayout(1, 1))  # the first plot covers rows 1 and col 1
print(gM1b, vp = vplayout(1, 2:3)) # the first plot covers rows 1 and col 1
grid.text("ages", just = c("centre", "bottom"),
          x = unit(.5, "npc"), y = unit(.025, "npc"),  
          gp = gpar(fontface = "bold", fontsize = 14, col = "grey30"))
grid.text("mortality-at-age", just = c("left"),
          x = unit(.02, "npc"), y = unit(.4, "npc"), rot=90, 
          gp = gpar(fontface = "bold", fontsize = 14, col = "grey30"))
grid.text("DDE Comparison between total Mortality estimated by three models (different dataset sizes by removing first ages)", 
          x = unit(.52, "npc"), y = unit(.98, "npc"), just = c("centre", "top"), 
          gp = gpar(fontface = "bold", fontsize = 17, col = "grey20"))
dev.off()
#```


################################ GG-PLOTING ################################

gM2a <- ggplot(ggData,aes(ageCl,qx, group=mod)) + 
  geom_line(aes(colour=mo)) + ylim(0, .55) +
  facet_grid(ag ~ .) + 
  #  geom_line(aes(ageCl,lx), col=4) +
  ggtitle("") +
  ylab("") + xlab("") +
  scale_x_continuous(limits=c(0,10), breaks=c(0,5,10)) +
  theme(legend.position="none",
        plot.margin=unit(c(1,0,1,1), "cm"),
        strip.text.y = element_blank() , 
        strip.background = element_blank()
  )
  
labelii <- function(variable, value){
  names_lii <- list("0"="No ages removed", 
                   "1"="Two age removed", 
                   "2"="Three ages removed",
                   "3"= "Four ages removed")
  return(names_lii[value])
}

gM2b <- ggplot(ggData,aes(ageCl,qx, group=mod)) + 
  geom_line(aes(colour=mo)) + ylim(0, 1) + xlim(10,30) +
  facet_grid(ag ~ . , labeller=labelii) + 
  #  geom_line(aes(ageCl,lx), col=4) +
  ggtitle("") +
  ylab("") + xlab("") +
  theme(legend.position="none",
        plot.margin=unit(c(1,1,1,-0.5), "cm"),
        strip.text.y = element_text(size = 12)
  )  

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_MAges.png", width=1000, height=600)
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3))) # 1 rows, 3 columns
print(gM2a, vp = vplayout(1, 1))  # the first plot covers rows 1 and col 1
print(gM2b, vp = vplayout(1, 2:3)) # the first plot covers rows 1 and col 1
grid.text("ages", just = c("centre", "bottom"),
          x = unit(.5, "npc"), y = unit(.025, "npc"),  
          gp = gpar(fontface = "bold", fontsize = 14, col = "grey30"))
grid.text("mortality-at-age", just = c("left"),
          x = unit(.02, "npc"), y = unit(.4, "npc"), rot=90, 
          gp = gpar(fontface = "bold", fontsize = 14, col = "grey30"))
grid.text("DDE Comparison between total Mortality estimated from four different dataset sizes (three models fitted)", 
          x = unit(.52, "npc"), y = unit(.98, "npc"), just = c("centre", "top"), 
          gp = gpar(fontface = "bold", fontsize = 17, col = "grey20"))
dev.off()
#```

##################################################################################################
