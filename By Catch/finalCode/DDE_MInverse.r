####################################################################################################
#                                 PROOFS BYCATCH MORTALITY
#       Maximum level of total mortality than the population can accept to keep constant
#                   created: (camilo.saavedra@vi.ieo.es) 14/11/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 15/11/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 18/11/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 03/12/2013
####################################################################################################

# inputs
# "../../RObjects/M_ByC.RData"
# "../../RObjects/Z_ByC.RData"
# "../../RObjects/byCatch.RData"
# "../../RObjects/matFemC.RData"

# outputs
# "../plots/DDE_Minverse.png"
# "../plots/DDE_MinverseZoom.png"

# IMPORTANT: Set working directory (to source file location)

# Charge libraries
library(ggplot2)
library(reshape)
library(grid)

# Loading natural mortality (three slopes)
load(file="../../RObjects/M_ByC.RData")
# Loading total mortality (three slopes)
load(file="../../RObjects/Z_ByC.RData")

# Load byCatch vector
load("../../RObjects/byCatch.RData")
by <- c(byCatch$byCub,0)

# Loading maturity data
load("../../RObjects/matFemC.RData")
# Moving one age up and delete the las age
Mat <- Mat[-length(Mat)]
Mat <- c(0,Mat)


# Select some parameters
n <- 1000 # Initial population
p <- 15000 # Total population
preg <- c(4,3,2) # Pregnancy rates

# Function to construct the population structure from the mortality-at-age
fun <- function(x){
  laZ <- M + F * x
  life <- data.frame(age=0:30,qx=laZ)
  for (j in 1:nrow(life)){
    if (j == 1) {dx <- life$qx[1]*n; nx <- n
    } else {
      nx <- c(nx, nx[j-1]- dx[j-1])
      dx <- c(dx, life$qx[j]*nx[j])
    }
  }
  nx <- nx[-length(nx)]
  pop <- nx/sum(nx) * p 
  birth <- sum((pop/2)*Mat)/pr
  return(abs(1-(birth/pop[1]))) # relationship between number of births and number of dolphins of age 0
}

# Maximum bycatch allowable by reducing the relationship between births and age 0 
for (i in 2:length(M_ByC)){
  M <- M_ByC[,i] 
  F <- Z_ByC[,i] * (by * 1.25)
#  F <- Z_ByC[,2] - M_ByC[,2] 
  for (j in 1:length(preg)){
    pr <- preg[j]
    assign(paste(names(M_ByC[i]),"_", preg[j], sep=""), 
           optimize(f=fun, interval=c(-1,5)))
  }
} # We obtain the times that bycatch should be reduced

l <- ls(pattern="_[+0-9]")

# Maximum mortalities allowed 
names(M_ByC) <- c("age", "MSl075", "MSl125", "MSl1")
df <- Z_ByC
df <- merge (Z_ByC, M_ByC)
reduct <- data.frame(Zmort=NULL, perc=NULL)
for (i in 1:length(l)){
  pos <- match(substr(l[i], 1, nchar(l[i])-2), names(Z_ByC))
#  v <- M_ByC[,pos] + (Z_ByC[,pos] - M_ByC[,pos]) * get(l[i])[[1]]
  v <- M_ByC[,pos] + (Z_ByC[,pos] * (by * 1.25) * get(l[i])[[1]])
  assign("df",`[[<-` (df, l[i], value=v))
  reduct <- rbind (reduct, data.frame(Zmort=l[i], perc=round(get(l[i])[[1]],3)))
}

#df
#reduct

# Dataframes with mortality-at-age and population structure and with the number of the births #
dfpop <- data.frame(age=df[,1])
dfpreg <- data.frame(preg=round(100/preg))
for (i in 2:length(df)){
    life <- data.frame(age=0:30,qx=df[,i])
    for (j in 1:nrow(life)){
      if (j == 1) {dx <- life$qx[1]*n; nx <- n
      } else {
        nx <- c(nx, nx[j-1]- dx[j-1])
        dx <- c(dx, life$qx[j]*nx[j])
      }
    }
    #nx <- nx[-length(nx)]
    pop <- nx/sum(nx) * p
    assign("dfpop",`[[<-` (dfpop, paste("qx_", names(df[i]), sep=""),
                           value=df[,i]))
    assign("dfpop",`[[<-` (dfpop, paste("pop_", names(df[i]), sep=""),
                           value=pop))
    for (k in 1:length(preg)){
      if (k == 1){ 
        birth <- sum((pop[-length(pop)]/2)*Mat)/preg[k]} 
      else{
        birth <- c(birth, sum((pop[-length(pop)]/2)*Mat)/preg[k])  
      }
      if ( k == length(preg)){
        assign("dfpreg",`[[<-` (dfpreg, names(df[i]),
                             value=birth))
      }   
    }
}

#dfpop
#dfpreg

### ggPlotting ###

ggData1 <- melt(df, id.vars="age")
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

gg1 <- ggplot(ggData1, aes(age, value, group=variable)) + 
  geom_line(aes(colour=variable)) + ylim(c(0,1)) +
  scale_colour_manual(values=c(rep("#D55E00",3), rep("#000000",3), rep("#009E73",9))) +
  scale_linetype_manual(values=rep(c(2,1,2), 5)) + # doesn't work
  ylab("mortality") + xlab("age") +
  ggtitle("Maximum level of total mortalities") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top")

gg2 <- ggplot(ggData1, aes(age, value, group=variable)) + 
  geom_line(aes(colour=variable)) + ylim(c(0,.2)) + xlim(c(1,10)) +
  scale_colour_manual(values=c(rep("#D55E00",3), rep("#000000",3), rep("#009E73",9))) +
  scale_linetype_manual(values=rep(c(2,1,2), 5)) + # doesn't work
  ylab("") + xlab("") +
  ggtitle("") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="none",
        plot.margin=unit(c(0,0.5,-0.5,-0.5), "cm")) # package::grid needed

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_Minverse.png", width=800, height=600)
print(gg1)
dev.off()
#```

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_MinverseZoom.png", width=800, height=600)
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(8, 10))) # 5 rows, 1 column
print(gg1, vp = vplayout(1:8, 1:10))
print(gg2, vp = vplayout(2:5, 2:6))
dev.off()
#```

