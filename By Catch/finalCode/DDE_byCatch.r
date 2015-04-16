####################################################################################################
#                                   BY CATCH        
#           Splitting the total mortality in natural and fishing mortality by age. 
#                    Modeling and plotting this proportion (by age)
#                  created: (camilo.saavedra@vi.ieo.es) 21/10/2013
#                  modified: (camilo.saavedra@vi.ieo.es) 13/11/2013
#                  modified: (camilo.saavedra@vi.ieo.es) 18/12/2013
####################################################################################################

# inputs
# "../../RData/CEMMA.csv"

# outputs
# "../../RObjects/byCatch.RData"
# "../plots/DDE_StrndZMF.png"


# Charge libraries
library(reshape)
library(ggplot2)
library(grid)

# IMPORTANT: Set working directory (to source file location)

# Read data 
CEMMA <- read.csv("../../RData/CEMMA.csv")
byC <- CEMMA[CEMMA$sp == "DDE",c("age", "cod")]
byC <- byC[!is.na(byC$age),]
byC <- byC[!byC$cod=="",]
# Changing age values (0.5 and 1.5 for 0 and 1)
byC[byC == 0.5] <- 0
byC[byC == 1.5] <- 1


### HOUSE KEEPING ###

#levels(byC$cod)
#[1] ""            "AUTOLYSED"   "BY"          "BY-SURVIVED"
#[5] "EBY"         "LS-BY"       "LS-EBY"      "LS-NOT-BY"  
#[9] "LS-RELEASED" "NO EBY"      "UN"   

# Natural strandings and by-caught
nby <- byC[byC$cod =="NO EBY" | byC$cod =="LS-NOT-BY",]
#nby <- byC[byC$cod =="NO EBY" | byC$cod =="LS-NOT-BY" | # "unknown" really
#           byC$cod =="" | byC$cod =="AUTOLYSED" | 
#           byC$cod == "LS-RELEASED" | byC$cod == "UN",]
by <- byC[byC$cod =="EBY" | byC$cod =="LS-BY" | byC$cod =="BY" | 
            byC$cod =="LS-EBY" | byC$cod =="BY-SURVIVED",]

#nrow(nby)
# [1] 104
#nrow(by)
# [1] 138

age <- data.frame(age=0:29)

By <- as.data.frame(apply(table(by), 1, sum))
By <- data.frame(age=as.numeric(rownames(By)), n=By[[1]])
By <- merge(By, age, all.y=T)
By[is.na(By)] <- 0
names(By) <- c("age", "ByC")

NBy <- as.data.frame(apply(table(nby), 1, sum))
NBy <- data.frame(age=as.numeric(rownames(NBy)), n=NBy[[1]])
NBy <- merge(NBy, age, all.y=T)
NBy[is.na(NBy)] <- 0
names(NBy) <- c("age", "NByC")

byCatch <- merge(By, NBy)
byCatch$tot <- byCatch$ByC + byCatch$NByC
byCatch$ByPerc <- byCatch$ByC/sum(byCatch$tot)
byCatch$NByPerc <- byCatch$NByC/sum(byCatch$tot)
byCatch$totPerc <- byCatch$tot/sum(byCatch$tot)


### Fitting bycatch model ###

# Standardizing strandings, proportion of dead in each age class. 
byCatch$BySt <- byCatch$ByC/byCatch$tot 
byCatch$NBySt <- byCatch$NByC/byCatch$tot 
byCatch[is.na(byCatch)] <- 0

ages <- age$age

### gamma function

#deadByAge <- c(124.08,44.775,31.675,25.23,19.85,15.16,10.92,10.545,10.2,9.885,9.585,9.27,11.94,
#  11.48,13.725,15.63,17.185,18.28,18.945,21.01,22.1,22.125,22.5,22.44,20,18,15.12,
#  11.34,7.595,4.64)
#library(MASS) ## loading package MASS
#gamma <- fitdistr(deadByAge,"gamma") ## fitting gamma pdf parameters
#coef(gamma)

#ages <- 0:29
#dead <- data.frame(ages, deadByAge)
#glm(deadByAge~ages, data=dead, family="Gamma")


### cubic function (third-degree polynomial)
cub <- nls(BySt~b0+b1*age+b2*age^2+b3*age^3,data= byCatch, 
           start=list(b0=0,b1=1,b2=1, b3=1))
c <- coef(cub)
byCub <- c[1]+c[2]*ages+c[3]*ages^2+c[4]*ages^3

# Merging byCatch vector with data
byCatch$byCub <- byCub

# Saving byCatch data
save(file="../../RObjects/byCatch.RData", byCatch)


### ggPlotting ###

ggData1 <- melt(byCatch[,-c(2,3,4,8,9,10)], id.vars="age")
names(ggData1) <- c("age","mort","n")

ggData2 <- byCatch[,c("age","BySt","byCub")]

gg1 <- ggplot(ggData1, aes(age,n)) + 
  geom_line(aes(linetype=mort)) +
  scale_linetype_manual(values=c(3,2,1)) +
  ylab("percentage of strandings") + xlab("age") +
  ggtitle("Strandings: natural and fishing mortality") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top",
        plot.margin=unit(c(0.5,1,-1,0), "cm"))      

gg2 <- ggplot(ggData2, aes(age,BySt)) + 
  geom_point() + geom_line(aes(age,byCub)) +
  ylab("percentage of bycatch") + xlab("age") +
  ggtitle("Strandings: Percentage of bycaught strandings") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top",
      plot.margin=unit(c(0.5,1,0,0.1), "cm"))     


#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_StrndZMF.png", width=800, height=600)
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(5, 1))) # 5 rows, 1 column
print(gg1, vp = vplayout(1:3, 1))  # the first plot covers rows 1 to 3 and col 1
print(gg2, vp = vplayout(4:5, 1)) # the first plot covers rows 4 and 5 and col 1
dev.off()
#```





