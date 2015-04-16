####################################################################################################
#                               TOTAL AND NATURAL MODELS                            
#             Natural and total mortality with different bycatch levels
#                   created: (camilo.saavedra@vi.ieo.es) 14/11/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 15/11/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 18/11/2013
####################################################################################################

# inputs
# "../../RObjects/Sparams0.RData"
# "../../RObjects/Sparams1.RData"
# "../../RObjects/Sparams2.RData"
# "../../RObjects/Sparams3.RData"
# "../../RObjects/byCatch.RData"
# "../../RObjects/matFemC.RData"

# outputs
# "../../RObjects/bayRec.RData"
# "../../RObjects/bayPopZ.RData"
# "../plots/DDE_MortZ3M.png"
# "../plots/DDE_MortZ3MZoom.png"
# "../../RObjects/Z_ByC.RData"
# "../../RObjects/M_ByC.RData"

# IMPORTANT: Set working directory (to source file location)

# Charge libraries
library(ggplot2)
library(reshape)

params <- list.files("../../RObjects/", pattern="^Sparams[0-9]")

# a1*exp(b1*ages)+a2+a3*exp(b3*ages)
ages <- 0:30

for (i in 1:length(params)){
    load(paste("../../RObjects/", params[i], sep=""))
}

### ALL ANALYSIS DONE WITH SILER2 (TWHO AGES REMOVED) ###

# Params with juvenile mortality variations
par <- c("a","b","c")
prop <- c(1.25, 1, 0.75)

# for...
a <- c(Sparams2[1]*prop[1], Sparams2[2], Sparams2[3:5])  
b <- c(Sparams2[1]*prop[2], Sparams2[2], Sparams2[3:5])  
c <- c(Sparams2[1]*prop[3], Sparams2[2], Sparams2[3:5]) 

# Mortality vector for each parameter variation
for (i in 1:length(par)){
  p <- get(par[i])
  assign(paste("S", par[i], sep=""),
         p[1]*exp(p[2]*ages)+p[3]+p[4]*exp(p[5]*ages))
}
S1 <- ls(pattern="^S[a-z]$")

# Save to Zmax analysis
Z_ByC <- data.frame(age=ages, Sl075=Sc, Sl125=Sa, Sl1=Sb)
save(file="../../RObjects/Z_ByC.RData", Z_ByC)


### byCatch mortality vector ##

# Load byCatch vector
load("../../RObjects/byCatch.RData")
by <- c(byCatch$byCub,0)
#nby <- 1-by
#1-by * prop[1]
for (i in 1:length(S1)){
  for (j in 1:length(prop)){
    assign(paste(S1[i], par[j], sep=""), 
    get(S1[i]) * (1 - by * prop[j]))
  }
}

S2 <- ls(pattern="^S[a-z]+$")

# Save to Zmax analysis
M_ByC <- data.frame(age=ages, Sl075=Sca, Sl125=Saa, Sl1=Sba)
save(file="../../RObjects/M_ByC.RData", M_ByC)


# TO ESTIMATE COEFFICIENTS OF THE MODELS
df <- data.frame(ages,Saa)
Silerdf <- nls(Saa ~ b0*exp(b1*ages)+b2+b3*exp(b4*ages), data=df,
               start=list(b0=0.1,b1=-2.5,b2=0.1, b3=0.01, b4=0.1))
c <- coef(Silerdf)
est <- c[[1]]*exp(c[[2]]*ages)+c[[3]]+c[[4]]*exp(c[[5]]*ages)

# NEW life table for the testing parametres 
for (k in 1:length(S2)) {
    lifeSiler <- data.frame(age=0:30,qx=get(S2[k]))
  n <- 1000
  # nx and dx - survivors and deaths at age
  for (j in 1:nrow(lifeSiler)){
    if (j == 1) {dx <- lifeSiler$qx[1]*n; nx <- n
    } else {
      nx <- c(nx, nx[j-1]- dx[j-1])
      dx <- c(dx, lifeSiler$qx[j]*nx[j])
    }
  }
  lifeSiler <- data.frame(lifeSiler, nx=round(nx,5), dx=round(dx,5))
  # lx - Survivorship-at-age percent 
  lifeSiler$lx <- lifeSiler$nx/n 
  # ex - Life expentancy at age ex = ∑lx/lx
  ex <- vector("numeric")
  for (j in 1:nrow(lifeSiler)) {
    e <- round(sum(lifeSiler$lx[j:nrow(lifeSiler)])/lifeSiler$lx[j],3)
    ex <- c(ex,e) }
  lifeSiler$ex <- ex
  # Z - Total mortality-at-age -L(nt/no)/t
  Z <- c(NA)
  for (j in 1:nrow(lifeSiler)){
    if (j == 1) {Z <- vector("numeric")}
    z <- round(-log(lifeSiler$nx[j+1]/lifeSiler$nx[j])/1,2)
    # Correction for the last mortality
    if (j == nrow(lifeSiler)) {z <- NA}
    Z <- c(Z,z)
  }
  lifeSiler$Z <- Z
  lifeSiler <- lifeSiler[-nrow(lifeSiler),]
  assign(paste("life",S2[k], sep=""), lifeSiler)
  rm(lifeSiler)
}
lifeS <- ls(pattern="^lifeS[a-z]+$")


### Population structure ###

n <- 15000
for (i in 1:length(lifeS)){
  assign(paste("pop", substr(lifeS[i], 5, nchar(lifeS[i])), sep=""), 
         get(lifeS[i])[,"nx"]/sum(get(lifeS[i])[,"nx"]) * n )
} 
pop <- ls(pattern="^popS[a-z]+$")

### Recruitment ###

# Loading maturity data
load("../../RObjects/matFemC.RData")

# Moving one age up and delete the las age
Mat <- Mat[-length(Mat)]
Mat <- c(0,Mat)

for (i in 1:length(pop)){
  mats <- get(pop[i])/2*Mat
  mats <- sum(mats)
  rec <- rbind(mats/4, mats/3, mats/2)
  rec <- round(rec) 
  rownames(rec) <- c("25%","33%","50%")
  assign(paste("rec", substr(pop[i], 4, nchar(pop[i])), sep=""),rec)
}
recs <- ls(pattern="^recS[a-z]+$")


### Constructing dataframe with the results ###

bayRec <- data.frame(rec=c("25%","33%","50%"))
bayPopZ <- data.frame(age=0:29)

for (i in 1:length(lifeS)){
  bayRec <- cbind(bayRec,get(recs[i]))
  bayPopZ <- cbind(bayPopZ,round(get(pop[i])),get(lifeS[i])[,"qx"]) 
}

names(bayRec) <- c("rec", substr(recs, nchar(recs)-2, nchar(recs))) 
names(bayPopZ) <- c("age", paste(rep(substr(pop, 4, nchar(pop)), each=2),
                   rep(c("Pop","qx"), length(par)), sep=""))

# Saving data frames
save(file="../../RObjects/MTest/bayRec.RData", bayRec)
save(file="../../RObjects/MTest/bayPopZ.RData", bayPopZ)


### ggPlotting ###

ggData <- bayPopZ[,seq(1, length(bayPopZ), by=2)]
ggData <- melt(ggData, id.vars="age")

gg1 <- ggplot(ggData, aes(age, value, group=variable)) + 
  geom_line(aes(colour=variable, linetype=variable)) + ylim(0,1) +
  scale_color_manual(values=rep(c("orangered","royalblue","royalblue4","royalblue"),4)) +
  scale_linetype_manual(values=rep(c(2,1,2,2),each=4)) +
  ylab("percentage of dead dolphins") + xlab("age") +
  ggtitle("Mortality-at-age: three natural and one total mortality") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="top")

gg2 <- ggplot(ggData, aes(age, value, group=variable)) + 
  geom_line(aes(colour=variable, linetype=variable)) + ylim(0,.25) + xlim(0,15) +
  scale_color_manual(values=rep(c("orangered","royalblue","royalblue4","royalblue"),4)) +
  scale_linetype_manual(values=rep(c(2,1,2,2),each=4)) +
  ylab("") + xlab("") +
  ggtitle("") +
  theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
        legend.title=element_blank(), legend.position="none",
        plot.margin=unit(c(0,0.5,-0.5,-0.5), "cm"))


#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_MortZ3M.png", width=800, height=600)
print(gg1)
dev.off()
#```

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_MortZ3MZoom.png", width=800, height=600)
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(8, 10))) # 5 rows, 1 column
print(gg1, vp = vplayout(1:8, 1:10))
print(gg2, vp = vplayout(2:5, 2:6))
dev.off()
#```



