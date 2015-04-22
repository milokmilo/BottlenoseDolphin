################################################################################################## #
#                  DIET PROPORTION ANALYSIS - BOTTLENOSE DOLPHIN DIET (HAKE)
#                  Proportion of the hake in the diet by sex and quarter       
#                    created: (camilo.saavedra@vi.ieo.es) 22/04/2015
################################################################################################## #

# inputs
# "../../RData/dietCD.csv"
# "../../RData/dietBDAst.csv"
# "../../RData/TTR_AllDiet.csv"
# outputs
# "../plots/TTR_DietProp.png"

################################################################################################## #


# IMPORTANT: Set working directory (to source file location)
#setwd("./Diet/finalCode")
#setwd("../../Diet/finalCode")


# Charge libraries
library(boot)
library(reshape)
library(ggplot2)

# Reading data
ttrSp <- read.csv("../../RData/dietBD.csv") # Galician diet
dietBDAst <- read.csv("../../RData/dietBDAst.csv") # Asturias diet
ttr <- read.csv2("../../RData/TTR_AllDiet.csv") 

#names(ttrSp)
# [1] "No"                "Congridae"         "Engraulidae"       "Clupeidae"        
# [5] "Argentinidae"      "Myctophidae"       "Gadidae"           "Belonidae"        
# [9] "Merluccidae"       "Centriscidae"      "Carangidae"        "Sparidae"         
# [13] "Mugilidae"         "Labridae"          "Zoarcidae"         "Ammodytidae"      
# [17] "Cottidae"          "Gobiidae"          "Scombridae"        "Callionymidae"    
# [21] "Atherinidae"       "Berycidae"         "Bothidae"          "Pleuronectidae"   
# [25] "Other_Fish"        "Sepiidae"          "Sepiolidae"        "Loliginidae"      
# [29] "Ommastrephidae"    "Octopodidae"       "Other_Cephalopoda" "Total" 


####################### GADIDAE ###############################

### ANALYSIS ###

# Boot function to calculate percentages
percGad <- function(data,d){
  sum(data[d,"Gadidae"])/sum(data[d,"Total"])*100
}

### Bootstrap ###

# Total
# Percentage of gadidae in the total diet 
Gad <- boot(ttrSp, percGad, R=1000) # 57.82075
GadCI <- boot.ci(Gad, type = c("perc")) #  45.84, 70.18
N <- nrow(ttrSp)

# Galician gadidae percentage 57.82075 (45.84, 70.18) 

### Portugal diet ###
# Assuming log-normal distribution bootstraping with Asturias mean and Galician sd

sdGad <- sd(as.vector(Gad[[2]]))
dietGad <- vector("numeric")
sigmaGad <- sqrt(log(1+(sdGad/dietBDAst[["Gadidae"]])^2))
muGad <- log(dietBDAst[["Gadidae"]])-(sigmaGad^2)/2
dietGad <- rlnorm(1000, muGad, sigmaGad)
mean(dietGad) # 49.82795
median(dietGad) # 49.29592
quantile(dietGad, c(0.025, 0.5, 0.975)) # 38.78341 49.29592 63.05885  
# Asturias Gadidae percentage 49.29592 (38.78341, 63.05885)


# Total Gadidae percentage for the Iberian Peninsula
totGad <- c(Gad[[2]], dietGad)
mean(totGad) # 53.82046
median(totGad) # 53.76156
quantile(totGad, c(0.025, 0.5, 0.975)) #  40.70941 53.76156 68.62966
# 53.76156 (40.70941, 68.62966)


####################### EUROPEAN HAKE ###############################

### ANALYSIS ###

# Boot function to calculate percentages
percHke <- function(data,d){
  sum(data[d,"Merluccidae"])/sum(data[d,"Total"])*100
}

### Bootstrap ###

# Total
# Percentage of hake in the total diet 
Hke <- boot(ttrSp, percHke, R=1000) # 33.27878
HkeCI <- boot.ci(Hke, type = c("perc")) # 20.97, 44.70
N <- nrow(ttrSp)

# Galician hake percentage 33.27878 (20.97, 44.70)

### Portugal diet ###
# Assuming log-normal distribution bootstraping with portugal mean and galician sd

sdHke <- sd(as.vector(Hke[[2]]))
dietHke <- vector("numeric")
sigmaHke <- sqrt(log(1+(sdHke/dietBDAst[["Merluccidae"]])^2))
muHke <- log(dietBDAst[["Merluccidae"]])-(sigmaHke^2)/2
dietHke <- rlnorm(1000, muHke, sigmaHke)
mean(dietHke) # 39.61628
median(dietHke) # 39.19146
quantile(dietHke, c(0.025, 0.5, 0.975)) # 29.06534 39.19146 52.33443 

# Portuguese hake percentage 39.19146 (29.06534, 52.33443) 

# Total Gaddine percentage Iberian Peninsula
totHke <- c(Hke[[2]], dietHke)
mean(totHke) # 36.36426
median(totHke) # 36.36383
quantile(totHke, c(0.025, 0.5, 0.975)) # 23.24895 36.36383 50.21732 
# 36.36383 (23.24895, 50.21732)


##########################################################################

### Percentages per sex and quarter ###

### HOUSE KEEPING  ###

# Correcting names with different nomenclature
library(car)
ttr$No <- recode(ttr$No, 
             "'122TTR0' = '122TTR00'; '124TTR0' = '124TTR00'; 
              '148TTR0' = '148TTR01'; '174TTR0' = '174TTR01'; 
              '217TTR0' = '217TTR02'; '274TTR0' = '274TTR03'; 
              '287TTR0' = '287TTR03'; '144TTR0' = '144TTR01'; 
              '149TTR0' = '149TTR01'; '160TTR0' = '160TTR01'; 
              '173TTR0' = '173TTR01'; '202TTR0' = '202TTR02'; 
              '216TTR0' = '216TTR02'; '219TTR0' = '219TTR02'; 
              '115TTR0' = '115TTR00'; '162TTR0' = '162TTR00'", as.factor.result=TRUE)

noTTR <- ttrSp[!ttrSp$No %in% ttr$No,"No"]
ttrSp <- ttrSp[!ttrSp$No==noTTR,]

# Add variables from the total dataset (AllDiet)
# Length, Date and Sex
len <- vector("numeric")
dat <- vector("character")
sex <- vector("numeric")
for (i in 1:nrow(ttrSp)){
  set <- subset(ttr, ttr$No %in% ttrSp$No[i] == TRUE)
  len <- c(len, unique(set$Length))
  dat <- c(dat, unique(as.character(set$Date)))
  sex <- c(sex, unique(set$Sex))
}
ttrSp$Length <- len
ttrSp$Date <- dat
ttrSp$Sex <- sex


# Add new variables 
# Year, Month, Quarter
ttrSp$Year <- substr(ttrSp$Date,7,10)
ttrSp$Month <- substr(ttrSp$Date,4,5)
for (i in 1:nrow(ttrSp)){
  if(ttrSp$Month[i] %in% c("01","02","03")) {
    ttrSp$Quarter[i] <- 1
  } else {
    if(ttrSp$Month[i] %in% c("04","05","06")) {
      ttrSp$Quarter[i] <- 2
    } else { 
      if(ttrSp$Month[i] %in% c("07","08","09")) {
        ttrSp$Quarter[i] <- 3
      } else { 
        if(ttrSp$Month[i] %in% c("10","11","12")) {
          ttrSp$Quarter[i] <- 4
        } else {
          ttrSp$Quarter[i] <- NA 
        }
      } 
    }
  }
}

# Time series (divided in 6 groups of 3 years)
#for (i in 1:nrow(ttrSp)){
#  if(ttrSp$Year[i] %in% c("1991","1992","1993")) {
#    ttrSp$timeSer[i] <- 1
#  } else {
#    if(ttrSp$Year[i] %in% c("1994","1995","1996")) {
#      ttrSp$timeSer[i] <- 2
#    } else { 
#      if(ttrSp$Year[i] %in% c("1997","1998","1999")) {
#        ttrSp$timeSer[i] <- 3
#      } else { 
#        if(ttrSp$Year[i] %in% c("2000","2001","2002")) {
#          ttrSp$timeSer[i] <- 4
#        } else {
#          if(ttrSp$Year[i] %in% c("2003","2004","2005")) {
#            ttrSp$timeSer[i] <- 5
#        } else { 
#          if(ttrSp$Year[i] %in% c("2006","2007","2008")) {
#            ttrSp$timeSer[i] <- 6
#          } else {
#            ttrSp$timeSer[i] <- NA 
#          }
#        } 
#      }
#    }
#  }
#  }
#}

Qtr <- c(1,2,3,4)
Sex <- c("M","F")

# Quarter
# Percentage of hake in the diet divided by quarters
for (i in Qtr){
  assign(paste("Hke", i, "Q", sep=""), 
    boot(ttrSp[ttrSp$Quarter==i,], percHke, R=1000))
  assign(paste("Hke", i, "Q", "CI", sep=""),
         boot.ci(get(paste("Hke", i, "Q", sep="")), type=c("perc")))
  assign(paste("N", i, "Q", sep=""),
    nrow(ttrSp[ttrSp$Quarter==i,])) # Dolphins per quarter
}

#ls(pattern="Hke+[0-9]+Q")
#ls(pattern="Hke+[0-9]+Q+CI")
#ls(pattern="N+[0-9]+Q")

# Quarter and Sex
# Percentage of hake in the diet divided by sex and quarters
for (i in Qtr){
  for (j in 1:length(Sex)){
    assign(paste("Hke",Sex[j], i, "Q", sep=""), 
           boot(subset(ttrSp, Sex==j & Quarter==i), percHke, R=1000))  
    assign(paste("Hke",Sex[j], i, "Q", "CI", sep=""),
           boot.ci(get(paste("Hke",Sex[j], i, "Q", sep="")), type=c("perc")))
    assign(paste("N",Sex[j], i, "Q", sep=""),
           nrow(subset(ttrSp, Sex==j & Quarter==i))) # Dolphins per quarter
  }
}

#ls(pattern="Hke+([M|F])+[0-9]+Q")
#ls(pattern="Hke+([M|F])+[0-9]+Q+CI")
#ls(pattern="N+([M|F])+[0-9]+Q")

# Sex
# Percentage of hake in the diet divided by sex
for (i in 1:length(Sex)){
  assign(paste("Hke", Sex[i], sep=""), 
         boot(subset(ttrSp, Sex==i), percHke, R=1000))  
  assign(paste("Hke", Sex[i], "CI", sep=""),
         boot.ci(get(paste("Hke", Sex[i], sep="")), type=c("perc")))
  assign(paste("N", Sex[i], sep=""),
         nrow(subset(ttrSp, Sex==i))) # Dolphins per quarter
}

#ls(pattern="Hke+[M|F]$")
#ls(pattern="Hke+[M|F]+CI+$")
#ls(pattern="N+[M|F]$")


############ Tables #################


### Means table ###

# Partial means
QSex <- ls(pattern="^Hke+([A-Z]*)+[0-4]+Q+$")
QSexCI <- ls(pattern="^N+([A-Z]*)+[0-4]+Q")

Mperc <- vector("numeric")
Fperc <- vector("numeric")
perc <- vector("numeric")
  
for (i in 1:length(QSex)){
  if (substr(QSex[i],4,4) == "F"){
    Mperc <- c(Mperc, get(QSex[i])[[1]])
  } else {
    if (substr(QSex[i],4,4) == "M"){
      Fperc <- c(Fperc, get(QSex[i])[[1]])  
  } else {
    perc <- c(perc, get(QSex[i])[[1]])
  }
 }             
}
# Total means 
Tot <- c(ls(pattern="Hke+[M|F]$"), "Hke")
Total <- vector("numeric") 
for (i in 1:3){
  Total <-  c(Total, get(Tot[i])[[1]])
}

# Mean table
Qmean <- rbind(Mperc, Fperc, perc)
Qmean <- as.data.frame(cbind(Qmean, Total))
rownames(Qmean) <- c("Males", "Females", "Total")
names(Qmean) <- c("Q1", "Q2", "Q3", "Q4", "Tot")
Qmean

### Counts table ###

# Partial counts
NSex <- ls(pattern="^N+([A-Z]*)+[0-4]+Q")

Mcount <- vector("numeric")
Fcount <- vector("numeric")
count <- vector("numeric")

for (i in 1:length(NSex)){
  if (substr(NSex[i],2,2) == "F"){
    Mcount <- c(Mcount, get(NSex[i])[[1]])
  } else {
    if (substr(NSex[i],2,2) == "M"){
      Fcount <- c(Fcount, get(NSex[i])[[1]])  
    } else {
      count <- c(count, get(NSex[i])[[1]])
    }
  }             
}
# Total counts 
NTot <- c(ls(pattern="N+[M|F]$"), "N")
NTotal <- vector("numeric") 
for (i in 1:3){
  NTotal <-  c(NTotal, get(NTot[i]))
}

# Counts table
Qcount <- rbind(Mcount, Fcount, count)
Qcount <- as.data.frame(cbind(Qcount, NTotal))
rownames(Qcount) <- c("Males", "Females", "Total")
names(Qcount) <- c("Q1", "Q2", "Q3", "Q4", "Tot")
Qcount

### CIs table ###

# Partial counts
QSexCI <- ls(pattern="^Hke+([A-Z]*)+[0-4]+Q+CI")

MCImin <- vector("numeric")
FCImin <- vector("numeric")
CImin <- vector("numeric")
MCImax <- vector("numeric")
FCImax <- vector("numeric")
CImax <- vector("numeric")

for (i in 1:length(QSexCI)){
  if (substr(QSexCI[i],4,4) == "F"){
    MCImin <- c(MCImin, get(QSexCI[i])[[4]][4])
    MCImax <- c(MCImax, get(QSexCI[i])[[4]][5])
  } else {
    if (substr(QSexCI[i],4,4) == "M"){
      FCImin <- c(FCImin, get(QSexCI[i])[[4]][4])  
      FCImax <- c(FCImax, get(QSexCI[i])[[4]][5])  
    } else {
      CImin <- c(CImin, get(QSexCI[i])[[4]][4])
      CImax <- c(CImax, get(QSexCI[i])[[4]][5])
    }
  }             
}
# Total CIs 
CITot <- c(ls(pattern="Hke+[M|F]+CI+$"), "HkeCI")
CITotalmin <- vector("numeric") 
CITotalmax <- vector("numeric") 
for (i in 1:3){
  CITotalmin <-  c(CITotalmin, get(CITot[i])[[4]][4])
  CITotalmax <-  c(CITotalmax, get(CITot[i])[[4]][5])
}
CITotal <- vector("numeric") 
for (i in 1:3){
  CITotal <- c(CITotal, CITotalmax[i], CITotalmin[i])
}

# CIs table
QCI <- rbind(MCImax,MCImin,FCImax,FCImin,CImax,CImin)
QCI <- as.data.frame(cbind(QCI, CITotal))
rownames(QCI) <- c("MalesMax","MalesMin","FemalesMax",
                   "FemalesMin","TotalMax", "TotalMin")
names(QCI) <- c("Q1", "Q2", "Q3", "Q4", "Tot")
QCI


### ggPlotting ###

Qmean$Sex <- rownames(Qmean)
ggMean <- melt(Qmean, id.vars="Sex")

QCImax <- as.data.frame(rbind(MCImax,FCImax,CImax))
QCImax <- as.data.frame(cbind(QCImax, CITotalmax))
QCImin <- as.data.frame(rbind(MCImin,FCImin,CImin))
QCImin <- as.data.frame(cbind(QCImin, CITotalmin))

ggMean$max <- melt(QCImax)[,2]
ggMean$min <- melt(QCImin)[,2]

gg <- ggplot(ggMean[!ggMean$variable=="Tot",], aes(variable, value, group=Sex)) + 
        geom_line(aes(colour=Sex), position=position_dodge(.15)) +
        geom_point(aes(colour=Sex), position=position_dodge(.15), size=3, shape=21, fill="white") +
        geom_errorbar(aes(ymin=min, ymax=max, group=Sex, colour=Sex),
                width=.2, position=position_dodge(.15)) +
        ggtitle("Percentage of hake in the Bottlenose dolphin diet, per sex and quarter")+
        ylab("percentage of hake in the diet (kg)") + xlab("dolphin weight (kg)") +
        theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
              legend.title= element_blank(),  legend.position="top")

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/TTR_DietProp.png", width=600, height=400)
print(gg)
dev.off()
#```


# Percentage of hake in the first part of the time series  
#years <- unique(ttrSp$Year)
#for (i in years) {
#  assign(paste("HkeY", i, sep=""),  boot(ttrSp[ttrSp$Year==i,], percHke, R=1000))
#  assign(paste("n", i, sep=""), nrow(ttrSp[ttrSp$Year==i,])) # Dolphins per quarter
#}

#Yr <- ls(pattern="^HkeY+[0-9]")

#plot(NULL, NULL, xlim=c(1990, 2008), ylim=c(0,10))
#for (i in 1:length(Yr)){
#  points(as.numeric(years[i]), get(Yr[i])[[1]])  
#}


