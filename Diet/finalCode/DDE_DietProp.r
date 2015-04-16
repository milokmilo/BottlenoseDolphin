####################################################################################################
#                  DIET PROPORTION ANALYSIS - COMMON DOLPHIN DIET (HAKE)
#                  Proportion of the hake in the diet by sex and quarter       
#                    created: (camilo.saavedra@vi.ieo.es) 12/09/2013
#                    modified: (camilo.saavedra@vi.ieo.es) 15/12/2013
####################################################################################################

# inputs
# "../../RData/dietCD.csv"
# "../../RData/dietCDPt.csv"
# "../../RData/DDE_AllDiet.csv"

# outputs
# "../plots/DDE_DietProp.png"

# IMPORTANT: Set working directory (to source file location)

# Charge libraries
library(boot)
library(reshape)
library(ggplot2)

# Reading data
ddeSp <- read.csv("../../RData/dietCD.csv") # Galician diet
dietCDPt <- read.csv("../../RData/dietCDPt.csv") # Potugal diet
dde <- read.csv("../../RData/DDE_AllDiet.csv") 



#names(ddeSp)
# [1] "No"               "Congridae"        "Engraulidae"      "Clupeidae"       
# [5] "Argentinidae"     "Myctophidae"      "Gadidae"          "Belonidae"       
# [9] "Merluccidae"      "Centriscidae"     "Carangidae"       "Sparidae"        
# [13] "Mugilidae"        "Labridae"         "Zoarcidae"        "Ammodytidae"     
# [17] "Cottidae"         "Gobiidae"         "Scombridae"       "Callionymidae"   
# [21] "Atherinidae"      "Berycidae"        "Bothidae"         "Pleuronectidae"  
# [25] "Other_fish"       "Sepiidae"         "Sepiolidae"       "Loliginidae"     
# [29] "Ommastrephidae"   "Octopodidae"      "Other_cephalopod" "Total"           
# [33] "Length"           "Date"             "Sex"              "Year"            
# [37] "Month"            "Quarter" 

####################### SARDINE ###############################

### ANALYSIS ###

# Boot function to calculate percentages
percSar <- function(data,d){
  sum(data[d,"Clupeidae"])/sum(data[d,"Total"])*100
}

### Bootstrap ###

# Total
# Percentage of hake in the total diet 
Sar <- boot(ddeSp, percSar, R=1000) # 11.43743
SarCI <- boot.ci(Sar, type = c("perc")) #  8.88, 14.06
N <- nrow(ddeSp)

# Galician sardine percentage 11.43743 (8.88, 14.06) 

### Portugal diet ###
# Assuming log-normal distribution bootstraping with portugal mean and galician sd

sdSar <- sd(as.vector(Sar[[2]]))
dietSar <- vector("numeric")
sigmaSar <- sqrt(log(1+(sdSar/dietCDPt[["Clupeidae"]])^2))
muSar <- log(dietCDPt[["Clupeidae"]])-(sigmaSar^2)/2
dietSar <- rlnorm(1000, muSar, sigmaSar)
mean(dietSar) # 43.47767
median(dietSar) # 43.45014
quantile(dietSar, c(0.025, 0.5, 0.975)) # 40.96239 43.45014 46.15013 

# Portuguese sardine percentage 43.47767 (40.96239, 46.15013)


# Total Sardine percentage Iberian Peninsula
totSar <- c(Sar[[2]], dietSar)
mean(totSar) # 27.47167
median(totSar) # 27.7238
quantile(totSar, c(0.025, 0.5, 0.975)) #  9.493947 27.723802 45.769452 
# 27.47167 (9.493947, 45.769452)


####################### EUROPEAN HAKE ###############################

### ANALYSIS ###

# Boot function to calculate percentages
percHke <- function(data,d){
  sum(data[d,"Merluccidae"])/sum(data[d,"Total"])*100
}

### Bootstrap ###

# Total
# Percentage of hake in the total diet 
Hke <- boot(ddeSp, percHke, R=1000) # 5.706074
HkeCI <- boot.ci(Hke, type = c("perc")) # 4.290,  7.346
N <- nrow(ddeSp)

# Galician hake percentage 5.706074 ( 4.290,  7.346 )

### Portugal diet ###
# Assuming log-normal distribution bootstraping with portugal mean and galician sd

sdHke <- sd(as.vector(Hke[[2]]))
dietHke <- vector("numeric")
sigmaHke <- sqrt(log(1+(sdHke/dietCDPt[["Merluccidae"]])^2))
muHke <- log(dietCDPt[["Merluccidae"]])-(sigmaHke^2)/2
dietHke <- rlnorm(1000, muHke, sigmaHke)
mean(dietHke) # 2.09424
median(dietHke) # 1.956226
quantile(dietHke, c(0.025, 0.5, 0.975)) # 0.958901 1.956226 3.947419 

# Portuguese hake percentage 2.09424 (0.958901, 3.947419) 

# Total Sardine percentage Iberian Peninsula
totHke <- c(Hke[[2]], dietHke)
mean(totHke) # 3.888011
median(totHke) # 4.181937
quantile(totHke, c(0.025, 0.5, 0.975)) # 1.097216 4.181937 7.007001 
# 3.888011 (1.097216,7.007001 )

##########################################################################

### Percentages per sex and quarter ###

### HOUSE KEEPING  ###

# fix wrong values
dde[dde$No=="DD104", "Sex"] <- 2
dde[dde$No=="DD134", "Sex"] <- 1
dde[dde$No=="DD562", "Sex"] <- 1

# Add variables from the total dataset (AllDiet)
# Length, Date and Sex
len <- vector("numeric")
dat <- vector("character")
sex <- vector("numeric")
for (i in 1:nrow(ddeSp)){
  set <- subset(dde, dde$No %in% ddeSp$No[i] == TRUE)
  len <- c(len, unique(set$Length))
  dat <- c(dat, unique(as.character(set$Date)))
  sex <- c(sex, unique(set$Sex))
}
ddeSp$Length <- len
ddeSp$Date <- dat
ddeSp$Sex <- sex

# Add new variables 
# Year, Month, Quarter
ddeSp$Year <- substr(ddeSp$Date,7,10)
ddeSp$Month <- substr(ddeSp$Date,4,5)
for (i in 1:nrow(ddeSp)){
  if(ddeSp$Month[i] %in% c("01","02","03")) {
    ddeSp$Quarter[i] <- 1
  } else {
    if(ddeSp$Month[i] %in% c("04","05","06")) {
      ddeSp$Quarter[i] <- 2
    } else { 
      if(ddeSp$Month[i] %in% c("07","08","09")) {
        ddeSp$Quarter[i] <- 3
      } else { 
        if(ddeSp$Month[i] %in% c("10","11","12")) {
          ddeSp$Quarter[i] <- 4
        } else {
          ddeSp$Quarter[i] <- NA 
        }
      } 
    }
  }
}

# Time series (divided in 6 groups of 3 years)
#for (i in 1:nrow(ddeSp)){
#  if(ddeSp$Year[i] %in% c("1991","1992","1993")) {
#    ddeSp$timeSer[i] <- 1
#  } else {
#    if(ddeSp$Year[i] %in% c("1994","1995","1996")) {
#      ddeSp$timeSer[i] <- 2
#    } else { 
#      if(ddeSp$Year[i] %in% c("1997","1998","1999")) {
#        ddeSp$timeSer[i] <- 3
#      } else { 
#        if(ddeSp$Year[i] %in% c("2000","2001","2002")) {
#          ddeSp$timeSer[i] <- 4
#        } else {
#          if(ddeSp$Year[i] %in% c("2003","2004","2005")) {
#            ddeSp$timeSer[i] <- 5
#        } else { 
#          if(ddeSp$Year[i] %in% c("2006","2007","2008")) {
#            ddeSp$timeSer[i] <- 6
#          } else {
#            ddeSp$timeSer[i] <- NA 
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
    boot(ddeSp[ddeSp$Quarter==i,], percHke, R=1000))
  assign(paste("Hke", i, "Q", "CI", sep=""),
         boot.ci(get(paste("Hke", i, "Q", sep="")), type=c("perc")))
  assign(paste("N", i, "Q", sep=""),
    nrow(ddeSp[ddeSp$Quarter==i,])) # Dolphins per quarter
}

#ls(pattern="Hke+[0-9]+Q")
#ls(pattern="Hke+[0-9]+Q+CI")
#ls(pattern="N+[0-9]+Q")

# Quarter and Sex
# Percentage of hake in the diet divided by sex and quarters
for (i in Qtr){
  for (j in 1:length(Sex)){
    assign(paste("Hke",Sex[j], i, "Q", sep=""), 
           boot(subset(ddeSp, Sex==j & Quarter==i), percHke, R=1000))  
    assign(paste("Hke",Sex[j], i, "Q", "CI", sep=""),
           boot.ci(get(paste("Hke",Sex[j], i, "Q", sep="")), type=c("perc")))
    assign(paste("N",Sex[j], i, "Q", sep=""),
           nrow(subset(ddeSp, Sex==j & Quarter==i))) # Dolphins per quarter
  }
}

#ls(pattern="Hke+([M|F])+[0-9]+Q")
#ls(pattern="Hke+([M|F])+[0-9]+Q+CI")
#ls(pattern="N+([M|F])+[0-9]+Q")

# Sex
# Percentage of hake in the diet divided by sex
for (i in 1:length(Sex)){
  assign(paste("Hke", Sex[i], sep=""), 
         boot(subset(ddeSp, Sex==i), percHke, R=1000))  
  assign(paste("Hke", Sex[i], "CI", sep=""),
         boot.ci(get(paste("Hke", Sex[i], sep="")), type=c("perc")))
  assign(paste("N", Sex[i], sep=""),
         nrow(subset(ddeSp, Sex==i))) # Dolphins per quarter
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
        ggtitle("Percentage of hake in the diet, per sex and quarter")+
        ylab("percentage of hake in the diet (kg)") + xlab("dolphin weight (kg)") +
        theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.5), 
              legend.title= element_blank(),  legend.position="top")

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_DietProp.png", width=600, height=400)
print(gg)
dev.off()
#```


# Percentage of hake in the first part of the time series  
#years <- unique(ddeSp$Year)
#for (i in years) {
#  assign(paste("HkeY", i, sep=""),  boot(ddeSp[ddeSp$Year==i,], percHke, R=1000))
#  assign(paste("n", i, sep=""), nrow(ddeSp[ddeSp$Year==i,])) # Dolphins per quarter
#}

#Yr <- ls(pattern="^HkeY+[0-9]")

#plot(NULL, NULL, xlim=c(1990, 2008), ylim=c(0,10))
#for (i in 1:length(Yr)){
#  points(as.numeric(years[i]), get(Yr[i])[[1]])  
#}


