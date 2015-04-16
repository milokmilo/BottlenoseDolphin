####################################################################################################
#                 WEIGHT DISTRIBUTION ANALYSIS - COMMON DOLPHIN DIET (HAKE)
#
#                    created: (camilo.saavedra@vi.ieo.es) 04/12/2013
####################################################################################################

# Set working directory (to source file location)


library(ggplot2)

# Leemos los datos
dde <- read.csv("../../RData/DDE_AllDiet.csv")


# Range of the years
#s <- substr(dde$Date, 7,10)
#s <- as.numeric(s)
#unique(s)
#range(s, na.rm=T)

# Number of dolphins analysed
#unique(dde$No)


# Add estimated weight for males, females and indeterminate.
for (i in 1:nrow(dde)){
  if (is.na(dde$Sex[i])) {dde$W[i] <- 10^(-4.68088 + 2.88534 * log10(dde$Length[i]))} else {
    if (dde$Sex[i] == 1) {dde$W[i] <- 10^(-4.56751 + 2.82045 * log10(dde$Length[i]))} else {
      if (dde$Sex[i] == 2) {dde$W[i] <- 10^(-4.74097 + 2.91829 * log10(dde$Length[i]))} else {
        stop ("Something is wrong")}
    }
  }
} # Weight estimates for males and females



wei <- tapply(dde$W, dde$No, mean)
wei <- round(wei)
weiFreq <- table(wei)
weiFreq <- as.data.frame(weiFreq)
weiFreq$wei <- as.numeric(as.character(weiFreq$wei))
weiFreqRg <- data.frame(weiFreq, 
                rg=cut(weiFreq$wei, breaks=seq(20,140,5)))
ggData <- tapply(weiFreqRg$Freq, weiFreqRg$rg, sum)
ggData <- data.frame(Freq= ggData[], rg=names(ggData))
val <- gsub("^\\(", "", ggData2$rg)
val <- gsub(",+[0-9]+\\]","", val)
ggData$val <- as.numeric(val)+2.5

gg <- ggplot(ggData, aes(val, Freq)) + geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(20,140,5), labels=seq(20,140,5)) +
  ylab("frequency") + xlab("length (cm)") + 
  ggtitle("common dolphin - weight distribution") 

png("../plots/WeightDistr.png", width=600, height=400)
print(gg)
dev.off()



