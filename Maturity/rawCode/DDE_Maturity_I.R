####################################################################################################
#                                       MATURITY
#           
#                  C.Saavedra (camilo.saavedra@vi.ieo.es) 27/09/2013
####################################################################################################

# IMPORTANT: Set working directory (to source file location)

# Charge libraries
library(ggplot2)

# Read data
CEMMA <- read.csv("../../data/RData/CEMMA.csv")
ddeMat <- CEMMA[CEMMA$sp == "DDE",c("month", "year", "season", "length.real", "length",
                                    "sex", "cod", "age", "maturity", "reprod.status")]

## Proportion of mature ~ Real length ##

ddeMatLen <- ddeMat[,c( "sex", "maturity", "length.real")]
ddeMatLen <- ddeMatLen[complete.cases(ddeMatLen),]
#max(ddeMatLen$length.real)
#min(ddeMatLen$length.real)
ddeMatLen$rg <- cut(ddeMatLen$length.real,breaks=seq(90,240,by=10)) # Creating ranges

total <- tapply(ddeMatLen$maturity, list(ddeMatLen$sex, ddeMatLen$rg), length) # Count total data by sex and range
matures <- tapply(ddeMatLen$maturity, list(ddeMatLen$sex, ddeMatLen$rg), sum)  # Count matures by sex and range
total[is.na(total)] <- 0 # Remove NAs  
matures[is.na(matures)] <- 0 # Remove NAs

# Males
m <- round((matures[1,]/total[1,]),2) # Proportion of mature males by length class
sum(matures[1,])# total mature males
sum(total[1,]) # total males
# Females
f <- round((matures[2,]/total[2,]),2) # Proportion of mature females by length class 
sum(matures[2,])# total mature females
sum(total[2,]) # total females 
# Data frame
df <- as.data.frame(f) # dataframe with female data
df$m <- m # adding males
len <- substr(rownames(df),2,4)  # construct length vector
df$len <- c(substr(len[1],1,2), len[2:length(len)]) # solve firts length problem
df$len <- as.numeric(df$len)

# Females plot
gML <- ggplot(df, aes(x=len)) + geom_point(aes(x=len,y=f)) + geom_line(aes(x=len,y=f), colour="red") 

dev.off()





