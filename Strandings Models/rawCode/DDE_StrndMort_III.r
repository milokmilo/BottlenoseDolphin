####################################################################################################
#         FITTING SEVERAL MODELS TO THE STRANDINGS DATA (SOME AGES REMOVED)
#            Exponential, GLM Poisson, second-degree polynomial, thirt-degree polynomial
#                   created:  (camilo.saavedra@vi.ieo.es) 16/10/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 21/10/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 12/11/2013
####################################################################################################

# inputs
# "../../RData/CEMMA.csv"

# outputs
# "../../RObjects/StrMod/ExpStr0.RData"
# "../../RObjects/StrMod/ExpStr1.RData"
# "../../RObjects/StrMod/ExpStr2.RData"
# "../../RObjects/StrMod/ExpStr3.RData"
# "../../RObjects/StrMod/glmPoisStr0.RData"
# "../../RObjects/StrMod/glmPoisStr1.RData"
# "../../RObjects/StrMod/glmPoisStr2.RData"
# "../../RObjects/StrMod/glmPoisStr3.RData"
# "../../RObjects/StrMod/IIPolStr0.RData"
# "../../RObjects/StrMod/IIPolStr1.RData"
# "../../RObjects/StrMod/IIPolStr2.RData"
# "../../RObjects/StrMod/IIPolStr3.RData"
# "../plots/DDE_predGLMStrd.png"
# "../plots/DDE_predStrd.png"
# "../../RObjects/ggDataStrnd.RData"

# IMPORTANT: Set working directory (to source file location)

# Call libraries 
library(ggplot2)

# Read data 
CEMMA <- read.csv("../../RData/CEMMA.csv")
ddePop <- CEMMA[CEMMA$sp == "DDE",c("age")]
ddePop <- ddePop[complete.cases(ddePop)]
# Changing age values (0.5 and 1.5 for 0 and 1)
ddePop[ddePop == 0.5] <- 0
ddePop[ddePop == 1.5] <- 1
# Calculating stranding frequencies by age 
Str <- table(ddePop)
Str <-data.frame(age = as.numeric(rownames(Str)), n = Str[])
ages <- data.frame(age=c(0,seq(1:29))) # Creates a dataframe with all the ages
Str <- merge(ages, Str, by="age", all.x=TRUE) # Merges dataframes
Str[is.na(Str)] <- 0 # Replaces to zeros
Str

# Removing firt ages for further analysis
Str0 <- Str # All ages (0 ages removed)
assign(paste("Str",1,sep=""), Str[-c(1),]) # Removes ages 0 and 1 (2 ages removed)
assign(paste("Str",2,sep=""), Str[-c(1:2),]) # Removes ages 0 to 2 (3 ages removed)
assign(paste("Str",3,sep=""), Str[-c(1:3),]) # Removes ages 0 to 3 (4 ages removed)
# Listing all the Stranding objects
StrObj <- ls(pattern="^Str[0-9]")


## Modeling strandings/population structure ##

for (i in 1:length(StrObj)){
# Exponential
  assign(paste("Exp",StrObj[i],sep=""), nls(n ~ exp(a + b * age), data = get(StrObj[i]),
               start = list(a = 0, b = 1)))
  print(summary(get(paste("Exp",StrObj[i],sep=""))))
  save(list=paste("Exp",StrObj[i],sep=""), file=paste("../../RObjects/StrMod/","Exp",StrObj[i],".RData",sep=""))
# GLM Family=Poisson (log(y)) 
  assign(paste("glmPois",StrObj[i],sep=""), glm(n ~ age, family=poisson(link="log"), get(StrObj[i])))
  print(summary(get(paste("glmPois",StrObj[i],sep=""))))
  print(anova(get(paste("glmPois",StrObj[i],sep=""))))
  save(list=paste("glmPois",StrObj[i],sep=""), file=paste("../../RObjects/StrMod/","glmPois",StrObj[i],".RData",sep=""))
# Second-degree polynomial
  assign(paste("IIPol",StrObj[i],sep=""), nls(n~b0+b1*age+b2*age^2,data= get(StrObj[i]), 
                start=list(b0=0,b1=1,b2=1)))
  print(summary(get(paste("IIPol",StrObj[i],sep=""))))
  save(list=paste("IIPol",StrObj[i],sep=""), file=paste("../../RObjects/StrMod/","IIPol",StrObj[i],".RData",sep=""))
}


## Predicting values ##

# Adding all ages
for (i in 1:length(StrObj)){
  assign(StrObj[i], merge(ages, get(StrObj[i]), by="age", all.x=TRUE)) # Merges dataframes
}

for (i in 1:length(StrObj)){
  co <- coef(get(paste("Exp",StrObj[i],sep="")))
  assign(StrObj[i], `[[<-`(get(StrObj[i]), "Exp", 
       value = round(exp(co[[1]] + co[[2]] * get(StrObj[i])$age), 2)))
  co <- coefficients(get(paste("glmPois",StrObj[i],sep="")))
  assign(StrObj[i], `[[<-`(get(StrObj[i]), "glmPois", 
                           value = round(exp(co[[1]] + co[[2]] * get(StrObj[i])$age), 2)))
  # We use absolute values for won't get negative values, may be better change it by zero or nought point one
  co <- coef(get(paste("IIPol",StrObj[i],sep="")))
  assign(StrObj[i], `[[<-`(get(StrObj[i]), "IIPol", 
       value = abs(round(co[[1]]+co[[2]]*get(StrObj[i])$age+co[[3]]*get(StrObj[i])$age^2,2))))                       
}


## Plotting observed and predicted values with ggplot2 ##

# Computing data
ggData <- data.frame()
for (i in 1:length(StrObj)) {
  Nmod <- get(StrObj[i])[,-c(1:2)]
  sums <- colSums(Nmod) # STANDARDIZING (can be suppressed)
  Nmod <- as.data.frame(apply(Nmod, 1, function(x) x/sums)) # STANDARDIZING (can be suppressed)
  Nmod <-data.frame(t(Nmod)) # STANDARDIZING (can be suppressed)
  pred <- vector()
  for (j in 1:length(Nmod)) {
    pred <- c(pred, as.vector(as.matrix(Nmod[j])))
  }
  Data <- data.frame(expand.grid(age=get(StrObj[i])$age, mod=names(Nmod)), n=rep(get(StrObj[i])$n
                    /sum(get(StrObj[1])$n, na.rm=T), # STANDARDIZING (can be suppressed)
                    length(names(Nmod))), pred=pred, data=rep(StrObj[i], length(names(Nmod))))
  rm(pred)
  ggData <- rbind(ggData, Data)
}

# Save data for performing other plots
save(file="../../RObjects/ggDataStrnd.RData", ggData)

# Plot
tit <- c("all stranding ages","one age removed","two ages removed","three ages removed")
lab <- function(data,value){return(tit[value])}
gg1 <- ggplot(ggData, aes(age, n)) + 
        geom_point(colour = "black", size = 2) +
        facet_grid(data ~ ., labeller=lab) + 
        geom_line(aes(age, pred, colour=mod, group=mod)) +
        ggtitle("Predicted strandings by fitting different models") + 
        ylab("percentage of strandings") + xlab("age") +
        theme(legend.title=element_blank(), legend.position = "none")
#        theme_bw() # Legend problem (can't remove)

# Including removed values as circle
dat <- subset(ggData, data=="Str0" & mod=="Exp", select=c(age,n))
gg1b <- gg1 +  geom_point(data = dat, colour = "black", size = 1.5, pch=1)

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_predStrd.png", width=800, height=600)
print(gg1b)
dev.off()
#```

# Only GLM Poisson model
gg2 <- ggplot(subset(ggData, mod=="glmPois"), aes(age, n)) + 
  geom_point(colour = "black", size = 2, pch=16) +
  facet_grid(data ~ ., labeller=lab) + 
  geom_line(aes(age, pred, colour="red")) +
  ggtitle("Predicted strandings fitted with Poisson GLM") + 
  ylab("percentage of strandings") + xlab("age") +
  theme(legend.position="none")
#  theme_bw() # Legend problem (can't remove)
gg2b <- gg2 +  geom_point(data = dat, colour = "black", size = 1.5, pch=1)


#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_predGLMStrd.png", width=800, height=600)
print(gg2b)
dev.off()
#```
