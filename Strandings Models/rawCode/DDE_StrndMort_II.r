####################################################################################################
#         FITTING SEVERAL MODELS TO THE STRANDINGS DATA (SOME AGES REMOVED)
#            Exponential, GLM Poisson, second-degree polynomial, thirt-degree polynomial
#                   created:  (camilo.saavedra@vi.ieo.es) 16/10/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 21/10/2013
#                   modified: (camilo.saavedra@vi.ieo.es) 12/11/2013
####################################################################################################

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
ages <- data.frame(age=c(0,seq(1:30))) # Creates a dataframe with all the ages
Str <- merge(ages, Str, by="age", all.x=TRUE) # Merges dataframes
Str[is.na(Str)] <- 0 # Replaces to zeros
Str

# Removing firt ages for further analysis
Str00 <- Str # All ages
assign(paste("Str",0,1,sep=""), Str[-c(1:2),]) # Removes ages 0 and 1 (2 values)
assign(paste("Str",0,2,sep=""), Str[-c(1:3),]) # Removes ages 0 to 2 (3 values)
assign(paste("Str",0,3,sep=""), Str[-c(1:4),]) # Removes ages 0 to 3 (4 values)
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
# Third-degree polynomial
#  assign(paste("IIIPol",StrObj[i],sep=""), nls(n~b0+b1*age+b2*age^2+b3*age^3,data= get(StrObj[i]), 
#                start=list(b0=0,b1=1,b2=1, b3=1)))
#  print(summary(get(paste("IIIPol",StrObj[i],sep=""))))
#  save(list=paste("IIIPol",StrObj[i],sep=""), file=paste("../../RObjects/StrMod/","IIIPol",StrObj[i],".RData",sep=""))
}


## Predicting values ##

# Adding all ages
for (i in 1:length(StrObj)){
  assign(StrObj[i], merge(ages, get(StrObj[i]), by="age", all.x=TRUE)) # Merges dataframes
#  for (j in 1:length(get(StrObj[i]))){
#    v <- get(StrObj[i])[[j]]; v[is.na(v)] <- 0   
#    assign(StrObj[i], `[[<-`(get(StrObj[i]), j, value = v))
#  }
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
#  co <- coef(get(paste("IIIPol",StrObj[i],sep="")))
#  assign(StrObj[i], `[[<-`(get(StrObj[i]), "IIIPol", 
#       value = abs(round(co[[1]]+co[[2]]*get(StrObj[i])$age+co[[3]]*get(StrObj[i])$age^2+co[[4]]*get(StrObj[i])$age^3,2))))        
}


## GLM Residuals analysis ##

#tit <- c("all stranding ages","two ages removed","three ages removed","four ages removed")
#png("../plots/DDE_glmRes.png", width=1000, height=800)
#par(mfcol=c(4,4),oma = c(0, 0, 3, 0))
#for (i in 1:length(StrObj)){
#plot(get(paste("glmPois",StrObj[i],sep="")), sub.caption=NA)
     #sub.caption=(paste("GLM Residual analysis", tit[i])))
#mtext("GLM residual analysis by removing none, two, three and four age classes", outer=TRUE)
##g <- ggplot(get(paste("glmPois",StrObj[i],sep="")), aes(.fitted, .resid)) +
##        geom_hline(yintercept=0) +
##        geom_point () +
##        geom_smooth (se=F)
##print(g)
#}
#dev.off()


## Plotting observed and predicted values ##

#tit <- c("","first two ages removed","first three ages removed","first four ages removed")
#png("../plots/DDE_predStr.png", width=1000, height=800)
##```{r smallplot, fig.width=10, fig.height=8}
#par(mfrow=c(2,2),oma = c(0, 0, 3, 0))
#for (i in 1:length(StrObj)){
#  plot(get(StrObj[i])$age, get(StrObj[i])$n, ylim=c(0,80), ylab="strandings", xlab="age", 
#       main=paste("Predicted strandings", tit[i]))
#  lines(get(StrObj[i])$age, get(StrObj[i])$Exp, col=2) # Red: exponential
#  lines(get(StrObj[i])$age, get(StrObj[i])$glmPois, col=4)
#  lines(get(StrObj[i])$age, get(StrObj[i])$IIPol, col=3) # Green: second-degree polynomial
##  lines(get(StrObj[i])$age, get(StrObj[i])$IIIPol, col=4) # Blue: third-degree polynomial
#}
#mtext("Predicted standings with exponential (red), 
#      Glm Poisson (blue) and second-degree polynomial (green) models", outer = TRUE, cex = 1.5)
##```
#dev.off()


## Plotting observed and predicted values with ggplot2 ##

# Computing data
ggData <- data.frame()
for (i in 1:length(StrObj)) {
  Nmod <- get(StrObj[i])[,-c(1:2)]
  pred <- vector()
  for (j in 1:length(Nmod)) {
    pred <- c(pred, as.vector(as.matrix(Nmod[j])))
  }
  Data <- data.frame(expand.grid(age=get(StrObj[i])$age, mod=names(Nmod)), n=rep(get(StrObj[i])$n, 
                    length(names(Nmod))), pred=pred, data=rep(StrObj[i], length(names(Nmod))))
  rm(pred)
  ggData <- rbind(ggData, Data)
}

# Plot
tit <- c("all stranding ages","two ages removed","three ages removed","four ages removed")
lab <- function(data,value){return(tit[value])}
gg1 <- ggplot(ggData, aes(age, n)) + 
        geom_point(colour = "black", size = 2) +
        facet_grid(. ~ data, labeller=lab) + 
        geom_line(aes(age, pred, colour=mod, group=mod)) +
        ggtitle("Predicted strandings applying different models") + 
        ylab("strandings") + xlab("age") +
        theme(legend.title=element_blank(), legend.position = "top")
#        theme_bw()

# Including removed values as circle
dat <- subset(ggData, data=="Str00" & mod=="Exp", select=c(age,n))
gg2 <- gg1 +  geom_point(data = dat, colour = "black", size = 1.5, pch=1)

gg1b <- ggplot(subset(ggData,mod=="glmPois"), aes(age, n)) + 
  geom_point(colour = "black", size = 2, pch=16) +
  facet_grid(. ~ data, labeller=lab) + 
  geom_line(aes(age, pred, colour="red")) +
  ggtitle("Predicted strandings fitted with Poisson GLM") + 
  ylab("strandings") + xlab("age") +
  theme(legend.position="none")
#  theme_bw()
gg2b <- gg1b +  geom_point(data = dat, colour = "black", size = 1.5, pch=1)



#```{r smallplot, fig.width=16, fig.height=8}
#png("../plots/DDE_predStrd.png", width=1000, height=800)
#print(gg2)
#dev.off()
#```

#```{r smallplot, fig.width=16, fig.height=8}
png("../plots/DDE_predGLMStrd.png", width=1000, height=800)
print(gg2b)
dev.off()
#```
