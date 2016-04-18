
library (ggplot2)

# Reading and computing data 

table <- read.csv("CEMMA.csv")

  ## Data for 8.4.2 Weighted Sum of Squares of Mean Length With Given Standard Deviation

# <year>  <step>  <area>  <age>  <number>  <mean>  <stddev> 

len <- table[table$sp=="TTR" & !is.na(table$length.total),]
min(len$length.total) # 60
max(len$length.total) # 345

ttrLA <- table[table$sp=="TTR" & !is.na(table$age) & !is.na(table$length.total),]
#max(ttrLA$age) # 34
#nrow(ttrLA) #91

ttrLAselect <- ttrLA[,c("year", "quarter", "age", "length.total")]
#ttrLAselect$age <- floor(ttrLAselect$age)
ttrLAselect$age <- ceiling(ttrLAselect$age)

age.num.sd <- merge(with(ttrLAselect, aggregate(length.total ~ age, FUN = mean)),
                  with(ttrLAselect, aggregate(length.total ~ age, FUN = sd)), by="age")
age.num.sd <- merge(with(ttrLAselect, aggregate(length.total ~ age, FUN = length)), 
                    age.num.sd, by="age")
age.num.sd[is.na(age.num.sd[,4]),4] <- mean(age.num.sd[,4], na.rm=T)# include the mean sd when NA

names(age.num.sd) <- c("age", "number", "mean", "stdesv")

#expand.grid(age.num.sd, age=0:35)
#age.num.sd <- age.num.sd[match(c(0:35), age.num.sd$age), ] 
#age.num.sd$age <- 0:35
#age.num.sd[is.na(age.num.sd$number), "number"] <- 0

#age.num.sd <- rbind(age.num.sd[1:22,], # aggregate common dolphin plus group
#                   c(22, sum(age.num.sd[23:28,2], na.rm=T),
#                     mean(age.num.sd[23:28,3]), max(age.num.sd[23:28,4], na.rm=T)))


#### Files for catchstatistics likelihood ###

age.num.sd$age <- paste("age", age.num.sd$age, sep="")

df <- data.frame(year=rep(1982:2012, each=4), 
           step=rep(1:4, 2012-1982+1),
           area=rep(1, (2012-1982+1)*4))
df$area <- paste("area", df$area, sep="")

  ## Amounts

final.df <- merge(df, age.num.sd)
write.table(final.df, "ttr.catch.lik", 
            row.names=FALSE, sep = "\t", na=" ", quote = FALSE)

  ## Fleet

ttr.catches.fleet <- expand.grid(year=1982:2012, 
                                 step=1:4, area=1, 
                                 fleet="ttr.catches", 
                                 amount=1)

write.table(ttr.catches.fleet, "ttr.catches.fleet", 
            row.names=FALSE, sep = "\t", na=" ", quote = FALSE)


#### Data for Growth models ####

ttrLASex <- table[table$sp=="TTR" & 
                    !is.na(table$age) & 
                    !is.na(table$length.total) & 
                    !is.na(table$sex),]  

  ## Exploratory plot

ggplot(ttrLASex, aes(age, length.total)) + 
        geom_point(aes(col=as.factor(sex))) + 
        geom_smooth() + 
        geom_smooth(aes(col=as.factor(sex)))


  ## Fitting gorwth models

par <- list(K=0.18, to=-5) # Starting values
# Gadget does not use "to" but should be calculated
Linf <- 350

# For Inmatures k parameter
vbAll <- nls(length.total ~ Linf * (1 - exp(-K * (age - to))), 
             start = par, data = ttrLASex,
             lower=c(0,-5), upper=c(1,0), algorithm = "port")      
# Linf=240 K=0.1367 to=-5
# Linf=240 K=0.0835 to=-10.5385
Allpred <- data.frame(age = 0:35)
Allpred$length.total <- predict(vbAll, newdata = Allpred)
ggplot(ttrLASex, aes(age, length.total)) + 
        geom_point(aes(col=as.factor(sex))) + 
        geom_line(data = Allpred)

# For Males k parameter
vbMale <- nls(length.total~Linf*(1-exp(-K*(age-to))), 
              start = par, data = subset(ttrLASex, ttrLASex$sex == 1))      
Malepred <- data.frame(age = 0.5:35.5)
Malepred$length.total <- predict(vbMale, newdata = Malepred)
ggplot(ttrLASex, aes(age, length.total)) + 
        geom_point(aes(col=as.factor(sex))) + 
        geom_line(data = Malepred, col = "blue")

# For Females k parameter
vbFemale <- nls(length.total~Linf*(1-exp(-K*(age-to))), 
                start = par, data = subset(ttrLASex, ttrLASex$sex == 2))      
Femalepred <- data.frame(age = 0.5:35.5)
Femalepred$length.total <- predict(vbFemale, newdata = Femalepred)
ggplot(ttrLASex, aes(age, length.total)) + 
        geom_point(aes(col=as.factor(sex))) + 
        geom_line(data = Femalepred, col = "red")

