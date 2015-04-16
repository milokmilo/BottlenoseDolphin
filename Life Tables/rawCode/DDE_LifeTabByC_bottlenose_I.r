####################################################################################################
#                             LIFE TABLES BY CAUGHT- bottlenose dolphin paper
#
#                   created:  (camilo.saavedra@vi.ieo.es) 24/10/2013
####################################################################################################

# IMPORTANT: Set working directory (to source file location)


a <- c(0,seq(1:39))
n <- c(36,14,32,15,13,8,4,5,3,3,2,3,3,6,3,8,5,5,8,3,11,4,1,4,3,3,6,2,1,2,0,1,0,1,0,2,0,0,0,0) #both sexes
#n <- c(10,5,13,4,2,3,0,0,0,3,0,0,2,1,2,1,3,1,3,1,7,2,1,1,1,1,4,1,1,2,0,1,0,1,0,1,0,0,0,0) #female
byCatch <- data.frame(age=a, n=n)

for (i in 2:length(byCatch)){
  lifeTab <- data.frame(ageCl=byCatch[,1], M=byCatch[,i]) # mortality table
  #lifeTab <- rbind(c(NA, 0, 0), lifeTab) 
  n <- 1000
  # S - Creating survivorship column
  d <- sum(lifeTab$M) # sum of deaths
  for (j in 1:nrow(lifeTab)){
    lifeTab$S[[j]] <- d-sum(lifeTab$M[1:j]) } 
  # N - number
  lifeTab$N <- lifeTab$M + lifeTab$S
  # nx - Standardizing survivors [(N/∑M) * 1000]
  lifeTab$nx <- round((lifeTab$N/d)*n,1) 
  # dx - Dolphins death-at-age [nx - n(x+1)]
  for (j in 1:nrow(lifeTab)) {
    if (j == 1) {dx <- vector("numeric")}
    d <- round(lifeTab$nx[j]-lifeTab$nx[j+1],3)
    if (j == nrow(lifeTab)) {d <- round(lifeTab$nx[j]-0,3)} 
    dx <- c(dx,d)}
  lifeTab$dx <- dx
  # qx - Death-at-age probability [dx / nx]
  for (j in 1:nrow(lifeTab)) {
    if (j == 1) {qx <- vector("numeric")}
    q <- round(lifeTab$dx[j]/lifeTab$nx[j],3)
    qx <- c(qx,q) }
  lifeTab$qx <- qx 
  # lx - Survivorship-at-age percent [nx / n]
  lifeTab$lx <- lifeTab$nx/n 
  # ex - Life expentancy at age ex = ∑ly/lx
  ex <- vector("numeric")
  for (j in 1:nrow(lifeTab)) {
    e <- round(sum(lifeTab$lx[j:nrow(lifeTab)])/lifeTab$lx[j],3)
    ex <- c(ex,e) }
  lifeTab$ex <- ex
  # Z - Total mortality-at-age -L(nt/no)/t
  Z <- c(NA)
  for (j in 1:nrow(lifeTab)){
    if (j == 1) {Z <- vector("numeric")}
    z <- round(-log(lifeTab$nx[j+1]/lifeTab$nx[j])/1,2)
    if (j == nrow(lifeTab)) {z <- 1.00}
    Z <- c(Z,z)
  }
  lifeTab$Z <- Z
  assign(paste("LifeTab", names(byCatch)[i], sep=""),lifeTab)
}

plot(lifeTab$ageCl, lifeTab$lx, type="l")
lifeByC <- ls(pattern="^LifeTab")


for (i in c(1)){
  young <- lifeTab[i:9, c("ageCl", "qx")]
  #young <- young[young$ageCl!=3 & young$ageCl!=4 ,]
  adult <- lifeTab[-c(1:2), c("ageCl", "qx")]
  #adult <- adult[adult$qx!=0,]
  adult <- adult[complete.cases(adult$qx),]
  old <- lifeTab[9:40, c("ageCl", "qx")]
  #old <- old[old$qx!=0,]
  old <- old[complete.cases(old$qx),]
  #old <- old[-nrow(old),]
  ag <- c(0,seq(1:39))
  # Adult mortality
  a2 <- min(adult$qx)
  # Younger mortality
  young$qx <- (young$qx) - a2
  exp <- nls(qx ~ a*exp(b*ageCl), data = young, start = list(a = 0.1, b = -0.1),control= nls.control(maxiter = 1e+02))
  a1 <- coef(exp)[[1]]
  b1 <- coef(exp)[[2]]
  fit <- a1 * exp(b1 * ag)
  # Senescence mortality
  old$qx <- (old$qx) - a2
  #old$qx[old$qx<0] <- 0
  expS <- nls(qx ~ a*exp(b*ageCl), data = old, start = list(a = 0.1, b = 0.1),control= nls.control(maxiter = 1e+09))
  a3 <- coef(expS)[[1]]
  b3 <- coef(expS)[[2]]
  fitS <- a3 * exp(b3 * ag)
  # Model
  Siler <- a1*exp(b1*ag)+a2+a3*exp(b3*ag)
#  Siler <- 0.1937*exp(-0.3237*ag)+0+0.0064*exp(0.1310*ag)
#  Siler <- 0.1914*exp(-0.2249*ag)+0+0.0209*exp(0.0978*ag)
  
  # plot
  t <- lifeTab[,c("ageCl", "qx")]
  
  assign(paste("Siler01", sep=""), cbind(t,fit, fitS, Siler))
}

# Siler parametres both sexes
# a1 = 0.1611612
# b1 = -0.1545924
# a2 = 0
# a3 = 0.01423946
# b3 = 0.09646701

# Siler parametres males
# a1 = 0.148787
# b1 = -0.2323681
# a2 = 0
# a3 = 0.003054432
# b3 = 0.1470077

## Plotting the two models ##
plot(lifeTab[,c("ageCl", "qx")], xlim=c(0,40), ylim=c(0,1))
abline(h=a2, col=4)
lines(Siler01$ageCl, Siler01$fit+a2, col=2, lty=1)
#lines(Siler02$ageCl, Siler02$fit+a2, col=2, lty=2)
lines(Siler01$ageCl, Siler01$fitS+a2, col=3)
lines (Siler01$ageCl, Siler01$Siler, col=1, lty=1)
#lines (Siler02$ageCl, Siler02$Siler, col=1, lty=2)

# Mean mortality
sum(lifeTab[,"dx"])/sum(lifeTab[,"nx"])

sum(lifeTab[,"nx"])
sum(lifeTab[1:9, "nx"])
sum(lifeTab[10:40, "nx"])/2/2

lifeSiler <- data.frame(ageCl=c(0,seq(1:39)), qxS=round(Siler01$Siler,3))
n <- 1000
# nx and dx - survivors and deaths at age
for (j in 1:nrow(lifeSiler)){
  if (j == 1) {dxS <- lifeSiler$qxS[1]*n; nxS <- n
  } else {
    nxS <- c(nxS, nxS[j-1]- dxS[j-1])
    dxS <- c(dxS, lifeSiler$qxS[j]*nxS[j])
  }
}
lifeSiler <- data.frame(lifeSiler, nxS=round(nxS,5), dxS=round(dxS,5))

data.frame(round(lifeSiler$nxS),round(lifeTab$nx))



plot(lifeTab$ageCl, lifeTab$qx, type="l")
lines(Siler01$ageCl, Siler01$Siler)
mean(lifeTab$qx, na.rm=T)
mean(Siler01$Siler, na.rm=T)





