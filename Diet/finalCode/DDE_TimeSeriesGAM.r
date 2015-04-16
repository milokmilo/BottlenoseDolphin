####################################################################################################
#                   DIET TIME SERIES - COMMON DOLPHIN DIET (HAKE)
#  Time series GAM, relationship of the diet with the recruitmen and SSB (hake, blue whiting and sardine)  
#                    modified script of: (m.b.santos@vi.ieo.es)  
#                   created: (camilo.saavedra@vi.ieo.es) 11/12/2013
####################################################################################################

# inputs
# "../../RData/All_mainprey_HAKsqr.txt"

# outputs


# IMPORTANT: Set working directory (to source file location)


# Charge libraries
#library(lattice)
#library(nlme)
#library(mgcv)
#library(MASS)
#library (pscl)
library (VGAM)

# Read data

DDEnumbers <- read.table("../../RData/All_mainprey_HAKsqr.txt", header = TRUE)

#names(DDEnumbers)
# [1] "No"           "DAY_YR"       "LAT"         
# [4] "QUARTER"      "YEAR"         "SEXO"        
# [7] "EST_LENGTH"   "Blue_whiting" "hake"        
# [10] "hake_SQR"     "scad"         "Hake_R"      
# [13] "Hake_SSB"     "BLW_R"        "BLW_SSB"     
# [16] "Sar_SSB"      "Sar_R"        "Sardine" 


#str(DDEnumbers)
# 'data.frame':  513 obs. of  18 variables:
# $ No          : Factor w/ 513 levels "1","10","11",..: 1 11 21 31 42 53 64 74 2 3 ...
# $ DAY_YR      : int  217 258 230 10 175 225 183 146 274 274 ...
# $ LAT         : num  42.9 43.1 42.6 42.7 43.1 ...
# $ QUARTER     : int  3 3 3 1 2 3 3 2 4 4 ...
# $ YEAR        : int  2007 2006 2007 2008 2007 2007 2007 2008 2007 2007 ...
# $ SEXO        : int  1 2 1 1 1 2 1 1 1 1 ...
# $ EST_LENGTH  : num  215 180 191 199 217 203 194 159 202 182 ...
# $ Blue_whiting: int  420 81 219 81 47 110 0 0 194 90 ...
# $ hake        : int  3 10 14 6 1 3 0 1 37 31 ...
# $ hake_SQR    : num  1.73 3.16 3.74 2.45 1 ...
# $ scad        : int  0 0 0 4 1 0 23 1 0 0 ...
# $ Hake_R      : num  159.2 99.5 159.2 120.7 159.2 ...
# $ Hake_SSB    : int  14700 12400 14700 15100 14700 14700 14700 15100 14700 14700 ...
# $ BLW_R       : int  4505 9610 4505 3903 4505 4505 4505 3903 4505 4505 ...
# $ BLW_SSB     : int  5390440 6479670 5390440 4180920 5390440 5390440 5390440 4180920 5390440 5390440 ...
# $ Sar_SSB     : int  521000 544280 521000 379990 521000 521000 521000 379990 521000 521000 ...
# $ Sar_R       : int  1859 1568 1859 2534 1859 1859 1859 2534 1859 1859 ...
# $ Sardine     : int  0 0 0 0 0 0 11 12 0 0 ...





my.logit.inv <- function(x) {(1 /(1 + exp(-x)));}

#Different approach with VGAM
#ZIP

M5 <- vgam(sqr(hake) ~ s(YEAR), family = zipoisson,
           data = DDEnumbers)
my.logit.inv <- function(x) {(1 /(1 + exp(-x)));}
M5.prd       <- predict(M5)
M5.prd.pi    <- my.logit.inv(M5.prd[,1])
M5.prd.mu    <- exp(M5.prd[,2])
M5.E         <- M5.prd.mu * (1 - M5.prd.pi)
M5.Var       <- (1 - M5.prd.pi) *
  (M5.prd.mu + M5.prd.pi * M5.prd.mu^2)
M5.res       <- (DDEnumbers$hake - M5.E)/sqrt(M5.Var)
Dispersion   <- sum(M5.res^2) / (nrow(DDEnumbers)-3-3 -2)
Dispersion
#[1] 3.304924 ---> OVERDISPERSION

install.packages("/home/saavedra/Descargas/VGAM_0.8-2.tar.gz")

#ZINB
M7 <- vgam(hake ~ s(YEAR), family = zinegbinomial,
           data = DDEnumbers,
           control = vgam.control(maxit = 100,
                                  epsilon = 1e-4))
M7.prd    <- predict(M7)
M7.prd.pi <- my.logit.inv(M7.prd[,1])
M7.prd.mu <- exp(M7.prd[,2])
M7.prd.k  <- exp(M7.prd[,3])
M7.E      <- M7.prd.mu * (1 - M7.prd.pi)
M7.Var    <- (1-M7.prd.pi) * M7.prd.mu *
  (1 + M7.prd.pi * M7.prd.mu + M7.prd.mu /
     M7.prd.k)
M7.res     <- (DDEnumbers$hake - M7.E) / sqrt(M7.Var)
Dispersion <- sum(M7.res^2) /
  (nrow(DDEnumbers) - 3 - 3 - 3)

Dispersion

par(mfrow=c(1,2));
plot(M7, se=TRUE);
summary(M7)

library (mgcv)
E7 <- residuals (M7, type = "pearson")

E7

#To fit a ZINB GAM model withouth a smoother in the predictor function of the binary model

constraints(M7)

m.SecondSmoother <- rbind(c(0,0),
                          c(0,1),
                          c(0,0))
M8a.clist <- list("(Intercept)" = diag(3),
                  "s(YEAR)"      = m.SecondSmoother,
                  "s(DAY_YR)" = m.SecondSmoother)

M8a <- vgam(hake_SQR ~ s(YEAR) + s(DAY_YR),
            family = zinegbinomial,
            constraints = M8a.clist,
            data = DDEnumbers,
            control = vgam.control(maxit = 100,
                                   epsilon = 1e-4))
coef(M8, matrix = TRUE)

> coef(M8, matrix = TRUE)
logit(phi)   log(munb)    log(k)
(Intercept) -0.7476666  2.59892697 -1.111172
s(YEAR)      0.0000000 -0.04017328  0.000000

log(psi/(1-psi) =-4430.044
    exp(-559.0694) / (1+exp(-559.0694))
    
    pdf(file="c:/01 Delphinus diet/Diet files/Figura4MEPS_pdf")
    par(mfrow=c(1,3), mar=c(20,2,2,2) + 0.1);
    plot(M8a, se=TRUE);
    dev.off()
    
    M5.prd.response  <- predict(M5, type = "response")
    > M5.prd.terms     <- predict(M5, type = "terms")
    > M5.prd.terms.raw <- predict(M5, type = "terms",
                                  raw = TRUE,
                                  se.fit = TRUE)
    > M5.const <- attr(M5.prd.terms, "constant")
    > bin.eta.raw <- M5.const[1] +
      M5.prd.terms.raw$fitted.values[,1]
    > poi.eta.raw <- M5.const[2] +
      M5.prd.terms.raw$fitted.values[,2]
    > Mu <- exp(poi.eta.raw)
    > Pi <- exp(bin.eta.raw)/ (1 + exp(bin.eta.raw))
    > Fit <- Mu * (1 - Pi)
    > cbind(fitted(M5), Mu * (1 - Pi))
    
    