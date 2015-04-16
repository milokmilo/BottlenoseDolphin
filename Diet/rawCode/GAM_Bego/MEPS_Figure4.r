#DDE numbers   4 variables reduced sample size to inclide Dolphin length and removing outlier (dolphin with many sardine otoliths). Also Year has been substituted by SSB based on the latest ICES advice for sardine SSB numbers from the assessment carried out in 2011.

library(lattice)

setwd("/01 Delphinus diet")
DDEnumbers <- read.table("All_mainprey_HAKsqr.txt", header = TRUE)

names(DDEnumbers)
#[1] "No"                     "MONTH"                  "J_DATE"
#[4] "QUARTER"                "YEAR"                   "SEXO"      "LAT"
# [7] "SIZE_CLASS"             "SECTOR"                 "COD"
#[10] "EST_LENGTH"             "Sardine"                "Boqueron"
#[13] "UK_Clupeidae"           "Clupeidae"              "Argentine"
#[16] "Myctophidae"            "Blue_whiting"           "Trisopterus"
#[19] "Gadiculus"              "UK_Gadidae"             "ALL_Gadidae"
#[22] "Belone"                 "hake"                   "Macror_scolopax"
#[25] "scad"                   "sea_bream_sparids"      "Sparidae"
#[28] "Labridae"               "sandeel_greatersandeel" "Mackerel"
#[31] "Gobiidae"               "Dragonet"               "Atherina"
#[34] "Arnoglossus"            "All_Bothidae"           "UK_Flatfish"
#[37] "UK_Fish"                "ALL_Fish"               "sepia_S_elegans"
#[40] "Sepiola_atlan"          "Sepietta"               "sepiolidae"
#[43] "Loligo"                 "Alloteuthis"            "Ommastrephid"
#[46] "ALL_Ommastrephidae"     "Chiroteuthis"           "Teuthowenia"
#[49] "Eledone"                "Octopus_vulgaris"       "UK_Cephalopoda"
#[52] "ALL_Cephalopod"         "Crustacean"             "Polychaete"
#[55] "ALL"

str(DDEnumbers)
#'data.frame':   514 obs. of  53 variables:
# $ No                : Factor w/ 514 levels "1","10","11",..: 1 11 21 31 42 53 64 74 2 3 ...
# $ QUARTER           : int  3 3 3 1 2 3 3 2 4 4 ...
# $ YEAR              : int  2007 2006 2007 2008 2007 2007 2007 2008 2007 2007 ...
# $ SEXO              : int  1 2 1 1 1 2 1 1 1 1 ...
# $ SIZE_CLASS        : Factor w/ 4 levels "1","2","3","Na": 2 2 2 2 2 2 2 2 2 2 ...
# $ SECTOR            : int  5 3 5 4 3 5 5 6 5 5 ...
# $ COD               : int  1 1 1 5 1 5 2 1 5 5 ...
# $ EST_LENGTH        : Factor w/ 101 levels "122","126","130",..: 82 48 59 67 84 71 62 27 70 50 ...
# $ Sardine           : int  0 0 0 0 0 0 11 12 0 0 ...
# $ Boqueron          : int  0 0 0 0 0 0 0 0 0 0 ...

library(nlme)
library(mgcv)
library(MASS)
library (pscl)

#Poisson and NB GLMs

M1 <- glm (DDEnumbers$Sardine ~ YEAR + LAT + J_DATE,
          family = poisson, data = DDEnumbers)
PearsonRes <- resid(M1, type = "pearson")
Dispersion <- sum(PearsonRes^2) / M1$df.resid
Dispersion

M2 <- glm.nb (DDEnumbers$Sardine ~ YEAR + LAT + J_DATE,
          data = DDEnumbers)
PearsonRes <- resid(M2, type = "pearson")
Dispersion <- sum(PearsonRes^2) / M1$df.resid
Dispersion

M3 <- zeroinfl (DDEnumbers$Sardine ~ YEAR,
          dist = "poisson", link = "logit",
          data = DDEnumbers)
summary (M3)

Gammas.logistic <-coef(M3, model = "zero")
X.logistic <- model.matrix(M3, model = "zero")
eta.logistic <- X.logistic %*% Gammas.logistic
p <- exp(eta.logistic) / (1 + exp(eta.logistic))
Betas.log <- coef(M3, model = "count")
X.log <- model.matrix(M3, model = "count")
eta.log <- X.log %*% Betas.log
mu <- exp (eta.log)
ExpY <- mu * (1 -p)
VarY <- (1-p) * (mu + p * mu^2)
PearsonRes <- (DDEnumbers$Sardine - ExpY) / sqrt(VarY)
N <- nrow(DDEnumbers)
Dispersion <- sum(PearsonRes^2) / (N - 16)
Dispersion

E3 <- resid(M3, type = "pearson")
Dispersion3 <- sum(E3^2)/ (N-16)
Dispersion3

M4 <- zeroinfl(DDEnumbers$Sardine ~ YEAR | 1,
              dist = "negbin", link = "logit",
              data = DDEnumbers)
E4 <- resid(M4, type= "pearson")
Dispersion <- sum(E4^2) / (nrow(DDEnumbers)-17)
Dispersion
exp(Intercept) / (1+exp(Intercept))



LikRatio <-2 * (logLik(M4) - logLik(M3))
df <- 1
pval <- 1 - pchisq(abs(LikRatio), df)
Output <- c(LikRatio, df, pval)
names(Output) <- c("Lik.Ratio", "df", "p-value")
round(Output, digits = 3)

library(MASS)
M1a <- glm.nb(DDEnumbers$Sardine ~ YEAR,
      data = DDEnumbers)

logLik(M1a)

#Different approach with VGAM
#ZIP

library (VGAM)
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
                   "s(J_DATE)" = m.SecondSmoother)

M8a <- vgam(hake_SQR ~ s(YEAR) + s(J_DATE),
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

