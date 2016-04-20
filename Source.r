################################################################################################### #
#                            SOURCE -> BOTTLENOSE DOLPHIN
#            
#                 created: (camilo.saavedra@vi.ieo.es) 10/04/2015
################################################################################################### #

# IMPORTANT: Set working directory (to source file location)
# setwd("/media/TI30919600A/Users/saavedra/Dropbox/Tesis/FPI-IEO/Cetaceans/Gadget/BottlenoseDolphin")

wd <- getwd()


################################################################################################### #
#### GROWTH ####
################################################################################################### #


#### GROWTH (MALES AND FEMALES) - BOTTLENOSE DOLPHIN ####

setwd("Growth/finalCode")

# von Bertalanffy and Gompertz growth models for males and females
# with our own data (Fiona's)
# and following Wells and Scott, 1999 and with our own data 

source("TTR_GrowthBySex.r")
# inputs
# "../../RData/TTR_Age-Length_M.WS.txt"
# "../../RData/TTR_Age-Length_F.WS.txt"
# "../../RData/CEMMA.csv"
# outputs
# "../../RObjects/vbA.RData"
# "../../RObjects/vbM.RData"
# "../../RObjects/vbF.RData"
# "../../RObjects/gzA.RData"
# "../../RObjects/gzM.RData"
# "../../RObjects/gzF.RData"
# "../../plots/TTR_vbSex-Wells&Scott.png"
# "../../plots/TTR_gzSex-Wells&Scott.png"
# "../plots/TTR_growth_vb.png"
# Gadget
# "../ttr.catch.lik"
# "../ttr.catches.fleet"

setwd(wd)
rm(list=ls())
wd <- getwd()

#### LENGTH WEIGHT RELATIONSHIP - BOTTLENOSE DOLPHIN ####

setwd("Length Weight/finalCode")

source("TTR_LenWei.r")
# inputs
# "../../RData/MaleTTR-AgeWeight.txt
# "../../RData/FemaleTTR-AgeWeight.txt
# "../../RObjects/vbM.RData"
# "../../RObjects/vbF.RData"
# "../../RObjects/vbA.RData"
# outputs
# "../plots/TTR_LengthWeight-Pierce.png"
# "../plots/TTR_LengthWeight-Ridgway&Fenner.png"
# "../plots/TTR_LengthWeight-Wells&Scott.png"
# "../../RObjects/LWmales.RData"
# "../../RObjects/LWfemales.RData"
# "../../RObjects/LWall.RData"
# Gadget
# "../ttr.refwgt1"
# "../ttr.refwgt5"
# "../ttr.refwgt20"

setwd(wd)
rm(list=ls())
wd <- getwd()


#### GROWTH (MALES AND FEMALES) - BOTTLENOSE DOLPHIN ####

setwd("Aggregation/finalCode")

source("TTR_Aggregation.r")
# Gadget
# "../ttr.len1.agg"
# "../ttr.len5.agg"
# "../ttr.len20.agg"
# "../ttr.alllen.agg"
# "../ttr.age.agg"
# "../ttr.allage.agg"
# "../ttr.area.agg"

setwd(wd)
rm(list=ls())
wd <- getwd()


################################################################################################### #
#### MORTALITY ####
################################################################################################### #


#### FLORIDA BOTTLENOSE LIFE TABLES - A - M - F ####

setwd("Life Tables/finalCode")

# Life tables from Florida Bottlenose data for male, female and both together
# raw data -number of stranded dolphins- extracted from Stolen and Barlow, 2003                    

source("TTR_LifeTables-Stolen&Barlow.r")
## inputs
# Stolen & Barlow, 2003
## outputs
# "../../RObjects/TTR_lifeA-S&B.RData"
# "../../RObjects/TTR_lifeM-S&B.RData"
# "../../RObjects/TTR_lifeF-S&B.RData"
# "../../RObjects/TTR_SilerA-S&B.RData"
# "../../RObjects/TTR_SparamsA-S&B.RData"
# "../../RObjects/TTR_SilerM-S&B.RData"
# "../../RObjects/TTR_SparamsM-S&B.RData"
# "../../RObjects/TTR_SilerF-S&B.RData"
# "../../RObjects/TTR_SparamsF-S&B.RData"
# "../plots/TTR_SilerA-S&B.png"
# "../plots/TTR_SilSurvA-S&B.png"
# "../plots/TTR_SilerM-S&B.png"
# "../plots/TTR_SilSurvM-S&B.png"
# "../plots/TTR_SilerF-S&B.png"
# "../plots/TTR_SilSurvF-S&B.png"


### STRANDINGS MODELS ###

      #setwd("Strandings Models/finalCode")

# FITTING SEVERAL MODELS TO THE STRANDINGS DATA (SOME AGES REMOVED)
# Exponential, GLM Poisson, second-degree polynomial, thirt-degree polynomial

      #source("DDE_StrndMort.r")
## inputs
# "../../RData/CEMMA.csv"
## outputs
# "../../RObjects/MExpStr0.RData"
# "../../RObjects/MExpStr1.RData"
# "../../RObjects/MExpStr2.RData"
# "../../RObjects/MExpStr3.RData"
# "../../RObjects/MglmPoisStr0.RData"
# "../../RObjects/MglmPoisStr1.RData"
# "../../RObjects/MglmPoisStr2.RData"
# "../../RObjects/MglmPoisStr3.RData"
# "../../RObjects/MIIPolStr0.RData"
# "../../RObjects/MIIPolStr1.RData"
# "../../RObjects/MIIPolStr2.RData"
# "../../RObjects/MIIPolStr3.RData"
# "../plots/DDE_predGLMStrd.png"
# "../plots/DDE_predStrd.png"
# "../../RObjects/ggStrnd.RData"

      #setwd(wd)
      #rm(list=ls())
      #wd <- getwd()


### LIFE TABLES ###

      #setwd("Life Tables/finalCode")

# LIFE TABLES
# Life tables constructed for all fitted models 
# (exponential, GLM Poisson and second-degree polynomial)

      #source("DDE_LifeTab.r") # REQUIRES: "DDE_StrndMort.r"
## inputs
# "../../RData/CEMMA.csv"
# "../../RObjects/MExp0.RData"
# "../../RObjects/MExp1.RData"
# "../../RObjects/MExp2.RData"
# "../../RObjects/MExp3.RData"
# "../../RObjects/MglmPois0.RData"
# "../../RObjects/MglmPois1.RData"
# "../../RObjects/MglmPois2.RData"
# "../../RObjects/MglmPois3.RData"
# "../../RObjects/MIIPol0.RData"
# "../../RObjects/MIIPol1.RData"
# "../../RObjects/MIIPol2.RData"
# "../../RObjects/MIIPol3.RData"
## outputs
# "../../RObjects/lifeExp0.RData"
# "../../RObjects/lifeExp1.RData"
# "../../RObjects/lifeExp2.RData"
# "../../RObjects/lifeExp3.RData"
# "../../RObjects/lifeglmPois0.RData"
# "../../RObjects/lifeglmPois1.RData"
# "../../RObjects/lifeglmPois2.RData"
# "../../RObjects/lifeglmPois3.RData"
# "../../RObjects/lifeIIPol0.RData"
# "../../RObjects/lifeIIPol1.RData"
# "../../RObjects/lifeIIPol2.RData"
# "../../RObjects/lifeIIPol3.RData"
# "../../RObjects/lifeN.RData"
# "../plots/DDE_MortAge.png"
# "../plots/DDE_MortMod.png"
# "../../RObjects/ggLife.RData"

      #setwd(wd)
      #rm(list=ls())
      #wd <- getwd()


#### SILER LIFE TABLES ####

setwd("Life Tables/finalCode")

# Siler life table from the predicted proportion of strandings
# using data from Stolen and Barlow, 1993

source("TTR_LifeTabSiler-Stolen&Barlow.r")
## inputs
# "../../RObjects/TTR_SilerA-S&B.RData"
# "../../RObjects/TTR_SilerM-S&B.RData"
# "../../RObjects/TTR_SilerF-S&B.RData"
## outputs
# ../../RObjects/lifeSilerA-S&B.RData
# ../../RObjects/lifeSilerM-S&B.RData
# ../../RObjects/lifeSilerF-S&B.RData

setwd(wd)
rm(list=ls())
wd <- getwd()


### MORTALITY ###

      #setwd("Mortality/finalCode")

# PLOT SEVERAL MORALITY MODELS
# Mortality fitted by different models and removing diferent ages
# (0, 1, 2, and 3 ages removed) (exponential, GLM, polynomial and Siler models)

      #source("DDE_plotMModels.r") # REQUIRES: "DDE_StrndMort.r" and "DDE_LifeTab.r"
## inputs
# "../../RObjects/ggStrnd.RData"
# "../../RObjects/ggLife.RData"
## outputs
# "../plots/DDE_MModels.png"
# "../plots/DDE_MAges.png"
# "../plots/DDE_StrdModels.png"

      #setwd(wd)
      #rm(list=ls())
      #wd <- getwd()


      #setwd("Mortality/finalCode")

# MORTALITY SURVIVORSHIP PLOT
# Mortality and survivorship fitted by different models and removing two ages
# (exponential, GLM, polynomial and Siler models)

      #source("DDE_MortSurvPlot.r") # REQUIRES: "DDE_StrndMort.r", "DDE_LifeTab.r" and "DDE_LifeTabSiler.r"
## inputs
# "../../RObjects/lifeN.RData"
# "../../RObjects/lifeExp2.RData"
# "../../RObjects/lifeglmPois2.RData"
# "../../RObjects/lifeIIPol2.RData"
# "../../RObjects/lifeSiler2.RData"
## outputs
# "../plots/DDE_MortSuv.png"

      #setwd(wd)
      #rm(list=ls())
      #wd <- getwd()


### MATURITY ###

      #setwd("Maturity/finalCode")

# MATURITY SPLITED BY SEX
# Three datasets fitted (males, females, females with old immature females removed)    
# Bootstrap to calculate the 95% CL of the A50 

      #source("DDE_MatSex.r")
## inputs
# "../../RData/CEMMA.csv"
## outputs
# "../../RObjects/MatFema.RData"
# "../../RObjects/MatFemC.RData"
# "../../RObjects/MatMale.RData"
# "../plots/DDE_MatSex.png"

      #setwd(wd)
      #rm(list=ls())
      #wd <- getwd()


### BY CATCH ###

      #setwd("By Catch/finalCode")

# BY CATCH        
# Splitting the total mortality in natural and fishing mortality by age. 

      #source("DDE_byCatch.r")
## inputs
# "../../RData/CEMMA.csv"
## outputs
# "../../RObjects/byCatch.RData"
# "../plots/DDE_StrndZMF.png

      #setwd(wd)
      #rm(list=ls())
      #wd <- getwd()


### BY CATCH LIFE TABLES ###

      #setwd("Life Tables/finalCode")

# BY CATCH SILER LIFE TABLES 
# Observed life tables with both total, bycatch and no bycatch data.
# Siler mortality model and Siler life tables with both types of strandings

      #source("DDE_LifeTabByC.r") # REQUIRES: "DDE_byCatch.r"
## inputs
# "../../RObjects/byCatch.RData"
## outputs
# "../../RObjects/lifeTabNByC.RData"
# "../../RObjects/NByCSiler00.RData"
# "../../RObjects/lifeSilerNByC.RData"
# "../plots/DDE_SilerNByC.png"

      #setwd(wd)
      #rm(list=ls())
      #wd <- getwd()


### POPULATION STRUCTURE ###

      #setwd("Population Structure/finalCode")

## POPULATION STRUCTURE
## Population structure calculated with different models 
## (observed data, glm and Siler removing the first two ages and Siler without bycatch)

      #source("DDE_popStr.r") # REQUIRES: "DDE_StrndMort.r", "DDE_LifeTab.r", "DDE_byCatch.r" and "DDE_lifeTabByC.r"
### inputs
# "../../RObjects/lifeN.RData"
# "../../RObjects/lifeglmPois2.RData"
# "../../RObjects/lifeSiler2.RData"
# "../../RObjects/lifeSilerNByC.RData"
### outputs
# "../../RObjects/popStr.RData"

      #setwd(wd)
      #rm(list=ls())
      #wd <- getwd()


### RECRUITMENT ###

      #setwd("Recruitmen/finalCode")

# RECRUITMENT (BIRTHS)
# Recruits calculated with three pregnancy rates for all fitted models
# (observed data, glm and Siler removing the first two ages and Siler without bycatch)

      #source("DDE_Rec.r") # REQUIRES: "DDE_MatSex.r" "DDE_StrndMort.r", "DDE_LifeTab.r", "DDE_byCatch.r", "DDE_lifeTabByC.r" and "DDE_popStr.r"
### inputs
# "../../RObjects/matFemC.RData"
# "../../RObjects/popStr.RData"
### outputs
# "../../RObjects/Rec.RData"

      #setwd(wd)
      #rm(list=ls())
      #wd <- getwd()


### M TEST ###

      #setwd("By Catch/finalCode")

# TOTAL AND NATURAL MODELS                            
# Natural and total mortality with different bycatch levels

      #source("DDE_MTest.r") # REQUIRES: "DDE_LifeTabSiler.r", "DDE_byCatch.r" and "DDE_MatSex.r"
## inputs
# "../../RObjects/Sparams0.RData"
# "../../RObjects/Sparams1.RData"
# "../../RObjects/Sparams2.RData"
# "../../RObjects/Sparams3.RData"
# "../../RObjects/byCatch.RData"
# "../../RObjects/matFemC.RData"
## output
# "../../RObjects/bayRec.RData"
# "../../RObjects/bayPopZ.RData"
# "../plots/DDE_MortZ3M.png"
# "../plots/DDE_MortZ3MZoom.png"
# "../../RObjects/Z_ByC.RData"
# "../../RObjects/M_ByC.RData"

      #setwd(wd)
      #rm(list=ls())
      #wd <- getwd()


### M INVERSE ###

      #setwd("By Catch/finalCode")

# PROOFS BYCATCH MORTALITY
# Maximum level of total mortality than the population can accept to keep constant

      #source("DDE_MInverse.r") # REQUIRES: "DDE_LifeTabSiler.r", "DDE_byCatch.r", "DDE_MatSex.r" and "DDE_MTest.r"
## inputs
# "../../RObjects/M_ByC.RData"
# "../../RObjects/Z_ByC.RData"
# "../../RObjects/byCatch.RData"
# "../../RObjects/matFemC.RData"
## outputs
# "../plots/DDE_Minverse.png"
# "../plots/DDE_MinverseZoom.png"

      #setwd(wd)
      #rm(list=ls())
      #wd <- getwd()



################################################################################################### #
#### DIET ####
################################################################################################### #

### LENGTH WEIGHT ###

setwd("Length Weight/finalCode")

# LENGTH-WEIGHT CURVES FOR BOTLLENOSE DOLPHIN
# Estimate length-weight relationship based on 
# Ridgway and Fenner, 1982 relationship and
# age-length and age-weight relationship in Wells and Scott, 1999

source("TTR_LenWei.r")
# inputs
# "../../RData/MaleTTR-AgeWeight.txt
# "../../RData/FemaleTTR-AgeWeight.txt
# "../../RObjects/vbM.RData"
# "../../RObjects/vbF.RData"
# "../../RObjects/vbA.RData"
# outputs
# "../plots/TTR_LengthWeight-Wells&Scott.png"
# "../../RObjects/LWmales.RData"
# "../../RObjects/LWfemales.RData"
# "../../RObjects/LWall.RData"


setwd(wd)
rm(list=ls())
wd <- getwd()


setwd("Length Weight/finalCode")

# ENERGY MODELS ANALYSIS - BOTTLENOSE DOLPHIN DIET (HAKE)
# Build an object with the diet and dolphin weight

source("DDE_DietWei.r")
## inputs
# "../../RData/TTR_AllDiet.csv"
# "../../RObjects/LWmales.RData"
# "../../RObjects/LWfemales.RData"
# "../../RObjects/LWall.RData"
## outputs
# "../../RObjects/TTR_AllDietWeight.RData"

setwd(wd)
rm(list=ls())
wd <- getwd()


### DIET ###

setwd("Diet/finalCode")

# ENERGY MODELS ANALYSIS - BOTTLENOSE DOLPHIN DIET
# Plotting all energy model and an average model

source("DDE_EnergMod.r") # REQUIRE: "DDE_DietWei.r"
## inputs
# "../../RObjects/TTR_AllDietWeight.RData"
# "../../RData/KjCDGal.csv"
# "../../RData/KjCDPt.csv"
## outputs
# "../plots/TTR_EnergMod.png"
# "../plots/TTR_EnergMod0.png"
# "../plots/TTR_LenWeiDistr.png"

setwd(wd)
rm(list=ls())
wd <- getwd()


setwd("Diet/finalCode")

# EXPLORATORY ANALYSIS - COMMON DOLPHIN DIET (HAKE)
# Length distribution (predator and prey), outliers and linear model

source("DDE_HkeLen.r")
## inputs
# "../../RData/TTR_AllDiet.csv"
## outputs
# "../plots/TTR_DolphHkeLen.png"
# "../plots/TTR_DolphHkeLenQrt.png"
# "../plots/TTR_HkeLenSel.png"

setwd(wd)
rm(list=ls())
wd <- getwd()


setwd("Diet/finalCode")

# EXPLORATORY ANALYSIS - BOTTLENOSE DOLPHIN DIET (Sardine)
# Length distribution (predator and prey), outliers and linear model

source("DDE_SarLen.r")
## inputs
# "../../RData/TTR_AllDiet.csv"
## outputs
# "../plots/TTR_DolphSarLen.png"
# "../plots/TTR_DolphSarLenQrt.png"
# "../plots/TTR_SarLenSel.png"

setwd(wd)
rm(list=ls())
wd <- getwd()


setwd("Diet/finalCode")

# EXPLORATORY ANALYSIS - BOTTLENOSE DOLPHIN DIET (Blue whiting)
# Length distribution (predator and prey), outliers and linear model

source("DDE_BWLen.r")
## inputs
# "../../RData/TTR_AllDiet.csv"
## outputs
# "../plots/TTR_DolphBWLen.png"
# "../plots/TTR_DolphBWLenQrt.png"
# "../plots/TTR_BWLenSel.png"

setwd(wd)
rm(list=ls())
wd <- getwd()


setwd("Diet/finalCode")

# BOX PLOT BY QUARTER - COMMON DOLPHIN DIET (HAKE)
# Box plot of number and weight prey by quarter, length distribution of the diet

source("DDE_DietSteps.r")
## inputs
# "../../RData/TTR_AllDiet.csv"
## outputs
# "../plots/TTR_hkeLen.png"

setwd(wd)
rm(list=ls())
wd <- getwd()


setwd("Diet/finalCode")

# DIET PROPORTION ANALYSIS - COMMON DOLPHIN DIET (HAKE)
# Proportion of the hake in the diet by sex and quarter       

source("DDE_DietProp.r")
## inputs
# "../../RData/dietCD.csv"
# "../../RData/dietCDPt.csv"
# "../../RData/TTR_AllDiet.csv"
## outputs
# "../plots/TTR_DietProp.png"

setwd(wd)
rm(list=ls())
wd <- getwd()


      #setwd("Diet/finalCode")

# DIET KJ ANALYSIS - COMMON DOLPHIN DIET (HAKE)
# Kj analysis in the diet (tranformation of the weight in Kj for Gadget)

      #source("DDE_Kj.r")
## inputs
# "../../RData/dietCD.csv"
# "../../RData/dietCDPt.csv"
## outputs
# ..

      #setwd(wd)
      #rm(list=ls())
      #wd <- getwd()


      #setwd("Diet/finalCode")

# DIET TIME SERIES - COMMON DOLPHIN DIET (HAKE)
# Time series GAM, relationship of the diet with the recruitmen and SSB (hake, blue whiting and sardine)  

      #source("DDE_TimeSeriesGAM.r")
## inputs
# "../../RData/All_mainprey_HAKsqr.txt"
## outputs

      #setwd(wd)
      #rm(list=ls())
      #wd <- getwd()

