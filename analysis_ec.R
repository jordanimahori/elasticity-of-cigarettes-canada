
# TO DO 
# 1. Test of first stage strength (fix)
# 2. Chow test (fix)
# 3. Check the datasets


# Clearing previous session.
rm(list = ls())

# Setting working directory.
setwd("/Users/jordan/Documents/Elasticity of Cigarettes")

# Loading packages.
library(readxl)
library(AER)
library(stargazer)
library(tidyverse)

# Importing master dataset
load("data/elasticity_data.RData")

# --------------------- CLEANING DATA & GENERATING CONTROL VARIABLES -----------------------

# Creating ln versions of our variables. 
mdta$lnsales <- log(mdta$carton_sales)
mdta$lnprice <- log(mdta$price_2002)
mdta$ln_total_tax = log(mdta$total_weighted_tax)

# Encoding province as a factor variable
mdta$province <- as_factor(mdta$province)

# Restricting our sample to years for which we have data
mdta <- filter(mdta, year > 2002)


# ------------------------ ESTIMATING  BASIC MODEL (NO IV) -------------------------------
  
# Running a basic regression, no FEs, linear time trend or IV
ols1 <- lm(lnsales ~ lnprice + rgdp_2002 + unemployment, data = mdta)

# Running a basic regression, FEs but no linear time trend or IV
ols2 <- lm(lnsales ~ lnprice + rgdp_2002 + unemployment + as.factor(year) + province, data = mdta)

# Running a basic regression, FEs and linear time trend but no IV
ols3 <- lm(lnsales ~ lnprice + rgdp_2002 + unemployment + as.factor(year) + province + province*year, data = mdta)


# --------------------------- ESTIMATING IV REGRESSSION ---------------------------------
  
# Testing the first stage. 
fstage1 <- lm(lnprice ~ ln_total_tax  + rgdp_2002 + unemployment + province + as.factor(year) + province*year, data = mdta)

# Estimating first equation, and testing the first stage
iv1 <- ivreg(lnsales ~ lnprice + rgdp_2002 + unemployment + province + as.factor(year) + province*year | ln_total_tax  + rgdp_2002 + unemployment + province + as.factor(year) + province*year, data = mdta)


# -------------- RE-ESTIMATING EQUATIONS AFTER DROPPING SMUGGLING PROVINCES ---------------

# Dropping Smuggling Provinces
nosmuggle <- filter(mdta, !(province == "ON" & year<= 2017 & year>=2006) &
                            !(province == "QC" & year<= 2009 & year>=2006) &
                              !(province == "NB" & year<=2017 & year>=2015) &
                                !(province == "NS" & year<=2008 & year>=2006))

# Testing first stage
fstage2 <- lm(lnprice ~ ln_total_tax  + rgdp_2002 + unemployment + province + as.factor(year) + province*year, data = nosmuggle)

# Re-estimating the first model
iv2 <- ivreg(lnsales ~ lnprice + rgdp_2002 + unemployment + province + as.factor(year) + province*year | ln_total_tax  + rgdp_2002 + unemployment + province + as.factor(year) + province*year, data = nosmuggle)

# --------------------------- ROBUSTNESS TESTS ---------------------------------
  
# Estimating elasticity through diff-in-diff (no time trend, no controls) 
rb1 <- ivreg(lnsales ~ lnprice + province + as.factor(year) | ln_total_tax + province + as.factor(year), data = mdta)

# Estimating elasticity through diff-in-diff (no time trend, controls)
rb2 <- ivreg(lnsales ~ lnprice + rgdp_2002 + unemployment + province + as.factor(year) | ln_total_tax + rgdp_2002 + unemployment + province + as.factor(year), data = mdta)

# Estimating elasticity through diff-in-diff (time trend, controls)
rb3 <- ivreg(lnsales ~ lnprice + rgdp_2002 + unemployment + province + as.factor(year) + province*year | ln_total_tax + rgdp_2002 + unemployment + province + as.factor(year) + province*year, data = mdta)

# Estimating elasticity through diff-in-diff (no time trend, no controls) - RESTRICTED PROVINCES
rb4 <- ivreg(lnsales ~ lnprice + province + as.factor(year) | ln_total_tax + province + as.factor(year), data = nosmuggle)

# Estimating elasticity through diff-in-diff (no time trend, controls) - RESTRICTED PROVINCES
rb5 <- ivreg(lnsales ~ lnprice + rgdp_2002 + unemployment + province + as.factor(year) | ln_total_tax + rgdp_2002 + unemployment + province + as.factor(year), data = nosmuggle)
             
# Estimating elasticity through diff-in-diff (time trend, controls) - RESTRICTED PROVINCES
rb6 <- ivreg(lnsales ~ lnprice + rgdp_2002 + unemployment + province + as.factor(year) + province*year | ln_total_tax + rgdp_2002 + unemployment + province + as.factor(year) + province*year, data = nosmuggle)



# ---------------------- EXPORTING THE RESULTS AS LATEX TABLES --------------------------

# Exporting Primary OLS and 2SLS Regressions at a table
stargazer(ols3, iv1, iv2, type = "latex", title = "Main Regressions", out = "~/mainregression.tex", keep = c("lnprice", "rgdp_2002", "unemployment"))

# Exporting Robustness Tests as a table
stargazer(rb1, rb2, rb3, rb4, rb5, rb6, type = "latex", title = "Robustness Test", out = "~/robusttest.tex", keep = c("lnprice", "rgdp_2002", "unemployment"))



# --------------------------------- RUNNING CHOW TEST -----------------------------------

# Running Two Separate Regressions
ctreg1 <- ivreg(lnsales ~ lnprice + rgdp_2002 + unemployment + province + as_factor(year) + province*year | ln_total_tax + year + rgdp_2002 + unemployment + province + as_factor(year) + province*year, data = mdta, subset = (year < 2013))
ctreg2 <- ivreg(lnsales ~ lnprice + rgdp_2002 + unemployment + province + as_factor(year) + province*year | ln_total_tax + rgdp_2002 + unemployment + province + as_factor(year) + province*year, data = mdta, subset = (year >= 2013))

# Run Chow Test
mdta$g2 <- if_else(mdta$year >= 2013, 1, 0)
mdta$g1 <- if_else(mdta$year < 2013, 1, 0)
mdta$g2lnprice <- mdta$g2*mdta$lnprice
mdta$g2rgdp_2002 <- mdta$g2*mdta$rgdp_2002
mdta$g2unemployment <- mdta$g2*mdta$unemployment
mdta$g1lnprice <- mdta$g1*mdta$lnprice
mdta$g1rgdp_2002 <- mdta$g1*mdta$rgdp_2002
mdta$g1unemployment <- mdta$g1*mdta$unemployment

ct3 <- ivreg(lnsales ~ g1lnprice + g1rgdp_2002 + province + as_factor(year) + province*year + g1unemployment + g2lnprice + g2rgdp_2002 + province*g2 + province*year*g2 + g2unemployment | ln_total_tax + g1rgdp_2002 + province + as_factor(year) + province*year + g1unemployment + g2lnprice + g2rgdp_2002 + province*g2 + province*year*g2 + g2unemployment, data = mdta)

t.test(g2lnprice = g1lnprice)
t.test(g2rgdp_2002 = g1rgdp_2002), accum
t.test(g2unemployment = g1unemployment), accum

                                           # END # 

