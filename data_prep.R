
# This script takes the various data sources in the /datasets directory and produces the 
# master dataset used for analysis in the paper. Depending on the format outputted, the
# files may have undergone some manual reformatting in Excel to bring them into a tabular
# format and remove unnecessary formatting applied by StatsCan for these summary files. 

# This analysis uses aggregate data, with many associated limitations. Please see the paper
# for a full discussion of these.


# -------------- Preparing the Environment -------------- #

# Loading Packages
library(tidyverse)

# Loading Datasets to Memory
cig_prices_2006 <- read_csv("datasets/cigarette_prices_2006.csv")
cpi_cig_2002 <- read_csv("datasets/cpi_2002_cigarettes.csv")
cpi_general_2002 <- read_csv("datasets/cpi_2002_general.csv")
excise_tax_cigs <- read_csv("datasets/excise_tax_cigs.csv")
nomgdp <- read_csv("datasets/ngdp.csv")
# population <- read_csv("datasets/population.csv")
# sales_carton <- read_csv("datasets/sales_cartons_cigarettes.csv")


# ----------- Function for Averaging Monthly Values ---------- # 





# -------------- Constructing Variables --------------- #

# Creating weights for cigarette prices by province relative to their 2006 values
weight <- cpi_cig_2002
for (i in 2:19) {
    weight[[i]] <- cpi_cig_2002[[i]]/cpi_cig_2002$y2006
}

# Extrapolating from 2006 cigarette prices by weighting using previously generated weights
cig_prices <- left_join(cig_prices_2006, weight, by = "province")

for (i in 3:20) {
    cig_prices[i] <- cig_prices$prices_2006*cig_prices[[i]]
}

cig_prices <- select(cig_prices, !prices_2006)
names(cig_prices)[2:19] <- (c("price_2001", "price_2002", "price_2003", "price_2004", "price_2005", "price_2006", "price_2007", "price_2008", "price_2009", "price_2010", "price_2011", "price_2012", "price_2013", "price_2014", "price_2015", "price_2016", "price_2017", "price_2018"))

# Cleaning up Tax Dataset
for (i in c(4, 6, 8)) {
    excise_tax_cigs[[i]][excise_tax_cigs[[i]] == "-"] <- 0
}

# Calculating Real GDP from Nominal GDP and CPI
rgdp <- left_join(nomgdp, cpi_general_2002, by = "province")
rgdp$cpi_2001 <- as.numeric(rgdp$cpi_2001)
rgdp$cpi_2002 <- as.numeric(rgdp$cpi_2002)

for (i in 2:18) {
    rgdp[[i]] <- rgdp[[i]]/rgdp[[(i+17)]]*100
}

names(rgdp)[2:18] <- c("rgdp_2001", "rgdp_2002", "rgdp_2003", "rgdp_2004", "rgdp_2005", "rgdp_2006", "rgdp_2007", "rgdp_2008", "rgdp_2009", "rgdp_2010", "rgdp_2011", "rgdp_2012", "rgdp_2013", "rgdp_2014", "rgdp_2015", "rgdp_2016", "rgdp_2017")
rgdp <- select(rgdp, 1:18)


# Cleaning up intermediate steps
rm(list = c("weight", "cpi_cig_2002", "cig_prices_2006", "nomgdp", "cpi_general_2002"))
