

# This script takes the various data sources in the /datasets directory and produces the 
# master dataset used for analysis in the paper. Depending on the format outputted, the
# files may have undergone some manual reformatting in Excel to bring them into a tabular
# format and remove unnecessary formatting applied by StatsCan for these summary files. 

# This analysis uses aggregate data, with many associated limitations. Please see the paper
# for a full discussion of these.



# -------------- Preparing the Environment -------------- #

# Loading Packages
library(tidyverse)
library(lubridate)

# Loading Datasets to Memory
cig_prices_2006 <- read_csv("datasets/cigarette_prices_2006.csv")
cpi_cig_2002 <- read_csv("datasets/cpi_2002_cigarettes.csv")
cpi_general_2002 <- read_csv("datasets/cpi_2002_general.csv")
excise_tax <- read_csv("datasets/excise_tax_cigs.csv")
nomgdp <- read_csv("datasets/ngdp.csv")
population <- read_csv("datasets/population.csv")
sales_carton <- read_csv("datasets/sales_cartons_cigarettes.csv")



# ----------- Function for Averaging Monthly Values ---------- # 

# Replacing "-" with 0 to represent no change in excise tax rates.
for (i in c(4, 6, 8)) {
    excise_tax[[i]][excise_tax[[i]] == "-"] <- 0
}

# The dataset was constructed by taking the change in tax rates that we obtained from one source
# and using that to extrapolate from a snapshot of excise taxes in 2016 to obatin the tax rates 
# in every year in each province. It starts on December 20, 2003 so for the first value I'm 
# changing the date to Jan 1, 2004. 

excise_tax <- select(excise_tax, !c(change_excise, change_sales, fed_change))

excise_tax$date[excise_tax$year == 2003] <- as_date("2004-01-01")
excise_tax$yday <- yday(excise_tax$date)




# Pseudo Code: Want to create an weighted average of tax for each year. Weights applied based on when the tax 
# change came into effect. 




# Create a "fake" data point on Jan 1 of each year equal to the tax rate in the previous year. 

year <- matrix(nrow = 15, ncol = length(unique(excise_tax$province)))
date <- matrix(nrow = 15, ncol = length(unique(excise_tax$province)))
province <- matrix(nrow = 15, ncol = length(unique(excise_tax$province)))
tax_excise <- matrix(nrow = 15, ncol = length(unique(excise_tax$province)))
tax_sales <- matrix(nrow = 15, ncol = length(unique(excise_tax$province)))
fed_excise <- matrix(nrow = 15, ncol = length(unique(excise_tax$province)))
yday <- matrix(nrow = 15, ncol = length(unique(excise_tax$province)))


for (i in 1:10) {
    for (j in 1:15) {
        year[j,i] <- 2003 + j
        date[j,i] <- str_c(as.character(2003 + j), "01-01", sep = "-")
        province[j,i] <- unique(excise_tax$province)[i]
        tax_excise[j,i] <- NA
        tax_sales[j,i] <- NA
        fed_excise[j,i] <- NA
        yday[j,i] <- NA
    }  
}

add <- bind_cols(year = as.vector(year), date = as_date(as.vector(date)), province = as.vector(province), 
                 tax_excise = as.vector(tax_excise), tax_sales = as.vector(tax_sales), 
                 fed_excise = as.vector(fed_excise), yday = as.vector(yday))

# Identifying any rows where there are observations landing on Jan 1.
unique(excise_tax$date[str_detect(as.character(excise_tax$date), "20??-01-01")])

# Filtering out those rows from my constructed dummy values. Binding with full tax data.
add <- filter(add, date != as_date("2012-01-01"))
tax <- bind_rows(excise_tax, add)



# --------------- Constructing Variables ---------------- #

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
names(cig_prices)[2:19] <- (c("price_2001", "price_2002", "price_2003", "price_2004", "price_2005",
                              "price_2006", "price_2007", "price_2008", "price_2009", "price_2010",
                              "price_2011", "price_2012", "price_2013", "price_2014", "price_2015", 
                              "price_2016", "price_2017", "price_2018"))

# Reshaping cig_prices to Conform to Single-Observation by Row
cig_prices <- cig_prices %>%
    pivot_longer(-province, names_to = "year", values_to = "cig_price")

cig_prices$year <- cig_prices$year %>%
    str_sub(start = 7L)

# Calculating Yearly Real GDP by Province from Nominal GDP and CPI
rgdp <- left_join(nomgdp, cpi_general_2002, by = "province")
rgdp$cpi_2001 <- as.numeric(rgdp$cpi_2001)
rgdp$cpi_2002 <- as.numeric(rgdp$cpi_2002)

for (i in 2:18) {
    rgdp[[i]] <- rgdp[[i]]/rgdp[[(i+17)]]*100
}

names(rgdp)[2:18] <- c("rgdp_2001", "rgdp_2002", "rgdp_2003", "rgdp_2004", "rgdp_2005", "rgdp_2006", 
                       "rgdp_2007", "rgdp_2008", "rgdp_2009", "rgdp_2010", "rgdp_2011", "rgdp_2012", 
                       "rgdp_2013", "rgdp_2014", "rgdp_2015", "rgdp_2016", "rgdp_2017")

rgdp <- select(rgdp, 1:18)

rgdp <- rgdp %>%
    pivot_longer(-province, names_to = "year", values_to = "rgdp")

rgdp$year <- rgdp$year %>% 
    str_sub(start = 6L)

# Re-scaling RGDP, since the original dataset has units of (1,000,000)s
rgdp$rgdp <- rgdp$rgdp*(1000000)

# Changing unit of observation for population to be Province-Year.
population <- population %>%
    pivot_longer(-province, names_to = "year", values_to = "population") 

population$year <- population$year %>%
    str_sub(start = 2L)

# Joining Population and RGDP. Generating RGDP Per Capita.
rgdppc <- rgdp %>% 
    left_join(population, by = c("province", "year")) %>%
    mutate(rgpppc = rgdp / population) %>%
    select(-c(rgdp, population))

# Reshaping Cigarette Sales (by Carton) Data
sales_carton <- sales_carton %>%
    pivot_longer(-province, names_to = "year", values_to = "sales_carton")

sales_carton$year <- sales_carton$year %>%
    str_sub(start = 2L)


# ----------------- Joining Data Sources to Single Dataset ---------------------- # 

# Joining Data Sources
mdta <- cig_prices %>%
    left_join(sales_carton, by = c("province", "year")) %>%
    left_join(rgdppc, by = c("province", "year")) %>% 
    left_join(population, by = c("province", "year"))

# Filtering to exclude cases where Carton Sales data is not available, or where full years
# are not available. 

mdta <- mdta %>%
    filter(is.na(sales_carton) == FALSE & year > 2003)

# Cleaning up intermediate steps
rm(list = c("weight", "cpi_cig_2002", "cig_prices_2006", "nomgdp", "cpi_general_2002", "population", 
            "rgdp", "sales_carton", "rgdppc", "cig_prices"))


