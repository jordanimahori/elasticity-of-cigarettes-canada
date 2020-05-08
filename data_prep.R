

# This script takes the various data sources in the /datasets directory and produces the 
# master dataset used for analysis in the paper. Depending on the format outputted, the
# files may have undergone some manual reformatting in Excel to bring them into a tabular
# format and remove unnecessary formatting applied by StatsCan for these summary files. 

# This analysis uses aggregate data, with many associated limitations. Please see the paper
# for a full discussion of these.



# -------------- Preparing the Environment -------------- #

# Clearing Environment 
rm(list = ls())

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


# ----------- Cleaning Tax Dataset ---------- # 

# Replacing "-" with 0 to represent no change in excise tax rates.
for (i in c(4, 6, 8)) {
    excise_tax[[i]][excise_tax[[i]] == "-"] <- 0
}

# Create a "fake" data point on Jan 1 of each year equal to the tax rate in the previous year. 
year <- matrix(nrow = 15, ncol = length(unique(excise_tax$province)))
date <- matrix(nrow = 15, ncol = length(unique(excise_tax$province)))
province <- matrix(nrow = 15, ncol = length(unique(excise_tax$province)))
tax_excise <- matrix(nrow = 15, ncol = length(unique(excise_tax$province)))
tax_sales <- matrix(nrow = 15, ncol = length(unique(excise_tax$province)))
fed_excise <- matrix(nrow = 15, ncol = length(unique(excise_tax$province)))
yday <- matrix(nrow = 15, ncol = length(unique(excise_tax$province)))

for (j in 1:10) {
    for (i in 1:15) {
        year[i,j] <- 2003 + i
        date[i,j] <- str_c(as.character(2003 + i), "01-01", sep = "-")
        province[i,j] <- unique(excise_tax$province)[j]
        tax_excise[i,j] <- NA
        tax_sales[i,j] <- NA
        fed_excise[i,j] <- NA
        yday[i,j] <- NA
    }  
}

add <- bind_cols(year = as.vector(year), date = as_date(as.vector(date)), province = as.vector(province), 
                 tax_excise = as.vector(tax_excise), tax_sales = as.vector(tax_sales), 
                 fed_excise = as.vector(fed_excise), yday = as.vector(yday))

# Identifying any rows where there are observations landing on Jan 1 and filtering them out. 
duplicate <- unique(excise_tax$date[str_which(as.character(excise_tax$date), ("20..-01-01"))])

# Binding with full tax data. Sorting by date. 
tax <- add %>%
    filter(!(add$date %in% duplicate)) %>%
    bind_rows(excise_tax)
tax <- arrange(tax, province, date)

# Filling in dates generated previously using the values from the period immediately before it.
tax <- tax %>%
    group_by(province) %>%
    fill(tax_excise, tax_sales, fed_excise, .direction = "down")

# Within a given year, create a column equal to the number of days until the subsequent date.
tax$yday <- yday(tax$date)

# Calculating the period of time within each year that a given tax rate was in effect. This
# for loop fails on the last iteration (because i + 1 is not defined) but I already have what 
# I need by then.

tax$period <- 0
group_by(tax, province)

for (i in seq_along(tax$date)) {
    if (tax$year[i+1] == tax$year[i])  {
        tax$period[i] <- tax$yday[i+1] - tax$yday[i] + 1
        }
    else {
        tax$period[i] <- 365 - tax$yday[i] + 1
    }
}

# Ungrouping Tibble. Removing unnecessary columns.
tax <- ungroup(tax)
tax <- tax %>% 
    select(-c(change_excise, change_sales, fed_change)) %>%
    filter(year != 2003 & year != 2018)

# Reality Check
# I need to figure out how to sum the number of cases per year.

# Creating a weighted value for tax calculated as the fraction of the year that elapsed during 
# the period multiplied by the tax rate. For simplicity, I'm ignoring leap years in my calculation.

tax <- tax %>%
    mutate(
        wexcise = tax$tax_excise*(period/365), 
        wsales = tax$tax_sales*(period/365), 
        wfedexcise = tax$fed_excise*(period/365)
        )


# I now sum those tax rates within the same year to create an overall weighted tax index that 
# represents the expected tax paid, assuming people are equally likely to smoke on all days. 

tax_merge <- tax %>%
    group_by(province, year) %>%
    summarise(
        wtsales = sum(wsales), 
        wtexcise = sum(wexcise), 
        wtfedexcise = sum(wfedexcise)
    )
tax_merge$year <- as.character(tax_merge$year)

# --------------- Constructing Remaining Variables ---------------- #

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
    left_join(population, by = c("province", "year")) %>%
    left_join(tax_merge, by = c("province", "year"))

###
# This is failing because I need to create a consistent merge ID. 

###


# Filtering to exclude cases where Carton Sales data is not available, or where full years
# are not available. 

mdta <- mdta %>%
    filter(is.na(sales_carton) == FALSE & year > 2003)

# Cleaning up intermediate steps
rm(list = c("weight", "cpi_cig_2002", "cig_prices_2006", "nomgdp", "cpi_general_2002", "population", 
            "rgdp", "sales_carton", "rgdppc", "cig_prices", "tax", "tax_merge", "add", "excise_tax"))



