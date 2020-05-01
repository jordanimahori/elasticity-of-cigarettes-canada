clear all
set more off

cd "/Users/jordan/Documents/STATA/ECO446"

import excel "/Users/jordan/Desktop/stata_temp_dataset.xlsx", sheet("Sheet1") firstrow clear

log close _all

log using "ECO446_paper.log", replace



// generating variables
gen total_tax = excise_tax_nom + sales_tax_nom + fed_excise_nom
gen lnsales = ln(cartons_pc)
gen lnprice = ln(price_2002)
gen lntotal_tax = ln(total_tax)

encode province, generate(province_var)


// cleaning data
drop if year == 2001
drop if year == 2002
drop if year == .


// testing first stage
regress lnprice lntotal_tax rgdp_2002 i.province_var i.year i.province_var#c.year c.rgdp_2002#i.province_var

// estimating elasticity through diff-in-diff (no time trend)
ivregress 2sls lnsales (lnprice = total_tax) i.province_var i.year 

// estimating elasticity through diff-in-diff (time trend, no RGDP)
ivregress 2sls lnsales (lnprice = lntotal_tax) i.province_var i.year i.province_var#c.year 

// estimating elasticity through diff-in-diff (time trend, RGDP, no interaction)
ivregress 2sls lnsales (lnprice = lntotal_tax) rgdp_2002 i.province_var i.year ///
i.province_var#c.year 

// estimating elasticity through diff-in-diff (time trend, RGDP & interaction )
ivregress 2sls lnsales (lnprice = lntotal_tax) rgdp_2002 i.province_var i.year ///
i.province_var#c.year c.rgdp_2002#i.province_var
