###########################
# social integration report
# load data
# author: sebastian daza
############################

library(data.table)

# auxiliary functions

# function check if any values are present
any_values = function(x, values) as.numeric(any(x %in% values))

# first week
dps = fread('data/180829_1_primerasemana.csv')
setnames(dps, names(dps), tolower(names(dps)))
setnames(dps, 'folio2', 'reg_folio')

# replace all missing values for NA
dps = dps[, lapply(.SD, function(x) ifelse(x < 0, NA, x)), .SDcols = names(dps)]

# temporary housing
dps[, temp_housing := apply(.SD, 1, any_values, c(1, 11, 12, 13, 7, 8)),
    .SDcols= names(dps) %like% 'p20_c_dia[0-9]$']

table(dps$temp_housing)

# money from familiy (including partner)
dps[, money_family := apply(.SD, 1, any_values, 1),
    .SDcols=c('p43_4_a', 'p43_3_a')]

table(dps$money_family, useNA='ifany')

# living with family
dps[, living_with_family := apply(.SD, 1, any_values, 4:6),
    .SDcols=names(dps) %like% 'p20_c_dia[0-9]$']

table(dps$living_with_family, useNA='ifany')

dps[, family_support := apply(.SD, 1, any_values, 1),
    .SDcols=c('living_with_family', 'money_family')]

table(dps$family_support, useNA='ifany')

# any job
dps[, work := p22]

table(dps$work, useNA='ifany')

# receiving money from public services
dps[, money_pp := apply(.SD, 1, any_values, 1),
    .SDcols=c('p43_1_a', 'p43_2_a')]

table(dps$money_pp, useNA='ifany')

dps[, .(reg_folio, family_support, temp_housing, work, money_pp)]

# 2 months

d2m = fread('data/180118_2_dosmeses.csv')
setnames(d2m, names(d2m), tolower(names(d2m)))
setnames(d2m, 'folio2', 'reg_folio')

#



