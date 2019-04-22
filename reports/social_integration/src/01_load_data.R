###########################
# social integration report
# load data
# author: sebastian daza
############################

library(data.table)

# auxiliary functions

# function check if any values are present
any_values = function(x, values) as.numeric(any(x %in% values))

#################
# first week
#################

d1s = fread('data/180829_1_primerasemana.csv')
setnames(d1s, names(d1s), tolower(names(d1s)))
setnames(d1s, 'folio2', 'reg_folio')

# replace all missing values for NA
d1s = d1s[, lapply(.SD, function(x) ifelse(x < 0, NA, x)), .SDcols = names(d1s)]

# temporary housing
d1s[, temp_housing := apply(.SD, 1, any_values, c(1, 7:9, 11:13)),
    .SDcols= names(d1s) %like% 'p20_c_dia[0-9]$']

table(d1s$temp_housing)

# money from familiy (including partner)
d1s[, money_family := apply(.SD, 1, any_values, 1),
    .SDcols=c('p43_3_a', 'p43_4_a')]

table(d1s$money_family, useNA='ifany')

# living with family
d1s[, living_with_family := apply(.SD, 1, any_values, 4:6),
    .SDcols=names(d1s) %like% 'p20_c_dia[0-9]$']

table(d1s$living_with_family, useNA='ifany')

d1s[, family_support := apply(.SD, 1, any_values, 1),
    .SDcols=c('living_with_family', 'money_family')]

table(d1s$family_support, useNA='ifany')

# any job
d1s[, work := p22]

table(d1s$work, useNA='ifany')

# receiving money from public services
d1s[, money_pp := apply(.SD, 1, any_values, 1),
    .SDcols=c('p43_1_a', 'p43_2_a')]

table(d1s$money_pp, useNA='ifany')

# select columns
d1s[, .(reg_folio, family_support, temp_housing, work, money_pp)]

###############
# 2 months
###############

d2m = fread('data/180118_2_dosmeses.csv')
setnames(d2m, names(d2m), tolower(names(d2m)))
setnames(d2m, 'folio2', 'reg_folio')

d2m = d2m[, lapply(.SD, function(x) ifelse(x < 0, NA, x)), .SDcols = names(d2m)]

# any job
# I am using all the weeks available
d2m[, work_formal := apply(.SD, 1, any_values, 1),
    .SDcols=names(d2m) %like% 'trabajo_[0-9]_ocurrencia_[0-9]+']

d2m[is.na(work_formal) & p21==0, work_formal := 0]
d2m[is.na(p21) & !is.na(work_formal), work_formal := NA]
table(d2m$work_formal, d2m$p21, useNA='ifany')

d2m[, work_informal := apply(.SD, 1, any_values, 1),
    .SDcols=names(d2m) %like% 'tbjo_cp_[0-9]_[0-9]+']

d2m[is.na(work_formal) & p28 == 0, work_informal := 0]
d2m[is.na(p28) & !is.na(work_informal), work_informal := NA]
table(d2m$work_informal, d2m$p28, useNA='ifany')

d2m[, work := apply(.SD, 1, any_values, 1),
    .SDcols=c('work_formal', 'work_informal')]

table(d2m$work, useNA='ifany')

# temporary housing
d2m[, temp_housing := apply(.SD, 1, any_values, c(1, 7:9, 11:12)),
    .SDcols= names(d2m) %like% 'lugar_[0-9]_tipo$']

table(d2m$temp_housing, useNA='ifany')

# spent the night in a risky place
d2m[, temp_housing := apply(.SD, 1, any_values, c(1, 7:9, 11:12)),
    .SDcols=names(d2m) %like% 'lugar_[0-9]_tipo$']

table(d2m$temp_housing, useNA='ifany')

d2m[, spent_night := apply(.SD, 1, any_values, c(1:4, 8)),
    .SDcols= names(d2m) %like% 'paso_noche_[0-9]$']

d2m[, temp_housing := apply(.SD, 1, any_values, 1),
    .SDcols=c('temp_housing', 'spent_night')]

# money from familiy (including partner)
d2m[, money_family := apply(.SD, 1, any_values, 1),
    .SDcols=c('p34_a_3', 'p34_a_4')]

table(d2m$money_family, useNA='ifany')

# living with family
d2m[, living_with_family := apply(.SD, 1, any_values, 4:6),
    .SDcols=names(d2m) %like% 'lugar_[0-9]_tipo$']

table(d2m$living_with_family, useNA='ifany')

d2m[, family_support := apply(.SD, 1, any_values, 1),
    .SDcols=c('living_with_family', 'money_family')]

table(d2m$family_support, useNA='ifany')

# receiving money from public services
d2m[, money_pp := apply(.SD, 1, any_values, 1),
    .SDcols=c('p34_a_1', 'p34_a_2')]

table(d2m$money_pp, useNA='ifany')

# select columns
d2m[, .(reg_folio, family_support, temp_housing, work, money_pp)]

