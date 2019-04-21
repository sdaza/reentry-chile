###########################
# social integration report
# load data
# author: sebastian daza
############################

library(sdazar)

# first week

dps = fread('data/180829_1_primerasemana.csv')
setnames(dps, names(dps), tolower(names(dps)))

setnames(dps, 'folio2', 'reg_folio')

# temporary housing
spent_night = lookvar(dps, 'p20_c_dia[0-9]$')
dps = assmis(dps, list(spent_night), list(c(-99, -88)))
table(dps[, spent_night[1], with=FALSE], useNA='ifany')

dps[, temp_housing := apply(.SD, 1,
                            function(x) as.numeric(any(x %in% c(1, 11, 12, 13, 7, 8)))),
    .SDcols=spent_night]

table(dps$temp_housing)

# money from familiy (including partner)
dps = assmis(dps, list(c('p43_4_a', 'p43_3_a')), list(c(-9, -8, -99, -88)))
table(dps$p43_4_a, useNA='ifany')

dps[, money_family := apply(.SD, 1,
                            function(x) as.numeric(any(x == 1))),
    .SDcols=c('p43_4_a', 'p43_3_a')]
table(dps$money_family, useNA='ifany')

# living with family
dps[, living_with_family := apply(.SD, 1,
                            function(x) as.numeric(any(x %in% 4:6))),
    .SDcols=spent_night]

table(dps$living_with_family, useNA='ifany')

dps[, family_support := apply(.SD, 1,
                            function(x) as.numeric(any(x == 1))),
    .SDcols=c('living_with_family', 'money_family')]

table(dps$family_support, useNA='ifany')

# any job
dps = assmis(dps, list('p22'), list(c(-9, -8, -99, -88)))
dps[, work := p22]

table(dps$work, useNA='ifany')

# public services
money_public_programs = c('p43_1_a', 'p43_2_a')
dps = assmis(dps, list(money_public_programs), list(c(-9, -8, -99, -88)))

dps[, money_pp := apply(.SD, 1,
                        function(x) as.numeric(any(x == 1))),
    .SDcols=money_public_programs]

table(dps$money_pp, useNA='ifany')

dps[, .(reg_folio, family_support, temp_housing, work, money_pp)]

# 2 months

d2m = fread('data/180118_2_dosmeses.csv')
setnames(d2m, names(d2m), tolower(names(d2m)))

setnames(dps, 'folio2', 'reg_folio')



