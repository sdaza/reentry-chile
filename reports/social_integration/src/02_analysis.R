###########################
# social integration report
# analysis
# author: sebastian daza
############################

library(data.table)
library(brms)
library(mice)
library(miceadds)
library(micemd)
library(texreg)
library(future)
library(countimp)

source('src/utils.R')

# load data
df = readRDS('output/clean_data.rd')
names(df)

fvars = c('edu', 'crime', 'time')
df[, c(fvars) := lapply(.SD, factor), .SDcols = fvars]

# imputation
mdf = df[, .(reg_folio, time, money_family, living_with_family,
              temp_housing, spent_night, work_informal, work_formal,
              contact_pp, money_pp, age, edu, n_children, crime,
              previous_sentences, mental_health, drug_dep_abuse,
              sentence_length, family_support_conflict)]

mdf[, age2 := age^2]

predM = mice::make.predictorMatrix(data = mdf)
predM[, "reg_folio"] = -2
impMethod = mice::make.method(data=mdf)
l2vars = c('previous_sentences', 'family_support_conflict')
predM[l2vars, 'time'] = 0
impMethod[l2vars] <- '2lonly.function'
imputationFunction <- list('previous_sentences' = 'pmm' , 'family_support_conflict' = 'pmm')
cluster_var <- list('previous_sentences' = 'reg_folio', 'family_support_conflict' = 'reg_folio')

#impMethod

imp = mice::mice(mdf, predictorMatrix = predM, m = 10, maxit = 30,
               method = impMethod,
               imputationFunction = imputationFunction,
               cluster_var = cluster_var,
               print = FALSE)

# explore quality of the imputation
plot(imp)
densityplot(imp)

# model using imputation using future package
plan(multiprocess)
m1 = brm_multiple(mvbind(money_family, living_with_family, temp_housing, spent_night, work_formal, work_informal,
                         money_pp, contact_pp) ~
                  time + age + n_children +
                  mental_health + drug_dep_abuse + previous_sentences + (1|p|reg_folio),
                  data = imp,
                  family = bernoulli(),
                  control = list(adapt_delta=0.90),
                  chains = 2)

screenreg(m1)

screenreg(m1,  omit.coef = '^temphousing|^workformal|^moneypp')
screenreg(m1,  omit.coef = '^moneyfamily|^workformal|^moneypp')
screenreg(m1,  omit.coef = '^moneyfamily|^temphousing|^moneypp')
screenreg(m1,  omit.coef = '^moneyfamily|^temphousing|^moneypp')

# coefficient values
setNames(as.list(c(1, 2)), c("foo", "bar"), omit.coef = '^temphousing|^workformal|^moneypp')

coefficients = c('moneyfamily_Intercept', 'temphousing_Intercept', 'workformal_Intercept', 'moneypp_Intercept',
'moneyfamily_time2', 'moneyfamily_time3', 'moneyfamily_time4', 'moneyfamily_age',
'moneyfamily_age2', 'moneyfamily_n_children', 'moneyfamily_family_support_conflict',
'moneyfamily_mental_health', 'moneyfamily_drug_dep_abuse', 'moneyfamily_previous_sentences',
'temphousing_time2', 'temphousing_time3', 'temphousing_time4', 'temphousing_age', 'temphousing_age2',
'temphousing_n_children', 'temphousing_family_support_conflict', 'temphousing_mental_health',
'temphousing_drug_dep_abuse', 'temphousing_previous_sentences',
'workformal_time2', 'workformal_time3', 'workformal_time4',
'workformal_age', 'workformal_age2', 'workformal_n_children',
'workformal_family_support_conflict', 'workformal_mental_health',
'workformal_drug_dep_abuse', 'workformal_previous_sentences',
'moneypp_time2', 'moneypp_time3', 'moneypp_time4', 'moneypp_age', 'moneypp_age2',
'moneypp_n_children', 'moneypp_family_support_conflict', 'moneypp_mental_health',
'moneypp_drug_dep_abuse', 'moneypp_previous_sentences')


coefficients_recode = coefficients
coefficients[grepl('Intercept', coefficients)]

# let's assume this model is multivariate (several dependent variables)

dep = runif(30)
dep1_var1 = runif(30)
dep1_var2 = runif(30)
dep2_var1 = runif(30)
dep2_var2 = runif(30)

m = lm(dep ~ dep1_var1 + dep1_var2 + dep2_var1 + dep2_var2 - 1)
custom.map.coef = list(
                       dep1_var1 = 'var1',
                       dep1_var1 = 'var1',


screenreg(list(m, m),
  custom.model.names = c('dep1', 'dep2'))
