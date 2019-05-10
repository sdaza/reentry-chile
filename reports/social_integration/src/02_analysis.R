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

# model to create wave plots
plan(multiprocess)
m0 = brm_multiple(mvbind(money_family, living_with_family, temp_housing, spent_night, work_formal, work_informal,
                         money_pp, contact_pp) ~
                  time,
                  data = imp,
                  family = bernoulli(),
                  control = list(adapt_delta=0.90),
                  chains = 2)

depvars = c('moneyfamily', 'livingwithfamily', 'temphousing',
  'spentnight', 'workformal', 'workinformal', 'moneypp', 'contactpp')

fitted_values = list()
newdata = data.frame(time = factor(1:4))
for (i in depvars) {
  print(paste0('fitting values for ', i))
  pp = fitted(m0, newdata = newdata, resp = i,
              summary = TRUE, robust = TRUE)
  fitted_values[[i]] = data.table(pp)[, dep := i][, time := 1:4]
}

fitted_values = rbindlist(fitted_values)

fitted_values[, time := factor(time,
              levels = c(1:4),
              labels =c('Primera semana', 'Dos meses', 'Seis meses', 'Doce meses'))]

cnames = c('Dinero familiares', 'Vive con familiares', 'Vivienda temporal', 'Noche en lugar de riesgo',
           'Trabajo formal', 'Trabajo informal', 'Dinero programas', 'Contacto instituciones')


fitted_values[, dep := factor(dep, labels = cnames, levels = depvars)]

depvars_list = list(
  'family' = cnames[1:2],
  'housing'  = cnames[3:4],
  'work'  = cnames[5:6],
  'public'  = cnames[7:8]
  )


create_plots_outcome = function(depvars, plotname) {

  savepdf(paste0('output/', plotname))

  print(
  ggplot(fitted_values[dep %in% depvars],
        aes(x=time, y=Estimate, group=dep, color=dep)) +
  geom_pointrange(aes(ymin=Q2.5, ymax=Q97.5), position = position_dodge(width=0.2)) +
  labs(x='\nOla\n', y='Probabilidad\n', caption = "Nota: Intervalos de credibilidad (95%), 50 imputaciones")  +
  scale_y_continuous(breaks = seq(0, 1, 0.10), limits = c(0,.90)) +
  scale_color_manual(values=c("#f03b20", "#2c7fb8")) +
  theme_classic() +
  theme(legend.position="top", legend.title=element_blank(),
    plot.caption = element_text(hjust = 0, size = 6, face = "italic"))
  )
  dev.off()

}

for (i in seq_along(depvars_list)) {
  create_plots_outcome(depvars_list[[i]], names(depvars_list[i]))
  }


# predictors model

plan(multiprocess)
m1 = brm_multiple(mvbind(money_family, living_with_family, temp_housing, spent_night, work_formal, work_informal,
                         money_pp, contact_pp) ~
                  time + age + n_children +
                  mental_health + drug_dep_abuse + previous_sentences + (1|p|reg_folio),
                  data = imp,
                  family = bernoulli(),
                  control = list(adapt_delta=0.90),
                  chains = 2)

cmap = list('Intercept' = 'Constante',
            'time2' = 'Dos meses',
            'time3' = 'Seis meses',
            'time4' = 'Doce meses',
            'age' = 'Edad',
            'n_children' = 'Número de hijos',
            'mental_health' = 'Problemas de salud mental',
            'drup_dep_abuse' = 'Dependencia / abuso drogas',
            'previous_sentences' = 'Número de condenas previas')



dep_regular_exp = c('^moneyfamily_', '^livingwithfamily_', '^temphousing_',
  '^spentnight_', '^workformal_', '^workinformal_',
  '^moneypp_', '^contactpp_')

for (i in seq_along(dep_regular_exp)) {
  print(paste0('::::: create table number ', i))
  texreg_objs[[i]] = extract.brms.select_coeff(m1, coeff_pattern = dep_regular_exp[i],
                                               include.r2 = TRUE)
}

ndeps = length(dep_regular_exp)

texreg(texreg_objs,
          custom.coef.map = cmap,
          custom.model.names = cnames,
          groups = list('Ola (ref = primera semana)' = 2:4),
          ci.test = FALSE,
          float.pos = "htp",
          caption = paste0('Modelo Bayesiano multivariable (',
                           ndeps, ' variables dependientes)'),
          booktabs = TRUE,
          use.packages = FALSE,
          dcolumn = TRUE,
          caption.above = TRUE,
          scalebox = 0.80,
          # fontsize = 'scriptsize',
          label = "integracion_social_m1",
          sideways = TRUE,
          digits = 2,
          custom.note = paste0("Intervalos de credibilidad 95\\%. Coeficientes corresponden a un modelo con ", ndeps, " variables dependientes.
          Efectos aleatorios y correlaciones entre variables dependientes son omitidos."),
          file = 'output/integracion_social_m1.tex'
          )
