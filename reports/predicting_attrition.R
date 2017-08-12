#'---
#'title: "Modeling Attrition , Chile Reentry Study"
#'author:
#'output: rmarkdown::github_document
#'date: "`r format(Sys.time(), '%B %d, %Y')`"
#'---

#'We use the baseline dataset to explore which factors seems to predict attrition,
#' and to identify potential biases of the data.

#+ setup, include = FALSE
knitr::opts_chunk$set(fig.path = "plots/predict-attrition-")

#'# Data setup
#'
#+ libraries, warning=FALSE, include=FALSE
# clean workspace
rm(list=ls(all=TRUE))

# libraries
library(haven)
library(sdazar)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(lme4)
library(rstanarm)
library(bayesplot)
library(StanCat)
library(psych)
library(brms)

# clean date function
cleanDates <- function(text) {
  a <- data.table(text)
  a[, temp := str_extract(text, "([0-9]+/[0-9]+/[0-9]+)|([0-9]+-[0-9]+-[0-9]+)")]
  a[, temp := str_replace_all(temp, "-", "/")]
  a[grepl("20[0-9]+", temp), cc := as.Date(temp, format = "%d/%m/%Y")]
  a[is.na(cc) && !grep("20[0-9]+", temp), cc := as.Date(temp, format = "%d/%m/%y")]
  a[is.na(cc) && grep("20[0-9]+", temp), cc := as.Date(temp, format = "%d/%m/%Y")]
  a[is.na(cc) && !grep("20[0-9]+", temp), cc := as.Date(temp, format = "%y/%m/%d")]
  a[is.na(cc) && grepl("20[0-9]+", temp), cc := as.Date(temp, format = "%Y/%m/%d")]
  suppressWarnings(a[as.numeric(text) > 0,
                     cc := as.Date(as.numeric(text), origin = "1899-12-30")])

  a[, year := year(cc)]
  a[, month := month(cc)]
  a[, day := day(cc)]

  a[month %in% c(9:12), year := 2016]
  a[month %in% c(1:8), year := 2017]
  suppressWarnings(a[, nd := ymd(paste(year, month, day, sep= "-"))])

  return(a$nd)
}


#+ data, warning = FALSE, include = TRUE
# get data
path <- "/Users/sdaza/Dropbox/Projects/re-entry/10 investigadores/sdaza/data/baseline/baseline_08052017.dta"
b <- as.data.table(read_stata(path))

# rename variables
setnames(b, names(b), tolower(names(b)))
ovars <- c("folio_2", "p1", "p7", "p13", "p194", "p195")
nvars <- c("id", "age", "edu", "kids", "s_health", "s_mental_heath")

setnames(b, ovars, nvars)
lb <- copy(b) # keep original version

hist(b$age)

##################################
# define variables
##################################

# self-reported health
vars <- c("s_health", "s_mental_heath")
dat <- assmis(b, list(vars), list(c(8,9)))

# kids binary
b[, any_kids := ifelse(kids > 0, 1, 0)]
table(dat$s_mental_heath, useNA = "ifany")

# edu binary
b[, edu12 := ifelse(edu >= 12, 1, 0)] # cuarto medio +
table(b$edu12, useNA = "ifany")

# mental health
vars <- lookvar(b, "salud")
b <- assmis(b, list(vars), list(c(8,9)))
# f <- fa(b[, vars, with = FALSE], nfactors = 4)
summary(psych::alpha(as.data.frame(b[, vars, with = FALSE])))

b[, mental_health_score := rowscore(b, vars, type = "mean")]
summary(b$mental_health_score)
hist(b$mental_health_score)

# interviewer assessment (too many missing cases)
vars <- lookvar(b, "evaluar")
b <- assmis(b, list(vars), list(c(8,9)))
b[, interviewer_assessment := rowscore(b, vars, p = 0.01)]
summary(b$interviewer_assessment)

# housing
table(b$p36, useNA = "ifany")
b <- assmis(b, list("p36"), list(c(8,9)))
setnames(b, "p36", "residential_instability")

# employment
b[, employment := rowscore(b, c("p173", "p181"), type = "any", val = 1)]
table(b$employment)
table(b[, .(p173, p181)]) # formal and independent

# substance use issues
b <- assmis(b, list("p210"), list(c(8,9)))
table(b$p210,  useNA = "ifany")
setnames(b, "p210", "drug_use_issues")
table(b$drug_use_issues, useNA = "ifany")

# drug prevalence
b[, hard_drugs := as.numeric(p212_2_d %in% c(1,2) | p212_2_e %in% c(1,2))]
# b[, .(id, hard_drugs, p212_2_d, p212_2_e)]
table(b$hard_drugs, useNA = "ifany")

# expectations to quit crime
table(b$p262, useNA = "ifany")
b <- assmis(b, list(c("p262")), list(c(8,9)))
b <- revscale(b, "p262", "expect_crime")
table(b$expect_crime, useNA = "ifany")

# crimes
table(b$p137, useNA = "ifany")
b[p137 == 7 , crime := 2] # theft
b[p137 %in% c(1:6, 8:11), crime := 1] # robbery
b[p137 %in% c(15:16), crime := 3] # drugs
b[p137 %in% c(12:14, 17:24), crime := 1] # other
table(b$crime, useNA = "ifany")
prop.table(table(b$crime, useNA = "ifany"))

# sentence time
b <- assmis(b, list(c("p138_anos", "p138_dias", "p138_mese")), list(c(88,99)))
b[, sentence_time := sum(p138_anos * 365.25 + p138_mese * 30.5 + p138_dias, na.rm = TRUE), id]
# b[, .(id, sentence_time, p138_anos, p138_mese, p138_dias)]
# hist(log(b$sentence_time))

# select variables
nvars <- c("id", "age", "edu12", "any_kids", "s_health", "s_mental_heath",
           "mental_health_score", "interviewer_assessment",
           "residential_instability", "employment", "sentence_time", "drug_use_issues",
           "hard_drugs", "expect_crime", "crime")

b <- b[, ..nvars]

# load records
load("/Users/sdaza/Dropbox/Projects/re-entry/10 investigadores/sdaza/data/records/register.Rdata")

r <- copy(dat); remove(dat)
setnames(r, names(r), tolower(names(r)))

# select variables
setnames(r, c("fecha ingreso", "encuestadora final"), c("ad", "int"))
r[, admission := cleanDates(ad)]
svars <- c("id","int","admission","week","two_months","six_months", "cdweek", "cdtwo_months")

r <- r[, ..svars]

# create interviewer id variable
r[, id_int := .GRP, int]
table(r$id_int, useNA = "ifany")

#+ merge data, eval = TRUE, include = TRUE, warning = FALSE
# merge cases and standardize some variables
setkey(b, id); setkey(r, id)
dat <- b[r]

# recode some variables for models
dat[, missing_week := ifelse(week == 1, 0, 1)]
dat[, missing_twomonths := ifelse(two_months == 1, 0, 1)]
dat[, log_sentence_time := scale(log(sentence_time + 0.01), center = TRUE, scale = FALSE)]
dat[, z_age := scale(age)]
dat[, z_mental_health_score := scale(mental_health_score)]
dat[, z_residential_instability := scale(residential_instability)]
dat[, crime := factor(crime, labels = c("robbery + others", "theft", "drugs"))]
# dat[, crime := relevel(crime, ref = c("drugs"))]

#'# Descriptives
#' The total sample is `r nrow(dat)`.

#+ descriptives
summary(dat[, .(age, any_kids, edu12, mental_health_score, residential_instability)])

#'# Modeling non-response: First week
#'
#'I show here Bayesian logistic random models where the group variable is the interviewer.
#'
#'### Is variance of response explained by interviewers?
#'
#+ interviewers first week, warning = FALSE, message = FALSE,  results = "hide"
fit1 <- brm(missing_week ~+ (1|id_int),
                   data = dat,  family = bernoulli(link = "logit"),
                    control= list(adapt_delta=.99))

stan_caterpillar(fit1, pars = "^r",
                 pars_label = paste0("Interviewer", 1:5))

#'It doesn't seem to be the case!
#'
#' ### Predicting first week non-response using covariates
#'
#+ stan first week, warning = FALSE, message = FALSE,  results = "hide"

fit1 <- brm(missing_week ~ z_age + any_kids + edu12 + z_mental_health_score +
                   z_residential_instability + employment + hard_drugs + crime +
                    log_sentence_time + (1|id_int),
                   data = dat,  family = bernoulli(link = "logit"),
                    control= list(adapt_delta=.99))

# pp_check(fit1)


# fit1 <- stan_glmer(missing_week ~ z_age + any_kids + edu12 + z_mental_health_score +
                   # z_residential_instability + employment + hard_drugs + crime +
                    # log_sentence_time + (1|id_int),
                   # data = dat,  family = binomial(link = "logit"), adapt_delta = 0.99)


# create plot
posterior <- as.matrix(fit1)

plot_title <- ggtitle("Posterior distributions Predicting Week Non-Response",
                      "with medians and 80% intervals")

# mcmc_areas(posterior) + plot_title

mcmc_areas(posterior,
           regex_pars = c("z_age", "any_kids", "edu12", "z_mental_health_score",
                    "z_residential_instability", "employment", "hard_drugs",
                    "log_sentence_time", "crime"),
           prob = 0.8) + plot_title

#' ### Marginal effects
#'
#+ marginal effects week, warning = FALSE, message = FALSE,  results = "hide"
eff <- c("z_age", "z_residential_instability", "employment")
plot(marginal_effects(fit1, effects = eff, re_formula = NA, spaghetti = FALSE),
  points = TRUE, jitter_width = 0, ask = FALSE)

#' Age, employment and residential stability seem to be key factors here.
#'
#' # Modeling Non-response: Two months
#'
#' ### Is variance of response explained by interviewers?
#'
#' This time there is more variability by interviewer.
#'
#+ interviewers two months, warning = FALSE, message = FALSE,  results = "hide"
fit1 <- brm(missing_twomonths ~+ (1|id_int),
                   data = dat,  family = bernoulli(link = "logit"),
                   control= list(adapt_delta=.99) )

stan_caterpillar(fit1, pars = "^r",
                 pars_label = paste0("Interviewer", 1:5))


#'### Predicting two-months non-response using covariates
#+ stan two months, warning = FALSE, message = FALSE,  results = "hide"
fit1 <- brm(missing_twomonths ~ z_age + any_kids + edu12 +  z_mental_health_score +
                   z_residential_instability + employment + hard_drugs
                   + crime + log_sentence_time + (1|id_int),
                   data = dat,  family = bernoulli(link = "logit"),
                   control= list(adapt_delta=.99))
summary(fit1)
# create plot
posterior <- as.matrix(fit1)

plot_title <- ggtitle("Posterior distributions Predicting Two Months Non-Response",
                      "with medians and 80% intervals")

# mcmc_areas(posterior) + plot_title
mcmc_areas(posterior,
           regex_pars = c("z_age", "any_kids", "edu12", "z_mental_health_score",
                    "z_residential_instability", "employment", "hard_drugs",
                    "log_sentence_time", "crime"),
           prob = 0.8) + plot_title

#' ### Marginal effects
#'
#+ marginal effects two months, warning = FALSE, message = FALSE,  results = "hide"
eff <- c("z_age", "z_residential_instability", "employment")
plot(marginal_effects(fit1, effects = eff, re_formula = NA, spaghetti = FALSE),
  points = TRUE, jitter_width = 0, ask = FALSE)

#' Same patterns, although noisier estimates.
#' Non-response, as expected, is not random and we will need to correct potential biases
#' by imputing or weighting.

