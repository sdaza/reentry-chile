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


#+ data, warning = FALSE, include = FALSE
# get data
path <- "/Users/sdaza/Dropbox/Projects/re-entry/10 investigadores/sdaza/data/baseline/baseline_08052017.dta"
b <- as.data.table(read_stata(path))
setnames(b, names(b), tolower(names(b)))

ovars <- c("folio_2", "p1", "p7", "p13", "p194", "p195")
nvars <- c("id", "age", "edu", "kids", "fhealth", "mhealth")

setnames(b, ovars, nvars)
b <- b[, ..nvars]

# some descriptives
anyDuplicated(b$id)
table(b$age)
table(b$edu)
table(b$fhealth)
table(b$mhealth)

# load records
load("/Users/sdaza/Dropbox/Projects/re-entry/10 investigadores/sdaza/data/records/register.Rdata")

r <- copy(dat); remove(dat)
setnames(r, names(r), tolower(names(r)))

# select variables
setnames(r, c("fecha ingreso", "encuestadora final"), c("ad", "int"))
r[, admission := cleanDates(ad)]
svars <- c("id","int","admission","c1","c2","c3","c4","ndc1","ndc2","ndc3","ndc4","start","week","twomonths","sixmonths")

r <- r[, ..svars]

# create interviewer id variable
r[, id_int := .GRP, int]
table(r$id_int, useNA = "ifany")

# number of cases
nrow(r)
nrow(b)

#+ merge data, eval = TRUE, include = FALSE, warning = FALSE
setkey(b, id); setkey(r, id)
dat <- b[r]
# dat[is.na(c1)]
# dat[is.na(c2)]

# assign missing data
vars <- c("fhealth", "mhealth")
dat <- assmis(dat, list(vars), list(c(8,9)))
table(dat$fhealth, useNA = "ifany")

#'# Descriptives
#' The total sample is `r nrow(dat)`.

#+ descriptives
summary(dat[, .(age, kids, edu, mhealth)])

#'# Modeling response first week

#'### Is variance of response explained by the Interviewer?
#'
#+ interviewers, warning = FALSE, message = FALSE,  results = "hide"
fit1 <- stan_glmer(c2 ~+ (1|id_int),
                   data = dat,  family = binomial(link = "logit"))
stan_caterpillar(fit1, pars = "b\\[\\(Intercept\\) id_int\\:[0-9]\\]",
                 pars_label = paste0("Interviewer", 1:5))

#'It doesn't seem to be the case!

#'### Predicting response using covariates
#+ stan, warning = FALSE, message = FALSE,  results = "hide"
fit1 <- stan_glmer(c2 ~ age + kids + edu +  mhealth + (1|id_int),
                   data = dat,  family = binomial(link = "logit"))

#+ explore model
color_scheme_set("blue")
posterior <- as.matrix(fit1)
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("age", "kids", "edu", "mhealth"),
           prob = 0.8) + plot_title

ppc_dens_overlay(y = fit1$y,
                 yrep = posterior_predict(fit1, draws = 50))

