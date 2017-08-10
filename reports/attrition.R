#'---
#'title: "Response Rate Estimation, Chile Reentry Study"
#' author: ""
#'output: rmarkdown::github_document
#'date: "`r format(Sys.time(), '%B %d, %Y')`"
#'---
#'
#'In this report, we estimate response rates using the following criteria:
#'
#'- The estimation by wave only uses cases far beyond the wave-specific  observation window.
#' For instance, for the wave *2-months*, we only consider those women who have been in the study for 4 months.
#'- We use the observed response rates to simulate the final response rate of the study, and the rate 0.70 for the last wave (*one-year*).
#'- This estimation is based on the administrative records of the study.
#'
#' #### Important:
#'
#' - **Names of waves: baseline, week, two_months, six_months**
#'
#' - **Variables names: d = date, c = clean, so cd = clean date**
#'
#' - **Start = release from prison**
#'
#' - **Deadline = time threshold to compute a given response rates**

#+ setup, eval =TRUE, echo = FALSE
knitr::opts_chunk$set(
  fig.path = "plots/attrition-"
)

#+ libraries, warning = FALSE, include = FALSE
rm(list=ls(all=TRUE))

path <- "/Users/sdaza/Dropbox/Projects/re-entry/10 investigadores/sdaza/data/records/"

library(readxl)
library(sdazar)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggthemes)

#+ get data, include = FALSE
dat <- data.table(read_excel(paste0(path, "20170628 Logros.xlsx"), sheet = "LISTADO TOTAL", skip = 0))
setnames(dat, "ID FOLIO", "id")
anyDuplicated(dat[, id])

text <- lookvar(dat, "NOMBRE")
text <- c(text, lookvar(dat, "APELLIDOS"))
dat[, (text) := NULL]


#+ clean function, include = FALSE
# function to clean
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


#'# Start date

#'We use the date of release to define the start date in the study.
#' Below, a plot with the number of women who began the study by month.

#+ define start date
setnames(dat, "FECHA EGRESO DEFINITIVA", "egreso")
setnames(dat, "FECHA EGRESO", "oegreso")

dat[, start := as.Date(egreso, format = "%Y/%m/%d")] # adjust format of date

# clean dates, see function at the bottom of the document
dat[is.na(start), start := cleanDates(oegreso)]
summary(dat$start)

# format dates
dat[, month := month(start)]
dat[, year := year(start)]
dat[, date := paste0(year, "-", month)]

# define month-year factor to plot
omonths <- paste0(c(rep(2016, length(9:12)), rep(2017, length(1:7))), "-", c(9:12,1:7))
dat[, date := factor(date, levels = omonths )]

# plot with cases by start data
agg <- dat[, .N, date]
ggplot(agg[!is.na(date)], aes(y = N, x = date)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(x = "\nMonth", y = "Number of women\n", title = "Respondents by start date") +
  theme_minimal()

#'# Dates by wave and response rates
#'## Baseline
#'7 cases with no dates, 5 inconsistent.

#+ baseline
# baseline response
text <- lookvar(dat, "Sí: se realizó Línea base")
setnames(dat, text, "baseline")
dat[, baseline := ifelse(grepl("^s", baseline, ignore.case =  TRUE), 1, 0)]
table(dat$baseline, useNA = "ifany")

# interview date
text <- lookvar(dat, "FECHA ENTREVISTA.+Primera fecha")
setnames(dat, text, "dbaseline")

dat[, cdbaseline := cleanDates(dbaseline)] # corrected!
dat[dbaseline == "12-01.2017", cdbaseline := as.Date("2017-01-12")]
dat[dbaseline == "V14 y M18/10", cdbaseline := as.Date("2016-10-18")]
dat[dbaseline == "V16/12716", cdbaseline := as.Date("2016-12-27")]
summary(dat$cdbaseline)

#'To check these cases:

#+ baseline checks
dat[, today := today()]

# baseline time (in days) with respect to start
t <- as.numeric(dat$cdbaseline - dat$start) # difference

mean(t[t <= 0], na.rm = TRUE) # in days
min(t[t <= 0], na.rm = TRUE) # is that possible?
table(t > 0) # only few cases with positive numbers

# check this! missing and inconsistent cases
dat[t > 0 | is.na(cdbaseline), .(id, start, baseline, cdbaseline, dbaseline)]

#'## First Week
#'8 cases missing, 5 inconsistent.

#+ first week
# first week response
setnames(dat, "PARTICIPA", "week")
dat[, week := ifelse(grepl("^s", week, ignore.case =  TRUE), 1, 0)]
table(dat$week, useNA = "ifany")
prop.table(table(dat$week, useNA = "ifany"))

# interview date
setnames(dat, "FECHA ENTREVISTA", "dweek")
dat[, cdweek := cleanDates(dweek)]
dat[dweek == "S29/10 y W02/11", cdweek := as.Date("2016-11-02")]
summary(dat[week ==1, cdweek])

dat[, deadline_week := start +  (7 * 5)] # add 5 weeks to compute rate (not useful, just doing it for completeness)

# difference between first week and start
t <- as.numeric((dat$cdweek - dat$start)/7)

mean(t[t>0], na.rm = TRUE)
max(t[t>0], na.rm = TRUE)  # is this right?
min(t[t>0], na.rm = TRUE)

# check this! missing and inconsistent cases
dat[week == 1 & (t <= 0 | is.na(cdweek)), .(id, start, week,  cdweek, dweek)] # check these cases

(tab <- table(dat[today > week | week == 1, week])) # all the sample
prop.table(tab)
week_rate <- prop.table(tab)[2]

#'## Two months
#'8 cases missing, 1 inconsistent.

#+ two months
# two months response
setnames(dat, "PARTICIPA__1", "two_months")
dat[, two_months := ifelse(grepl("^s", two_months, ignore.case =  TRUE), 1, 0)]
table(dat$two_months, useNA = "ifany")
prop.table(table(dat$two_months, useNA = "ifany"))

# interview date
setnames(dat, "FECHA ENTREVISTA__1", "dtwo_months")
dat[, cdtwo_months := cleanDates(dtwo_months)]
dat[dtwo_months == "13-0-2017", cdtwo_months := as.Date("2017-04-13")] # I guess!
dat[dtwo_months == "27/2", cdtwo_months := as.Date("2017-02-27")]
summary(dat[two_months == 1, cdtwo_months])

# difference between two months and start
t <- as.numeric((dat$cdtwo_months - dat$start) / 30.5) # in months

mean(t[t>0], na.rm = TRUE)
max(t[t>0], na.rm = TRUE)  # 4 months?
min(t[t>0], na.rm= TRUE)

# check this! missing and inconsistent cases
dat[two_months == 1 & (t < 0 | is.na(cdtwo_months)) , .(id, start, two_months, cdtwo_months, dtwo_months)]

dat[, deadline_two_months := start + 30.5 * 4] # add 4 months
length(dat[today > deadline_two_months | two_months == 1, deadline_two_months]) # all of them!
(tab <- table(dat[today > deadline_two_months | two_months == 1, two_months]))
prop.table(tab)
two_months_rate <- prop.table(tab)[2]

#'## Six months
#'3 missing dates, no inconsistencies!

#+ six months
# six months response
setnames(dat, "PARTICIPA__2", "six_months")
dat[, six_months := ifelse(grepl("^s", six_months, ignore.case =  TRUE), 1, 0)]
table(dat$six_months, useNA = "ifany")
prop.table(table(dat$six_months, useNA = "ifany"))

# interview date
setnames(dat, "FECHA ENTREVISTA__2", "dsix_months")
dat[, cdsix_months := cleanDates(dsix_months)]
dat[cdsix_months == "0207-05-15", cdsix_months := as.Date("2017-05-15")]
dat[cdsix_months == "3017-04-24 ", cdsix_months := as.Date("2017-04-24 ")]
summary(dat[six_months == 1, cdsix_months])

t <- as.numeric((dat$cdsix_months - dat$start) / 30.5)

mean(t[t>0], na.rm = TRUE)
max(t[t>0], na.rm = TRUE)
min(t[t>0], na.rm = TRUE) # 4 months, really?

# check this! missing cases
dat[six_months == 1 & (t < 0 | is.na(cdsix_months)), .(id, start, six_months, cdsix_months, dsix_months)]

dat[, deadline_six_months := start + 30.5 * 8] # add 8 months

(tab <- table(dat[today > deadline_six_months, six_months]))
prop.table(tab)

six_months_rate <- prop.table(tab)[2]

#'## Time of response
#+ plot time by application, echo=FALSE, results='hide',message=FALSE, warning=FALSE
t2 <- as.numeric((dat$cdweek - dat$start))
t3 <- as.numeric((dat$cdtwo_months - dat$start))
t4 <- as.numeric((dat$cdsix_months - dat$start))

t <- data.table(id = dat$id, "1 week" = t2, "2 months" = t3, "6  months" = t4)
t <- melt(t, id.vars = "id", value.name = "days", variable.name = "wave")
t[, wave := factor(wave)]
table(t$wave)

ggplot(t, aes(days, fill = wave, colour = wave)) +
  geom_density(alpha = 0.1) + theme_minimal() + theme(legend.position="top") +
   xlim(0, 365) + labs(x = "\nDays from prison release",  y = "Density\n", title = "Density of time to interview from prison release")+
  geom_vline(xintercept = 7, color = "gray", size = 0.7, linetype = "dotted") +
  geom_vline(xintercept = 30.5 * 2, color = "gray", size = 0.7, linetype = "dotted") +
  geom_vline(xintercept = 30.5 * 6, color = "gray", size = 0.7, linetype = "dotted") +
  annotate("text", x = 7, y = 0.13, label = "1 week", size = 3) +
  annotate("text", x = 30.5 * 2, y = 0.07, label = "2 months", size = 3) +
  annotate("text", x = 30.5 * 6, y = 0.04, label = "6 months", size = 3)

#'## Summary response rates
#'From first week to six months.

#+ print rates
cbind(week_rate, two_months_rate, six_months_rate)

#'## Simulate final response rate
#'All the effort should be put during these last waves!

#+ simulate rates
fcases <- list()
pcases <- list()
fup <- list()
i <- 1
for (i in 1:1000) {

  dat[today < deadline_six_months & six_months != 1, six_months_s := ifelse(runif(.N) < six_months_rate, 1, 0)]
  dat[six_months == 1, six_months_s := 1]
  dat[is.na(six_months_s), six_months_s := six_months]

  dat[, year_s := 0]
  dat[six_months_s == 1, year_s := ifelse(runif(.N) < .70, 1, 0)] # using rate .70
  fcases[[i]] <- table(dat$year_s)[2]
  pcases[[i]] <- prop.table(table(dat$year_s))[2] # respect to the total

  agg <- dat[, .N, .(baseline,week,two_months,six_months_s,year_s)]
  s <- sum(agg$N)
  agg[, prop := N / sum(N)]
  agg[, followups := apply(.SD, 1, sum), .SDcols = c("week", "two_months", "six_months_s","year_s")]
  fup[[i]] <- agg[followups > 2, sum(N)]/ s

}


#'The expect number of cases in the final sample:

#+ fcases
summary(unlist(fcases))

#'Cumulative response rate by the final wave:

#+ pcases
summary(unlist(pcases))

#'Expected proportion of cases with 2 or more waves:
#+
summary(unlist(fup))

#'# Appendix

#'## Function to clean dates

#+ functions, eval = FALSE
cleacdates <- function(text) {

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
  suppressWarnings(a[, cd := ymd(paste(yeıar, month, day, sep= "-"))])

  return(a$cd)
}

#+ save data, include = FALSE
save(dat, file = "/Users/sdaza/Dropbox/Projects/re-entry/10 investigadores/sdaza/data/records/register.Rdata")

