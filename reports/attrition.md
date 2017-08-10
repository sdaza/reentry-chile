Response Rate Estimation, Chile Reentry Study
================
August 10, 2017

In this report, we estimate response rates using the following criteria:

-   The estimation by wave only uses cases far beyond the wave-specific observation window. For instance, for the wave *2-months*, we only consider those women who have been in the study for 4 months.
-   We use the observed response rates to simulate the final response rate of the study, and the rate 0.70 for the last wave (*one-year*).
-   This estimation is based on the administrative records of the study.

#### Important:

-   **Names of waves: baseline, week, two\_months, six\_months**

-   **Variables names: d = date, c = clean, so cd = clean date**

-   **Start = release from prison**

-   **Deadline = time threshold to compute a given response rates**

Start date
==========

We use the date of release to define the start date in the study. Below, a plot with the number of women who began the study by month.

``` r
setnames(dat, "FECHA EGRESO DEFINITIVA", "egreso")
setnames(dat, "FECHA EGRESO", "oegreso")

dat[, start := as.Date(egreso, format = "%Y/%m/%d")] # adjust format of date

# clean dates, see function at the bottom of the document
dat[is.na(start), start := cleanDates(oegreso)]
summary(dat$start)
```

    ##         Min.      1st Qu.       Median         Mean      3rd Qu. 
    ## "2016-01-01" "2016-10-19" "2016-11-30" "2016-12-05" "2017-01-22" 
    ##         Max. 
    ## "2017-03-31"

``` r
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
```

![](plots/attrition-define%20start%20date-1.png)

Dates by wave and response rates
================================

Baseline
--------

7 cases with no dates, 5 inconsistent.

``` r
# baseline response
text <- lookvar(dat, "Sí: se realizó Línea base")
setnames(dat, text, "baseline")
dat[, baseline := ifelse(grepl("^s", baseline, ignore.case =  TRUE), 1, 0)]
table(dat$baseline, useNA = "ifany")
```

    ## 
    ##   1 
    ## 225

``` r
# interview date
text <- lookvar(dat, "FECHA ENTREVISTA.+Primera fecha")
setnames(dat, text, "dbaseline")

dat[, cdbaseline := cleanDates(dbaseline)] # corrected!
dat[dbaseline == "12-01.2017", cdbaseline := as.Date("2017-01-12")]
dat[dbaseline == "V14 y M18/10", cdbaseline := as.Date("2016-10-18")]
dat[dbaseline == "V16/12716", cdbaseline := as.Date("2016-12-27")]
summary(dat$cdbaseline)
```

    ##         Min.      1st Qu.       Median         Mean      3rd Qu. 
    ## "2016-09-01" "2016-10-07" "2016-11-12" "2016-11-25" "2017-01-11" 
    ##         Max.         NA's 
    ## "2017-03-28"          "7"

To check these cases:

``` r
dat[, today := today()]

# baseline time (in days) with respect to start
t <- as.numeric(dat$cdbaseline - dat$start) # difference

mean(t[t <= 0], na.rm = TRUE) # in days
```

    ## [1] -11.30986

``` r
min(t[t <= 0], na.rm = TRUE) # is that possible?
```

    ## [1] -111

``` r
table(t > 0) # only few cases with positive numbers
```

    ## 
    ## FALSE  TRUE 
    ##   213     5

``` r
# check this! missing and inconsistent cases
dat[t > 0 | is.na(cdbaseline), .(id, start, baseline, cdbaseline, dbaseline)]
```

    ##        id      start baseline cdbaseline dbaseline
    ##  1: 10293 2017-03-25        1       <NA>        NA
    ##  2: 50037 2016-10-19        1 2016-10-30     42673
    ##  3: 10285 2017-03-17        1       <NA>        NA
    ##  4: 20200 2016-12-21        1 2016-12-26     42730
    ##  5: 10272 2017-03-06        1       <NA>        NA
    ##  6: 10288 2017-03-18        1       <NA>        NA
    ##  7: 10202 2016-01-01        1 2016-12-21     42725
    ##  8: 30025 2016-01-05        1 2016-09-21     42634
    ##  9: 10291 2017-03-22        1       <NA>        NA
    ## 10: 40059 2016-10-17        1 2016-10-19 W19/10/16
    ## 11: 20120 2016-10-29        1       <NA>        NA
    ## 12: 10282 2017-03-13        1       <NA>        NA

First Week
----------

8 cases missing, 5 inconsistent.

``` r
# first week response
setnames(dat, "PARTICIPA", "week")
dat[, week := ifelse(grepl("^s", week, ignore.case =  TRUE), 1, 0)]
table(dat$week, useNA = "ifany")
```

    ## 
    ##   0   1 
    ##  44 181

``` r
prop.table(table(dat$week, useNA = "ifany"))
```

    ## 
    ##         0         1 
    ## 0.1955556 0.8044444

``` r
# interview date
setnames(dat, "FECHA ENTREVISTA", "dweek")
dat[, cdweek := cleanDates(dweek)]
dat[dweek == "S29/10 y W02/11", cdweek := as.Date("2016-11-02")]
summary(dat[week ==1, cdweek])
```

    ##         Min.      1st Qu.       Median         Mean      3rd Qu. 
    ## "2016-09-23" "2016-10-31" "2016-12-05" "2016-12-14" "2017-01-21" 
    ##         Max.         NA's 
    ## "2017-04-12"          "8"

``` r
dat[, deadline_week := start +  (7 * 5)] # add 5 weeks to compute rate (not useful, just doing it for completeness)

# difference between first week and start
t <- as.numeric((dat$cdweek - dat$start)/7)

mean(t[t>0], na.rm = TRUE)
```

    ## [1] 1.950972

``` r
max(t[t>0], na.rm = TRUE)  # is this right?
```

    ## [1] 53.28571

``` r
min(t[t>0], na.rm = TRUE)
```

    ## [1] 0.1428571

``` r
# check this! missing and inconsistent cases
dat[week == 1 & (t <= 0 | is.na(cdweek)), .(id, start, week,  cdweek, dweek)] # check these cases
```

    ##        id      start week     cdweek dweek
    ##  1: 50242 2017-02-02    1 2017-01-21 42756
    ##  2: 20081 2016-10-19    1       <NA>    NA
    ##  3: 20032 2016-10-16    1 2016-10-12 42655
    ##  4: 20229 2017-01-18    1 2017-01-07 42742
    ##  5: 50271 2017-03-06    1       <NA>    NA
    ##  6: 20071 2016-10-18    1 2016-10-13 42656
    ##  7: 10285 2017-03-17    1       <NA>    NA
    ##  8: 50276 2017-03-08    1       <NA>    NA
    ##  9: 10272 2017-03-06    1       <NA>    NA
    ## 10: 20142 2016-11-15    1       <NA>    NA
    ## 11: 10288 2017-03-18    1       <NA>    NA
    ## 12: 20261 2017-02-19    1       <NA>    NA
    ## 13: 50249 2017-02-09    1 2017-01-22 42757

``` r
(tab <- table(dat[today > week | week == 1, week])) # all the sample
```

    ## 
    ##   0   1 
    ##  44 181

``` r
prop.table(tab)
```

    ## 
    ##         0         1 
    ## 0.1955556 0.8044444

``` r
week_rate <- prop.table(tab)[2]
```

Two months
----------

8 cases missing, 1 inconsistent.

``` r
# two months response
setnames(dat, "PARTICIPA__1", "two_months")
dat[, two_months := ifelse(grepl("^s", two_months, ignore.case =  TRUE), 1, 0)]
table(dat$two_months, useNA = "ifany")
```

    ## 
    ##   0   1 
    ##  50 175

``` r
prop.table(table(dat$two_months, useNA = "ifany"))
```

    ## 
    ##         0         1 
    ## 0.2222222 0.7777778

``` r
# interview date
setnames(dat, "FECHA ENTREVISTA__1", "dtwo_months")
dat[, cdtwo_months := cleanDates(dtwo_months)]
dat[dtwo_months == "13-0-2017", cdtwo_months := as.Date("2017-04-13")] # I guess!
dat[dtwo_months == "27/2", cdtwo_months := as.Date("2017-02-27")]
summary(dat[two_months == 1, cdtwo_months])
```

    ##         Min.      1st Qu.       Median         Mean      3rd Qu. 
    ## "2016-10-24" "2016-12-24" "2017-01-24" "2017-02-13" "2017-04-11" 
    ##         Max.         NA's 
    ## "2017-07-13"          "8"

``` r
# difference between two months and start
t <- as.numeric((dat$cdtwo_months - dat$start) / 30.5) # in months

mean(t[t>0], na.rm = TRUE)
```

    ## [1] 2.316876

``` r
max(t[t>0], na.rm = TRUE)  # 4 months?
```

    ## [1] 4.491803

``` r
min(t[t>0], na.rm= TRUE)
```

    ## [1] 0.3278689

``` r
# check this! missing and inconsistent cases
dat[two_months == 1 & (t < 0 | is.na(cdtwo_months)) , .(id, start, two_months, cdtwo_months, dtwo_months)]
```

    ##       id      start two_months cdtwo_months dtwo_months
    ## 1: 20081 2016-10-19          1         <NA>          NA
    ## 2: 20095 2016-12-07          1         <NA>          NA
    ## 3: 10099 2016-12-20          1         <NA>          NA
    ## 4: 10187 2016-12-16          1         <NA>          NA
    ## 5: 50104 2016-10-25          1   2016-10-24       42667
    ## 6: 30178 2016-12-06          1         <NA>          NA
    ## 7: 10244 2017-02-04          1         <NA>          NA
    ## 8: 10213 2017-01-08          1         <NA>          NA
    ## 9: 10228 2017-01-18          1         <NA>          NA

``` r
dat[, deadline_two_months := start + 30.5 * 4] # add 4 months
length(dat[today > deadline_two_months | two_months == 1, deadline_two_months]) # all of them!
```

    ## [1] 225

``` r
(tab <- table(dat[today > deadline_two_months | two_months == 1, two_months]))
```

    ## 
    ##   0   1 
    ##  50 175

``` r
prop.table(tab)
```

    ## 
    ##         0         1 
    ## 0.2222222 0.7777778

``` r
two_months_rate <- prop.table(tab)[2]
```

Six months
----------

3 missing dates, no inconsistencies!

``` r
# six months response
setnames(dat, "PARTICIPA__2", "six_months")
dat[, six_months := ifelse(grepl("^s", six_months, ignore.case =  TRUE), 1, 0)]
table(dat$six_months, useNA = "ifany")
```

    ## 
    ##   0   1 
    ## 101 124

``` r
prop.table(table(dat$six_months, useNA = "ifany"))
```

    ## 
    ##         0         1 
    ## 0.4488889 0.5511111

``` r
# interview date
setnames(dat, "FECHA ENTREVISTA__2", "dsix_months")
dat[, cdsix_months := cleanDates(dsix_months)]
dat[cdsix_months == "0207-05-15", cdsix_months := as.Date("2017-05-15")]
dat[cdsix_months == "3017-04-24 ", cdsix_months := as.Date("2017-04-24 ")]
summary(dat[six_months == 1, cdsix_months])
```

    ##         Min.      1st Qu.       Median         Mean      3rd Qu. 
    ## "2017-03-15" "2017-04-24" "2017-05-18" "2017-05-17" "2017-06-12" 
    ##         Max.         NA's 
    ## "2017-07-14"          "3"

``` r
t <- as.numeric((dat$cdsix_months - dat$start) / 30.5)

mean(t[t>0], na.rm = TRUE)
```

    ## [1] 6.203171

``` r
max(t[t>0], na.rm = TRUE)
```

    ## [1] 9.016393

``` r
min(t[t>0], na.rm = TRUE) # 4 months, really?
```

    ## [1] 4.459016

``` r
# check this! missing cases
dat[six_months == 1 & (t < 0 | is.na(cdsix_months)), .(id, start, six_months, cdsix_months, dsix_months)]
```

    ##       id      start six_months cdsix_months dsix_months
    ## 1: 50126 2016-12-07          1         <NA>          NA
    ## 2: 10007 2016-09-20          1         <NA>          NA
    ## 3: 50231 2017-01-22          1         <NA>          NA

``` r
dat[, deadline_six_months := start + 30.5 * 8] # add 8 months

(tab <- table(dat[today > deadline_six_months, six_months]))
```

    ## 
    ##  0  1 
    ## 32 96

``` r
prop.table(tab)
```

    ## 
    ##    0    1 
    ## 0.25 0.75

``` r
six_months_rate <- prop.table(tab)[2]
```

Time of response
----------------

![](plots/attrition-plot%20time%20by%20application-1.png)

Summary response rates
----------------------

From first week to six months.

``` r
cbind(week_rate, two_months_rate, six_months_rate)
```

    ##   week_rate two_months_rate six_months_rate
    ## 1 0.8044444       0.7777778            0.75

Simulate final response rate
----------------------------

All the effort should be put during these last waves!

``` r
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
```

The expect number of cases in the final sample:

``` r
summary(unlist(fcases))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    96.0   118.0   123.0   122.9   127.0   145.0

Cumulative response rate by the final wave:

``` r
summary(unlist(pcases))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.4267  0.5244  0.5467  0.5463  0.5644  0.6444

Expected proportion of cases with 2 or more waves:

``` r
summary(unlist(fup))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.6311  0.6800  0.6933  0.6924  0.7022  0.7467

Appendix
========

Function to clean dates
-----------------------

``` r
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
```
