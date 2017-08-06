Modeling Attrition , Chile Reentry Study
================
August 06, 2017

We use the baseline dataset to explore which factors seems to predict attrition, and to identify potential biases of the observed data.

``` r
#+ get data
path <- "/Users/sdaza/Dropbox/Projects/re-entry/10 investigadores/sdaza/data/baseline/baseline_08052017.dta"
b <- as.data.table(read_stata(path))
setnames(b, names(b), tolower(names(b)))

ovars <- c("folio_2", "p1", "p7", "p13", "p194", "p195")
nvars <- c("id", "age", "edu", "kids", "fhealth", "mhealth")

setnames(b, ovars, nvars)
b <- b[, ..nvars]


# some descriptives
anyDuplicated(b$id)
```

    ## [1] 0

``` r
table(b$edad)
```

    ## < table of extent 0 >

``` r
table(b$edu)
```

    ## 
    ##  0  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 
    ## 10  3 10  8  9 17 15 50 11 20  8 51  1 11  2  1

``` r
table(b$fhealth)
```

    ## 
    ##   1   2   3   4   5   8   9 
    ##  31 102  63  24   5   1   1

``` r
table(b$mhealth)
```

    ## 
    ##   1   2   3   4   5   8   9 
    ##  26 132  47  16   2   3   1

``` r
#+ load records
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
```

    ## 
    ##  1  2  3  4  5 
    ## 33 59 58 58 17

``` r
# number of cases
nrow(r)
```

    ## [1] 225

``` r
nrow(b) # 227 ?
```

    ## [1] 227

``` r
# why?
b[!b$id %in% r$id]$id # two ids not in the record file!
```

    ## [1] 40280 10011
    ## attr(,"label")
    ## [1] "folio_2"
    ## attr(,"format.stata")
    ## [1] "%12.0g"

Why we have those missing ids?
