---
title: "Modeling Attrition , Chile Reentry Study"
output: rmarkdown::github_document
date: "August 06, 2017"
---

We use the baseline dataset to explore which factors seems to predict attrition, and to identify potential biases of the observed data.







```r
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

```
## [1] 0
```

```r
table(b$edad)
```

```
## < table of extent 0 >
```

```r
table(b$edu)
```

```
## 
##  0  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 
## 10  3 10  8  9 17 15 50 11 20  8 51  1 11  2  1
```

```r
table(b$fhealth)
```

```
## 
##   1   2   3   4   5   8   9 
##  31 102  63  24   5   1   1
```

```r
table(b$mhealth)
```

```
## 
##   1   2   3   4   5   8   9 
##  26 132  47  16   2   3   1
```

```r
#+ load records
load("/Users/sdaza/Dropbox/Projects/re-entry/10 investigadores/sdaza/data/records/register.Rdata")

r <- copy(dat); remove(dat)
setnames(r, names(r), tolower(names(r)))

# select variables
setnames(r, c("fecha ingreso", "encuestadora final"), c("ad", "int"))
names(r)
```

```
##  [1] "encuestadora"                                                                                                                                                     
##  [2] "int"                                                                                                                                                              
##  [3] "orden egreso"                                                                                                                                                     
##  [4] "id"                                                                                                                                                               
##  [5] "edad"                                                                                                                                                             
##  [6] "unidad penitenciaria"                                                                                                                                             
##  [7] "ad"                                                                                                                                                               
##  [8] "delito"                                                                                                                                                           
##  [9] "sección"                                                                                                                                                          
## [10] "beneficios intrap"                                                                                                                                                
## [11] "oegreso"                                                                                                                                                          
## [12] "egreso"                                                                                                                                                           
## [13] "mes egreso"                                                                                                                                                       
## [14] "motivo egreso"                                                                                                                                                    
## [15] "x__1"                                                                                                                                                             
## [16] "convocatoria contacto\r\n\r\nsí: si logró contacto\r\nno: si no se logró tener primer contacto"                                                                   
## [17] "nº de intentos"                                                                                                                                                   
## [18] "convocatoria participa\r\n\r\nsí: participó en convocatoria \r\nno: no participó en convocatoria"                                                                 
## [19] "por qué no\r\n\r\npor qué no se logró nunca un contacto directo.\r\nsi la mujer es convocada y declara no querer hacer la encuesta, eso se registra en línea base"
## [20] "nº de intentos\r\n(relativos a la encuesta. por ejemplo, si fijaron una fecha y no llegó, y hubo que fijar una nueva)"                                            
## [21] "dc1"                                                                                                                                                              
## [22] "c1"                                                                                                                                                               
## [23] "por qué no\r\n\r\nno se responde si respondió sí a la columna anterior"                                                                                           
## [24] "observaciones encuestador"                                                                                                                                        
## [25] "comentarios"                                                                                                                                                      
## [26] "nº intentos contacto"                                                                                                                                             
## [27] "logra contacto"                                                                                                                                                   
## [28] "por qué no logra contacto"                                                                                                                                        
## [29] "nº intentos encuentro"                                                                                                                                            
## [30] "dc2"                                                                                                                                                              
## [31] "c2"                                                                                                                                                               
## [32] "por qué no"                                                                                                                                                       
## [33] "observaciones encuestadora"                                                                                                                                       
## [34] "comentarios__1"                                                                                                                                                   
## [35] "se logra contacto 2 semanas fuera"                                                                                                                                
## [36] "se logra contacto 1 mes fuera"                                                                                                                                    
## [37] "se logra contacto mes y medio fuera"                                                                                                                              
## [38] "observaciones"                                                                                                                                                    
## [39] "nº intentos contacto__1"                                                                                                                                          
## [40] "logra contacto__1"                                                                                                                                                
## [41] "por qué no logra contacto__1"                                                                                                                                     
## [42] "nº intentos encuentro__1"                                                                                                                                         
## [43] "dc3"                                                                                                                                                              
## [44] "c3"                                                                                                                                                               
## [45] "por qué no__1"                                                                                                                                                    
## [46] "observaciones encuestadora__1"                                                                                                                                    
## [47] "x__2"                                                                                                                                                             
## [48] "se logra contacto 3 meses fuera"                                                                                                                                  
## [49] "se logra contacto 4 meses fuera"                                                                                                                                  
## [50] "se logra contacto 5 meses fuera"                                                                                                                                  
## [51] "se logra contacto 5 meses y medio fuera"                                                                                                                          
## [52] "observaciones__1"                                                                                                                                                 
## [53] "nº intentos contacto__2"                                                                                                                                          
## [54] "logra contacto__2"                                                                                                                                                
## [55] "por qué no logra contacto__2"                                                                                                                                     
## [56] "nº intentos encuentro__2"                                                                                                                                         
## [57] "dc4"                                                                                                                                                              
## [58] "c4"                                                                                                                                                               
## [59] "por qué no__2"                                                                                                                                                    
## [60] "observaciones encuestadora__2"                                                                                                                                    
## [61] "comentarios__2"                                                                                                                                                   
## [62] "se logra contacto 7 meses fuera"                                                                                                                                  
## [63] "se logra contacto 8 meses fuera"                                                                                                                                  
## [64] "se logra contacto 9 meses fuera"                                                                                                                                  
## [65] "se logra contacto 10 meses fuera"                                                                                                                                 
## [66] "se logra contacto 11 meses fuera"                                                                                                                                 
## [67] "se logra contacto 11 y medio meses fuera"                                                                                                                         
## [68] "observaciones contacto permanente"                                                                                                                                
## [69] "nº intentos contacto__3"                                                                                                                                          
## [70] "logra contacto__3"                                                                                                                                                
## [71] "por qué no logra contacto__3"                                                                                                                                     
## [72] "nº intentos encuentro__3"                                                                                                                                         
## [73] "fecha entrevista__3"                                                                                                                                              
## [74] "participa__3"                                                                                                                                                     
## [75] "por qué no__3"                                                                                                                                                    
## [76] "observaciones encuestadoras"                                                                                                                                      
## [77] "comentarios__3"                                                                                                                                                   
## [78] "start"                                                                                                                                                            
## [79] "month"                                                                                                                                                            
## [80] "year"                                                                                                                                                             
## [81] "date"                                                                                                                                                             
## [82] "ndc1"                                                                                                                                                             
## [83] "today"                                                                                                                                                            
## [84] "ndc2"                                                                                                                                                             
## [85] "week"                                                                                                                                                             
## [86] "ndc3"                                                                                                                                                             
## [87] "twomonths"                                                                                                                                                        
## [88] "ndc4"                                                                                                                                                             
## [89] "sixmonths"                                                                                                                                                        
## [90] "c4_s"                                                                                                                                                             
## [91] "c5_s"
```

```r
r[, admission := cleanDates(ad)]
svars <- c("id","int","admission","c1","c2","c3","c4","ndc1","ndc2","ndc3","ndc4","start","week","twomonths","sixmonths")

r <- r[, ..svars]

# create interviewer id variable
r[, id_int := .GRP, int]
table(r$id_int, useNA = "ifany")
```

```
## 
##  1  2  3  4  5 
## 33 59 58 58 17
```

```r
# number of cases
nrow(r)
```

```
## [1] 225
```

```r
nrow(b) # 227 ?
```

```
## [1] 227
```

```r
# why?
b[!b$id %in% r$id]$id # two ids not in the record file!
```

```
## [1] 40280 10011
## attr(,"label")
## [1] "folio_2"
## attr(,"format.stata")
## [1] "%12.0g"
```

Why we have those missing ids?





# Appendix

