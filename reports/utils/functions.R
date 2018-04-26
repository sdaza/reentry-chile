var.wt = function(x.wt) {
   # computes weighted mean and variance (ML-estimators!)
   # x.wt ... matrix with two columns: cbind(x, wt)

   x.wt = na.omit(x.wt)
   x = x.wt[, 1]
   wt = x.wt[, 2]
   m = x %*% wt / sum(wt)
   v = x^2 %*% wt / sum(wt) - m^2
   res =  c(mean.wt = m, var.wt = v)
   return(res)
}



rbm.logit <- function(lps, grp, wt = rep(1, length(lps))) 
{
   # Rubin Benchmark for propensity score logits (Rubin 2001)
   # lps  ... logit of PS
   # grp  ... grouping variable (treatment/assignment) - factor
   # wt   ... weights (typically PS- or PS-strata weights)

   g <- length(table(grp))
   stat <- unlist(by(cbind(lps, wt), grp, var.wt))  # weighted mean and variance (vector)
   m <- stat[seq(1, 2*g, by = 2)]
   v <- stat[seq(2, 2*g, by = 2)]
   B <- (m[2] - m[1]) / sqrt(sum(v) / 2)   # standardized difference in means
   R <-  v[2] / v[1]                       # variance ratio
   res <- c(B, R)
   names(res) <- c('B', 'R')
   return(res)
}

rbm = function(covar, grp, 
                    lps = rep(1, length(grp)), 
                    wt = rep(1, length(grp)), plt = TRUE) 
{
   # plot of descriptive metrics (Rubin 2001)
   # covar ... data.frame of covariates
   # lps   ... PS-logit
   # grp   ... grouping variable (treatment/assignment) - factor
   # wt    ... weights (typically PS- or PS-strata weights)

   rbm.mat <- apply(covar, 2, rbm.logit, grp, wt)
   B <- rbm.mat[1, ]
   R <-  rbm.mat[2, ]
   rbm.vec <- rbm.logit(lps, grp, wt)
   res <- round(cbind(B, R), 3)
   dimnames(res) <- list(names(covar), c('B', 'R'))
   res
}
