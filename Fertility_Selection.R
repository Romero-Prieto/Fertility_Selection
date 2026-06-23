rm(list = ls())                                                                 #Clearing all generated data if any.
library(stringr)                                                                #To work with strings (i.e., any character, including letters, numbers, and symbols).
library(ggplot2)                                                                #To draw plots.
library(lubridate)                                                              #To work with continuous time data.
library(dplyr)                                                                  #To load the library dplyr.
library(readr)                                                                  #To import/export data from a .csv file.


set.seed(0)                                                                     #To set the seed for random number generation.
pATh                 = "/Users/lshjr3/Documents/FertilitySelection/"            #To define the path.
lISt                 = read.csv(paste0(pATh,"lISt.csv"))                        #To read all survey names.
DHS                  = lISt[["survey"]]                                         #To retain only the name of the survey.


bootstrapping = function(data,structure,R) {                                    #To generate the bootstrap frequency weights. If structure = c("strata", "cluster", "woman"), the function resamples clusters within each strata and women within each cluster.  
  data[["woman"]] = 1:nrow(data)
  W               = 1;
  for (j in 1:(length(structure) - 1)) {
    S               = unique(data[, structure[j:(j + 1)]])
    S               = S[order(S[[structure[j]]], S[[structure[j + 1]]]), ]
    S[["k"]]        = ave(S[[structure[j]]], S[[structure[j]]], FUN = seq_along)
    S[["K"]]        = ave(S[[structure[j]]], S[[structure[j]]], FUN = length)
    S[["A"]]        = cumsum((S[["k"]] == 1)*S[["K"]]) - S[["K"]]
    s               = matrix(runif(nrow(S)*R), nrow = nrow(S), ncol = R)
    Fq              = cbind(S[["k"]], ceiling(S[["K"]]*s)) + S[["A"]]
    for (i in (R + 1):1) {Fq[, i] = as.matrix(as.numeric(table(c(Fq[ ,1], Fq[ ,i]))) - 1, ncol = 1)}
    Fq              = data.frame(cbind(S[, structure[j:(j + 1)]], Fq))
    A               = data.frame(data[, structure[j:(j + 1)]])
    A[["j"]]        = 1:nrow(A) 
    Fq              = full_join(A, Fq, by = structure[j:(j + 1)])
    Fq              = as.matrix(Fq[order(Fq[["j"]]), 5:(R + 4)])
    W               = W*Fq
  }
  return(list(W = W))
}
  
fertility = function(B,DOB,interview,W) {                                       #To calculate age-specific fertility rates (ASFR), total fertility rate (TFR), and bootstrap Confidence Intervals for a time interval of three years.
  T2 = median(interview)                                                        #To define the end of the time interval.
  T1 = T2 %m-% years(3)                                                         #To define the beginning of the time interval.
  a  = B
  b  = B
  for (i in 1:20) {
    a[[i]] = as.matrix(time_length(interval(DOB, B[[i]]), "years"))             #To calculate the age at maternity.
    b[[i]] = as.matrix((B[[i]] >= T1)*((B[[i]] < T2)))                          #To indicate births within the time interval.
  }
  
  n        = 1
  x        = seq(15, 50, by = n)
  exposure = matrix(NA, nrow = length(x) - 1, ncol = ncol(W))
  events   = exposure
  Age      = matrix(NA, nrow = length(x) - 1, ncol = 1)
  for (i in 1:(length(x) - 1)) {
    A             = pmin(pmax(DOB %m+% years(x[i]), T1), interview)
    O             = pmin(pmin(DOB %m+% years(x[i + 1]), T2), interview)
    sEL           = (O > A)
    exposure[i, ] = matrix(pmax(time_length(interval(A[sEL], O[sEL]), "years"),0), nrow = 1) %*% W[sEL, ]
    events[i, ]   = matrix(rowSums((a[sEL, ] >= x[i])*(a[sEL, ] < x[i + 1])*b[sEL, ], na.rm = TRUE), nrow = 1) %*% W[sEL, ]
    Age[i]        = (x[i] + x[i + 1])/2
  }
  
  rates    = events/exposure
  ASFR     = t(apply(rates, 1, function(x) quantile(x, c(0.5000, 0.025, 0.975))))
  TFR      = quantile(colSums(n*rates), c(0.5000, 0.025, 0.975))
  return(list(TFR = TFR, ASFR = ASFR, Age = Age))
}



Table_TFR            = matrix(NA, nrow = 0, ncol = 11)
colnames(Table_TFR)  = c("Survey", "MP_users", "TFR_all", "TFR_all_LB", "TFR_all_UB", "TFR_sel", "TFR_sel_LB", "TFR_sel_UB", "TFR_post", "TFR_post_LB", "TFR_post_UB" )
for (svy in DHS) {
  data                 = read.csv(paste0(pATh,"Data/",svy,".csv"))
  freq                 = bootstrapping(data,c("strata", "cluster", "woman"),250)
  freq                 = freq[["W"]]
  W                    = matrix(data[["W"]], ncol = 1)
  WS                   = matrix(data[["W"]]*data[["mobile"]], ncol = 1)
  WR                   = matrix(data[["WR"]], ncol = 1)
  WR[is.na(WR)]        = 0
  W                    = sweep(freq, 1, W, "*")
  WS                   = sweep(freq, 1, WS, "*")
  WR                   = sweep(freq, 1, WR, "*")
  
  dATeS                = c("interview","DOB")
  for (i in dATeS) {data[[i]] = dmy(as.character(data[[i]]))}
  for (i in 1:20) {data[[paste0("B_",i)]] = dmy(as.character(data[[paste0("B_",i)]]))}
  B                    = data[, startsWith(names(data), "B_")]
  DOB                  = data[["DOB"]]
  interview            = data[["interview"]]
  
  all                  = fertility(B,DOB,interview,W)
  selected             = fertility(B,DOB,interview,WS)
  poststratified       = fertility(B,DOB,interview,WR)
  mobile               = sum(data[["W"]]*data[["mobile"]])/sum(data[["W"]])
  
  Table_TFR            = rbind(Table_TFR, c(svy, mobile, all[["TFR"]], selected[["TFR"]], poststratified[["TFR"]]))
}

write_csv(data.frame(Table_TFR), paste0(pATh,"Table_TFR.csv"))