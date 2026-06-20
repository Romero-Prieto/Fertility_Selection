rm(list = ls())                                                                 #Clearing all generated data if any.
library(stringr)                                                                #To work with strings (i.e., any character, including letters, numbers, and symbols).
library(ggplot2)                                                                #To draw plots.
library(lubridate)                                                              #To work with continuous time data.
library(dplyr)                                                                  #To load the library dplyr.


pATh                 = "/Users/lshjr3/Documents/FertilitySelection/Data/"       #To define the path.
DHS                  = c("BF81", "GH8C", "MW81")
dATeS                = c("interview","DOB")

bootstrapping = function(data,R) {                                              #To generate the bootstrap frequency weights resampling clusters within each strata and women within each cluster.
  S               = unique(data[, c("strata", "cluster")])
  S               = S[order(S[["strata"]], S[["strata"]]), ]
  S[["k"]]        = ave(S[["strata"]], S[["strata"]], FUN = seq_along)
  S[["K"]]        = ave(S[["strata"]], S[["strata"]], FUN = length)
  S[["A"]]        = cumsum((S[["k"]] == 1)*S[["K"]]) - S[["K"]]
  s               = matrix(runif(nrow(S)*R), nrow = nrow(S), ncol = R)
  Wb              = cbind(S[["k"]], ceiling(S[["K"]]*s)) + S[["A"]]
  for (i in (R + 1):1) {Wb[ ,i] = as.matrix(as.numeric(table(c(Wb[ ,1], Wb[ ,i]))) - 1, ncol = 1)}
  Wb              = data.frame(cbind(S[, c("strata", "cluster")],Wb))
  A               = data.frame(data[, c("strata", "cluster")])
  A[["j"]]        = 1:nrow(A) 
  Wb              = full_join(A, Wb, by = c("strata", "cluster"))
  Wb              = as.matrix(Wb[order(Wb[["j"]]), 5:(R + 4)])
  
  data[["woman"]] = 1:nrow(data)
  S               = unique(data[, c("cluster", "woman")])
  S               = S[order(S[["cluster"]], S[["cluster"]]), ]
  S[["k"]]        = ave(S[["cluster"]], S[["cluster"]], FUN = seq_along)
  S[["K"]]        = ave(S[["cluster"]], S[["cluster"]], FUN = length)
  S[["A"]]        = cumsum((S[["k"]] == 1)*S[["K"]]) - S[["K"]]
  s               = matrix(runif(nrow(S)*R), nrow = nrow(S), ncol = R)
  Ww              = cbind(S[["k"]], ceiling(S[["K"]]*s)) + S[["A"]]
  for (i in (R + 1):1) {Ww[ ,i] = as.matrix(as.numeric(table(c(Ww[ ,1], Ww[ ,i]))) - 1, ncol = 1)}
  Ww              = data.frame(cbind(S[, c("cluster", "woman")],Ww))
  A               = data.frame(data[, c("cluster", "woman")])
  A[["j"]]        = 1:nrow(A) 
  Ww              = full_join(A, Ww, by = c("cluster", "woman"))
  Ww              = as.matrix(Ww[order(Ww[["j"]]), 5:(R + 4)])
  W               = Wb*Ww
  return(list(W = W, Wb = Wb, Ww = Ww))
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
    A             = pmax(DOB %m+% years(x[i]), T1)
    O             = pmin(DOB %m+% years(x[i + 1]), T2)
    exposure[i, ] = matrix(pmax(time_length(interval(A, O), "years"),0), nrow = 1) %*% W
    events[i, ]   = matrix(rowSums((a >= x[i])*(a < x[i + 1])*b, na.rm = TRUE), nrow = 1) %*% W
    Age[i]        = (x[i] + x[i + 1])/2
  }
  
  rates    = events/exposure
  ASFR     = t(apply(rates, 1, function(x) quantile(x, c(0.5000, 0.025, 0.975))))
  TFR      = quantile(colSums(n*rates), c(0.5000, 0.025, 0.975))
  return(list(TFR = TFR, ASFR = ASFR, Age = Age))
}



for (svy in DHS) {
  data                 = read.csv(paste0(pATh,svy,".csv"))
  freq                 = bootstrapping(data,100)
  freq                 = freq[["W"]]
  W                    = matrix(data[["W"]], ncol = 1)
  WS                   = matrix(data[["W"]]*data[["mobile"]], ncol = 1)
  WR                   = matrix(data[["WR"]], ncol = 1)
  WR[is.na(WR)]        = 0
  W                    = sweep(freq, 1, W, "*")
  WS                   = sweep(freq, 1, WS, "*")
  WR                   = sweep(freq, 1, WR, "*")
  
  for (i in dATeS) {data[[i]] = dmy(as.character(data[[i]]))}
  for (i in 1:20) {data[[paste0("B_",i)]] = dmy(as.character(data[[paste0("B_",i)]]))}
  B                    = data[,startsWith(names(data), "B_")]
  DOB                  = data[["DOB"]]
  interview            = data[["interview"]]
  all                  = fertility(B,DOB,interview,W)
  selected             = fertility(B,DOB,interview,WS)
  poststratified       = fertility(B,DOB,interview,WR)
}

