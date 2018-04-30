library(quantmod)
library(lubridate)
library(readr)

#### Still in progress

companylist <- read_csv("~/Documents/30850Project/companylist.csv")
symbols = companylist$Symbol
symbols = symbols[1:100]

for(i in symbols) {
  getSymbols(i, src = "yahoo")
}

xts.to.df = function(x) {
  s = as.data.frame(x, data.frame)
  s$date = index(x)
  print(head(s))
  if(ncol(s) < 6) {
    s = NULL
  } else {
    s = s[, c(6:7)]
  }
  return(s)
}

head(xts.to.df(symbols[4]))

make.financial.df = function(sym) {
  df = xts.to.df(sym[1])
  for(i in 2:length(sym)){
    print(sym[i])
    add = xts.to.df(sym[i])
    df = merge(df, add, by = "date")
  }
  return(df)
}

t = make.financial.df(symbols)

