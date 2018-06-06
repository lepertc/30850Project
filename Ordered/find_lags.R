load("useThis.RData")
library(Hmisc)
library(data.table)
library(ggplot2)

DataSlid1 <- slide(Data1, Var = "A", slideBy = -1)


df = daily.f[, "Freq.sqrt"]

add.lag = function(df, lag, nam) {
  df$lag = Lag(df$Freq.sqrt, shift = lag)
  names(df)[ncol(df)] = nam
  return(df)
}

model = function(df) {
  m = lm(Freq.sqrt ~ ., df)
  c = summary(m)$coefficients
  p = c[nrow(c), 4]
  return(p)
}


run1 = function(df, noms) {
  out = list()
  for(i in 1:length(noms)) {
    df.i = add.lag(df, -i, noms[i])
    add = list(number = noms[i], p.val = model(df.i))
    if(TRUE) {
      df = df.i
    }
    out[[i]] = add
  }
  return(rbindlist(out))
}


df1 = add.lag(df, -1, "one")

t1 = model(df1, "one")

tt1 = run1(df, noms)

run2 = function(df, noms, alpha) { 
  n = length(noms)
  B = seq(n, 1, -1)
  B = B^2
  B = B/sum(B) * alpha
  out = list()
  for(i in 1:n) {
    df.i = add.lag(df, -i, noms[i])
    add = list(number = noms[i], p.val = model(df.i))
    n.disco = ncol(df.i) - 2
    add$threshold = B[i] * max(1, n.disco)
    if(add$p.val <= add$threshold) {
      print(i)
      df = df.i
      add$discovery = TRUE
    } else { 
      add$discovery = FALSE
      }
    out[[i]] = add
  }
  return(list(data = df, res = rbindlist(out)))
}

noms = as.character(seq(1, 500))

tt2 = run2(df, noms, 0.1)

ggplot(tt2$res, aes(x = as.numeric(number), y = p.val)) + geom_point(aes(color = discovery))  + 
  theme_bw() + geom_line(aes(y = threshold)) + scale_y_continuous(limits = c(0, 0.01))


run3 = function(df, noms, alpha) {
  out = list()
  n = length(noms)
  B = seq(n, 1, -1)
  B = B^3
  B = B/sum(B) * alpha
  d = 0
  for(i in 1:n) {
    df.i = add.lag(df, -i, noms[i])
    add = list(number = noms[i], p.val = model(df.i))
    add$threshold = B[i - d]
    if(add$p.val <= add$threshold) {
      d = i
      print(i)
      df = df.i
      add$discovery = TRUE
    } else { 
      add$discovery = FALSE
    }
    out[[i]] = add
  }
  return(list(data = df, res = rbindlist(out)))
}

tt3 = run3(df, noms, 0.1)

ggplot(tt3$res, aes(x = as.numeric(number), y = p.val)) + geom_point(aes(color = discovery))  + 
  theme_bw() + geom_line(aes(y = threshold)) + scale_y_continuous(limits = c(0, 0.01))
