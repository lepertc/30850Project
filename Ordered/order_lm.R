library(data.table)
library(ggplot2)
library(gridExtra)
library(MASS)
load("useThis.RData")

daily.end = daily.end.f[, c("station", "Start.date", "Freq")]
daily.station = merge(daily.start.f, daily.end, by = c("station", "Start.date"))
daily.station$Freq.sqrt = sqrt(daily.station$Freq.x + daily.station$Freq.y)
daily.station$Freq.x = NULL
daily.station$Freq.y = NULL
# Intuition order

# Overall predictions
## Weather covariates in order 
## Financial covariates in order
daily.f = daily.f[, c(1:7, 16:20, 8:15, 21:38)]

# Start prediction
## Weather covariates in order 
## Financial covariates in order
## Census covariates in order
## Weather binaries

daily.station$MD = 1*(daily.station$state == "MD")
daily.station$VA = 1*(daily.station$state == "VA")
daily.station$state = NULL

daily.station$avg.household.size = gsub("-", NA, daily.station$avg.household.size)
daily.station$avg.household.size = as.numeric(daily.station$avg.household.size)

daily.station$mean.travel.time.work = gsub("N", NA, daily.station$mean.travel.time.work)
daily.station$mean.travel.time.work = as.numeric(daily.station$mean.travel.time.work)

daily.station$median.income = gsub("-", NA, daily.station$median.income)
daily.station$median.income = gsub("+", "", daily.station$median.income)
daily.station$median.income = as.numeric(daily.station$median.income)

daily.station$mean.income = gsub("-", NA, daily.station$mean.income)
daily.station$mean.income = gsub("+", "", daily.station$mean.income)
daily.station$mean.income = as.numeric(daily.station$mean.income)

daily.station$percent.families.below.poverty.line = gsub("-", NA, daily.station$percent.families.below.poverty.line)
daily.station$percent.families.below.poverty.line = as.numeric(daily.station$percent.families.below.poverty.line)

test.mag = function(corcor, v1, v2, n.sim = 200) {
  mumu = c(mean(v1), mean(v2))
  varvar = c(var(v1, na.rm = T), var(v2, na.rm = T))
  covcov = corcor * sqrt(varvar[1]) * sqrt(varvar[2])
  sigma = matrix(c(varvar[1], covcov, covcov, varvar[2]), ncol = 2)
  cors = c()
  if(is.na(sum(varvar)) == TRUE) {
    c = list(estimate = NA, p.value = NA)
  } else {
    for(i in 1:n.sim) {
      simsim = mvrnorm(length(v1), mu = mumu, Sigma = sigma)
      cors[i] = cor.test(simsim[, 1], simsim[, 2])$estimate
    }
    tar = abs(cor.test(v1, v2)$estimate)
    p = sum(1*(tar <= cors))/n.sim
    c = list(estimate = as.numeric(tar), p.value = p)
  }
  return(c)
}


# Hypo start column 8

# Data spliting order
# Sample 10 percent of the data in two ways:
# 1. First 10 percent of dates
# 2. Random 10 percent
# Rank by p-value of correlation use that order.

# Ordered p.values Intuition
get.p.value = function(dfs) {
  pvals = list()
  for(k in dfs) {
    for(i in 8:ncol(k)) {
      print(i)
      v1 = unlist(as.list(k[[i]]))
      v2 = k$un.trended
      c = test.mag(0.1, v1, v2)
      print(c)
      add = list(correl = c$estimate, p.value = c$p.value, vari = names(k)[i])
      pvals[[length(pvals) + 1]] = add
    }
  }
  return(rbindlist(pvals))
}

get.p.value.census = function(df) {
  pvals = list()
  for(i in 2:52) {
    print(i)
    v1 = as.list(df[[i]])[[1]]
    v2 = df$Freq.sqrt
    c = test.mag(0.1, v1, v2)
    print(c)
    add = list(correl = c$estimate, p.value = c$p.value, vari = names(df)[i])
    pvals[[length(pvals) + 1]] = add
  }
  return(rbindlist(pvals))
}

de.trend = function(df) {
  #df$Freq.sqrt = sqrt(df$Freq)
  df$month = as.factor(month(df$Start.date))
  df$dow = as.factor(weekdays(df$Start.date))
  m = lm(Freq.sqrt ~ poly(Start.date, 2) + month + dow, df)
  df$fit = fitted(m)
  df$un.trended = df$Freq.sqrt - df$fit
  return(df)
}

daily.station = de.trend(daily.station)

get.ps = function(daily.f, daily.station) {
  census = daily.station[, c(3, 39:89)]
  daily.station = daily.station[, c(1:7, 16:20, 8:15, 21:38)]
  
  census$Freq = census$Freq.sqrt^2
  
  census = aggregate(Freq ~ ., census, FUN = mean)
  #census$Freq = census$Freq/census$number.of.households
  census$Freq.sqrt = sqrt(census$Freq)
  census$Freq = NULL
  
  t1 = get.p.value(list(daily.f, daily.station))
  print("CENSUS")
  t2 = get.p.value.census(census)
  
  tt = rbind(t1, t2)
  print(nrow(tt))
  tt = tt[c(1:12, 31:43, 13:30, 44:113)]
  
  return(tt)
}

t1 = get.ps(daily.f, daily.station)
t1$ref = 1:nrow(t1)

# Ordered by first 5 percent dates p.value
daily.f = daily.f[order(daily.f$Start.date), ]
daily.f.SM = daily.f[c(1:127), ]
daily.f.L = daily.f[c(128:2553), ]

daily.station.f = daily.station[order(daily.station$Start.date), ]
daily.station.f.SM = daily.station.f[c(1:33627), ]
daily.station.f.L = daily.station.f[c(33626:672542), ]

t2order = get.ps(daily.f.SM, daily.station.f.SM)
t2 = get.ps(daily.f.L, daily.station.f.L)
t2$ref = 1:nrow(t2)
t2 = t2[order(t2order$p.value), ]

# Ordered by 5% random sample
sam1 = sample(2553, 127)
daily.SM = daily.f[c(sam1), ]
daily.L = daily.f[!c(sam1), ]

sam2 = sample(672542, 33627)
daily.station.SM = daily.station[c(sam2), ]
daily.station.L = daily.station[!c(sam2), ]

t3order = get.ps(daily.SM, daily.station.SM)
t3 = get.ps(daily.L, daily.station.L)
t3$ref = 1:nrow(t3)
t3 = t3[order(t3order$p.value), ]

# Forward Stop
forward.stop = function(df, alpha) {
  df = df[is.na(df$p.value) == FALSE, ]
  cumu = list()
  it = log(1/(1-df$p.value))
  print(length(it))
  for(i in 1:length(it)) {
    cumu[[i]] = sum(it[1:i])/i
  }
  df$quan = as.numeric(cumu)
  df$signal = df$quan < alpha
  return(df)
}

t1FS = forward.stop(t1, 0.05)

t2FS = forward.stop(t2, 0.05)
#t2FS$row = 1:nrow(t2FS)
#k.thresh = max(t2FS[t2FS$signal == TRUE, ]$row)
#t2FS$signal = t2FS$row <= k.thresh

t3FS = forward.stop(t3, 0.05)

p11 = ggplot(t1FS, aes(x = 1:nrow(t1FS))) + geom_line(aes(y = quan)) + 
  labs(y = "Forward Step", x = NULL, title = "...intuition") +
  geom_point(size = 0.2, color = "blue", aes(y = p.value)) +
  theme_bw() + geom_line(aes(y = 0.05), color = "red") + 
  scale_y_continuous(limits = c(0, 1))

p12 = ggplot(t2FS, aes(x = 1:nrow(t2FS))) + geom_line(aes(y = quan)) + 
  labs(x = NULL, y = NULL, title = "...on first 10% dates") +
  geom_point(size = 0.2, color = "blue", aes(y = p.value)) +
  theme_bw() + geom_line(aes(y = 0.05), color = "red")

p13 = ggplot(t3FS, aes(x = 1:nrow(t3FS))) + geom_line(aes(y = quan)) + 
  labs(x = NULL, y = NULL, title = "...on random 10%") +
  geom_point(size = 0.2, color = "blue", aes(y = p.value)) +
  theme_bw() + geom_line(aes(y = 0.05), color = "red")

grid.arrange(p11, p12, p13, ncol = 3, 
             top = "Forward stop quantity for hypotheses ordered based on...",
             bottom = "Blue - individual p.values; Red - alpha = 0.05 threshold") # 800 X 400

# SeqStep

seq.step = function(df, alpha, C) {
  df = df[is.na(df$p.value) == FALSE, ]
  cumu = list()
  it = C * (df$p.value > 1-1/C)
  print(length(it))
  for(i in 1:length(it)) {
    cumu[[i]] = sum(it[1:i])/i
  }
  df$quan = as.numeric(cumu)
  df$signal = df$quan < alpha
  return(df)
}

t1SS = seq.step(t1, 0.05, 2)

t2SS = seq.step(t2, 0.05, 2)
#t2SS$row = 1:nrow(t2SS)
#k.thresh = max(t2SS[t2SS$signal == TRUE, ]$row)
#t2SS$signal = t2SS$row <= k.thresh

t3SS = seq.step(t3, 0.05, 2)

p21 = ggplot(t1SS, aes(x = 1:nrow(t1SS))) + geom_line(aes(y = quan)) + 
  geom_point(size = 0.2, color = "blue", aes(y = p.value)) + 
  labs(y = "Seq Step", x = NULL) +
  theme_bw() + geom_line(aes(y = 0.05), color = "red")

p22 = ggplot(t2SS, aes(x = 1:nrow(t2SS))) + geom_line(aes( y = quan)) + 
  geom_point(size = 0.2, color = "blue", aes(y = p.value)) + 
  labs(x = NULL, y = NULL) +
  theme_bw() + geom_line(aes(y = 0.05), color = "red")

p23 = ggplot(t3SS, aes(x = 1:nrow(t3SS))) + geom_line(aes(y = quan)) + 
  geom_point(size = 0.2, color = "blue", aes(y = p.value)) + 
  labs(x = NULL, y = NULL) +
  theme_bw() + geom_line(aes(y = 0.05), color = "red")

grid.arrange(p21, p22, p23, ncol = 3, 
             top = "Seq step quantity for hypotheses ordered based on...",
             bottom = "Blue - individual p.values; Red - alpha = 0.05 threshold") # 800 X 400


hinge = function(df, alpha, C) {
  df = df[is.na(df$p.value) == FALSE, ]
  cumu = list()
  it = C * log(1/(C * (1 - df$p.value)))
  print(length(it))
  for(i in 1:length(it)) {
    if(df$p.value[i] < 1 - 1/C) {
      it[i] = 0
    }
    cumu[[i]] = sum(it[1:i])/i
  }
  df$quan = as.numeric(cumu)
  df$signal = df$quan < alpha
  return(df)
}

t1hing = hinge(t1, 0.05, 2)

t2hing = hinge(t2, 0.05, 2)
#t2hing$row = 1:nrow(t2hing)
#k.thresh = max(t2hing[t2hing$signal == TRUE, ]$row)
#t2hing$signal = t2hing$row <= k.thresh

t3hing = hinge(t3, 0.05, 2)

p31 = ggplot(t1hing, aes(x = 1:nrow(t1SS))) + geom_line(aes(y = quan)) + 
  geom_point(size = 0.2, color = "blue", aes(y = p.value)) + 
  labs(y = "Hinge Exp", x = NULL) +
  theme_bw() + geom_line(aes(y = 0.05), color = "red") +
  scale_y_continuous(limits = c(0, 1.5))

p32 = ggplot(t2hing, aes(x = 1:nrow(t2SS))) + geom_line(aes( y = quan)) + 
  geom_point(size = 0.2, color = "blue", aes(y = p.value)) + 
  labs(x = NULL, y = NULL) +
  theme_bw() + geom_line(aes(y = 0.05), color = "red")

p33 = ggplot(t3hing, aes(x = 1:nrow(t3SS))) + geom_line(aes(y = quan)) + 
  geom_point(size = 0.2, color = "blue", aes(y = p.value)) + 
  labs(x = NULL, y = NULL) +
  theme_bw() + geom_line(aes(y = 0.05), color = "red")

grid.arrange(p31, p32, p33, ncol = 3, 
             top = "Hinge Exp quantity for hypotheses ordered based on...",
             bottom = "Blue - individual p.values; Red - alpha = 0.05 threshold") # 800 X 400

grid.arrange(p11, p12, p13, p21, p22, p23, p31, p32, p33, ncol = 3, 
             top = "Hinge Exp quantity for hypotheses ordered based on...",
             bottom = "Blue - individual p.values; Red - alpha = 0.05 threshold")




t1FS$method = "Forward Stop"
t2FS$method = "Forward Stop"
t3FS$method = "Forward Stop"
t1SS$method = "Seq Step"
t2SS$method = "Seq Step"
t3SS$method = "Seq Step"
t1hing$method = "Hinge Exp"
t2hing$method = "Hinge Exp"
t3hing$method = "Hinge Exp"

t1FS$row = 1:nrow(t1FS)
t1SS$row = 1:nrow(t1SS)
t1hing$row = 1:nrow(t1hing)

t11 = rbind(t1FS, t1SS, t1hing)


t2FS$row = 1:nrow(t2FS)
t2SS$row = 1:nrow(t2SS)
t2hing$row = 1:nrow(t2hing)

t22 = rbind(t2FS, t2SS, t2hing)


t3FS$row = 1:nrow(t3FS)
t3SS$row = 1:nrow(t3SS)
t3hing$row = 1:nrow(t3hing)

t33 = rbind(t3FS, t3SS, t3hing, fill = T)

t11$ordering = "Intuition"
t22$ordering = "First 5%"
t33$ordering = "Random 5%"

ttt <- rbind(t11, t22, t33, fill = T)


ttt$ordering = as.factor(ttt$ordering)
levels(ttt$ordering)
ttt$ordering = factor(ttt$ordering, levels = c("Intuition", "First 5%", "Random 5%"))

ggplot(ttt, aes(x = row)) + geom_point(aes(y = p.value), size = 0.5) + 
  geom_line(aes(y = quan, color = method)) + facet_wrap( ~ ordering, ncol=3, scales = "free") +
  theme_bw() + theme(legend.position="bottom") + geom_line(aes(y = 0.05), linetype = 2) +
  #scale_y_continuous(limits = c(0, 3)) + 
  labs(x = NULL, y = NULL, 
       title = "Signals detected for hypotheses ordered by intuition with various methods",
       caption = "Points - individual p.values; Dashed line - alpha = 0.05 threshold")

k = aggregate(row ~ signal + method + ordering, ttt, FUN = max)

# BH
bh = t1[order(t1$p.value), ]
bh$k = 1:nrow(bh)
bh$bh_thresh = 0.05 * bh$k/nrow(bh)

##### 
