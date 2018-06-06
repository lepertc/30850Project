library(ggplot2)

forwardStop = function(p) {
  return(log(1/(1 - p)))
}

seqStep = function(p, C) {
  if(p < 1 - 1/C) {
    return(0)
  } else {
    return(C)
  }
}

hingeExp = function(p, C) {
  if(p < 1 - 1/C) {
    return(0)
  } else {
    return(1/(C*(1 - p)))
  }
}


p.s = seq(0, 1, 0.0001)

forward.stop = forwardStop(p.s)

forward.stop = as.data.frame(t(rbind(p.s, forward.stop)))

seq.step = c()

for(i in 1:length(p.s)) {
  seq.step[i] = seqStep(p.s[i], 2)
}

seq.step = as.data.frame(t(rbind(p.s, seq.step)))


hinge.exp = c()

for(i in 1:length(p.s)) {
  hinge.exp[i] = hingeExp(p.s[i], 2)
}

hinge.exp = as.data.frame(t(rbind(p.s, hinge.exp)))

forward.stop$type = "Forward Stop"
hinge.exp$type = "Hinge Exp"
seq.step$type = "Seq Step"

names(forward.stop)[2] = "Addition"
names(hinge.exp)[2] = "Addition"
names(seq.step)[2] = "Addition"

df = rbind(forward.stop, hinge.exp, seq.step)

ggplot(df, aes(x = p.s, y = Addition)) + geom_line() + facet_wrap(~ type, scales = "free") + 
  theme_bw() + labs(x = "p-value") + scale_y_continuous(limits = c(0, 10))
