setwd("~/UChicago/S18/Multiple Testing/Project")
set.seed(06061995)

library(dplyr)
library(plyr)
library(lubridate)

names(daily.end.f)
names(daily.f)
names(daily.start.f)


# 5% subsample dfs
daily.f.5 = filter(daily.f, Start.date %in% days[s5])
daily.f.5.lag = filter(daily.f, Start.date %in% (days[(s5-1)]))
daily.f.5.lag2 = filter(daily.f, Start.date %in% (days[(s5-2)]))
daily.f.5.lag3 = filter(daily.f, Start.date %in% (days[(s5-3)]))
daily.f.5.lag4 = filter(daily.f, Start.date %in% (days[(s5-4)]))
daily.f.5.lag5 = filter(daily.f, Start.date %in% (days[(s5-5)]))

# model seasonality - month, dow, year (num), year^2 - all significane
year = as.factor(year(daily.f.5$Start.date))

# base model
season.mod5.1 = glm(daily.f.5$Freq~daily.f.5$Start.date + daily.f.5$dow +
                  daily.f.5$month+year,
                  family=quasipoisson)

# year month interaction
season.mod5.2 = glm(daily.f.5$Freq~I(1:nrow(daily.f.5))+
                    daily.f.5$dow +
                    daily.f.5$month*year,
                  family=quasipoisson)

# squared
season.mod5.3 = glm(daily.f.5$Freq~I(1:nrow(daily.f.5))+I((1:nrow(daily.f.5))^2)+
                      I((1:nrow(daily.f.5))^3) +
                      daily.f.5$dow +
                      daily.f.5$month*year,
                    family=quasipoisson)

# season mod 4, just lags
season.mod5.4 = glm(daily.f.5$Freq~daily.f.5.lag$Freq+daily.f.5.lag2$Freq+
                      daily.f.5.lag3$Freq,
                   family="quasipoisson")

# add dow
season.mod5.5 = glm(daily.f.5$Freq~daily.f.5.lag$Freq+daily.f.5.lag2$Freq+
                      daily.f.5.lag3$Freq+daily.f.5$dow,
                    family="quasipoisson")

# add overall trend
season.mod5.6 = glm(daily.f.5$Freq~daily.f.5.lag$Freq+daily.f.5.lag2$Freq+
                      daily.f.5.lag3$Freq+daily.f.5$dow,
                    family="quasipoisson")
# 3,0

plot(resid(season.mod5.3))

plot(resid(season.mod5.4))
plot(resid(season.mod5.5))
plot(resid(season.mod5.6))

daily.f.5 = mutate(daily.f.5,strend = predict(season.mod5.6,type="response"))

plot(daily.f.5$Start.date,daily.f.5$Freq,xlab="Date",ylab="frequency",
                  main="5% train (n=127)")
lines(daily.f.5$Start.date,daily.f.5$strend)


#exploratory analysis of other features
for (i in 8:38){
  plot(daily.f.5[,i],daily.f.5$Freq - daily.f.5$strend,
       main = paste0(names(daily.f.5)[i]))
}

#1,3,8,16 valid weather types




# now LASSO to choose model
library(glmnet)

L5.features = c(8:20,21,23,28,34)
names(daily.f.5)[L5.features]


temp.mat5 = as.matrix(cbind(daily.f.5[,L5.features]))

mod = cv.glmnet(x=temp.mat5,y=daily.f.5$Freq,alpha=1,family="poisson",
                offset=log(daily.f.5$strend))

plot(mod$glmnet.fit)
coefs=coef(mod, s = mod$lambda.min)

select.5 = L5.features[coefs[2:length(coefs)]!=0]
names(daily.f.5)[select.5]


#model on test set
season.mod5 = glm(Freq~F.lag1+F.lag2+lag3+dow,
                    data = daily.f[-s5,],
                    family="quasipoisson")

test5.features = daily.f[-c(s5,1:3),c(2,select.5)]
#test5.features = mutate(test5.features,strend = predict(season.mod5,type="response"))
strend = predict(season.mod5,type="response")

mod.5 = glm(Freq~offset(log(strend))+.,data=test5.features,
           family="quasipoisson")

#CI
m = length(mod.5$coefficients)-1
alf = 1-(.05/m)
CI5 = signif(exp(confint(mod.5,level=alf)),4)

#signals: PRCP, SNOW, TMAX

plot(daily.f[-c(s5,1:3),]$Start.date,
     daily.f[-c(s5,1:3),]$Freq,
     xlab="Date",ylab="frequency",
     main="5% test (full model)")
lines(daily.f[-c(s5,1:3),]$Start.date,
      predict(mod.5,type='response'))

plot(daily.f[-c(s5,1:3),]$Freq,predict(mod.5,type='response'),
     xlim=c(0,20000),
     xlab="observed",ylab="predicted",main="Predicted vs. observed counts")
abline(a=0,b=1)