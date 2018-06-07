

# 10% subsample dfs
daily.f.10 = filter(daily.f, Start.date %in% days[s10])

daily.f.10.lag = filter(daily.f, Start.date %in% (days[(s10-1)]))
daily.f.10.lag2 = filter(daily.f, Start.date %in% (days[(s10-2)]))
daily.f.10.lag3 = filter(daily.f, Start.date %in% (days[(s10-3)]))
daily.f.10.lag4 = filter(daily.f, Start.date %in% (days[(s10-4)]))

# model seasonality - month, dow, year (num), year^2 - all significane
year = as.factor(year(daily.f.10$Start.date))

# squared
season.mod10.3 = glm(daily.f.10$Freq~I(1:nrow(daily.f.10))+I((1:nrow(daily.f.10))^2)+
                      I((1:nrow(daily.f.10))^3) +
                      daily.f.10$dow +
                      daily.f.10$month*year,
                    family=quasipoisson)

# season mod 4, just lags
season.mod10.4 = glm(daily.f.10$Freq~daily.f.10.lag$Freq+daily.f.10.lag2$Freq
                     +daily.f.10.lag3$Freq,
                    family="quasipoisson")

# add dow
season.mod10.5 = glm(daily.f.10$Freq~daily.f.10.lag$Freq+daily.f.10.lag2$Freq+
                       daily.f.10.lag3$Freq+
                      daily.f.10$dow,
                    family="quasipoisson")

# add overall trend
season.mod10.6 = glm(daily.f.10$Freq~daily.f.10.lag$Freq+daily.f.10.lag2$Freq+
                      daily.f.10$dow + I(1:nrow(daily.f.10))+
                      I((1:nrow(daily.f.10))^2)+I((1:nrow(daily.f.10))^3),
                    family="quasipoisson")

plot(resid(season.mod10.3))

plot(resid(season.mod10.4))
plot(resid(season.mod10.5))
plot(resid(season.mod10.6))

#2,3

daily.f.10 = mutate(daily.f.10,strend = predict(season.mod10.6,type="response"))

plot(daily.f.10$Start.date,daily.f.10$Freq,xlab="Date",ylab="frequency",
     main="10% train (n=254)")
lines(daily.f.10$Start.date,daily.f.10$strend)

#exploratory analysis of other features
for (i in 8:38){
  plot(daily.f.10[,i],daily.f.10$Freq - daily.f.10$strend,
       main = paste0(names(daily.f.10)[i]))
}

#1,3,8,13,16 valid weather types




# now LASSO to choose model
library(glmnet)

L10.features = c(8:20,21,23,28,31,34)

temp.mat10 = as.matrix(cbind(daily.f.10[,L10.features]))

mod = cv.glmnet(x=temp.mat10,y=daily.f.10$Freq,alpha=1,family="poisson",
                offset=log(daily.f.10$strend))

plot(mod$glmnet.fit)
coefs=coef(mod, s = mod$lambda.min)

select.10 = L10.features[coefs[2:length(coefs)]!=0]
names(daily.f.10)[select.10]


#model on test set
season.mod10 = glm(Freq~F.lag1+F.lag2+dow + I(1:nrow(daily.f[-s10,]))+
                    I((1:nrow(daily.f[-s10,]))^2)+I((1:nrow(daily.f[-s10,]))^3),
                  data = daily.f[-s10,],
                  family="quasipoisson")

test10.features = daily.f[-c(s10,1:2),c(2,select.10)]
strend = predict(season.mod10,type="response")

mod.10 = glm(Freq~offset(log(strend))+.,data=test10.features,
            family="quasipoisson")

#CI
m = length(mod.10$coefficients)-1
alf = 1-(.05/m)
CI10 = signif(exp(confint(mod.10,level=alf)),6)

# signals 10: AWND, PRCP, SNOW, TMAX, TMIN


plot(daily.f[-c(s10,1:2),]$Start.date,
     daily.f[-c(s10,1:2),]$Freq,
     xlab="Date",ylab="frequency",
     main="10% test (full model)")
lines(daily.f[-c(s10,1:2),]$Start.date,
      predict(mod.10,type='response'))