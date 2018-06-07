# 25% subsample dfs
daily.f.25 = filter(daily.f, Start.date %in% days[s25])
daily.f.25.lag = filter(daily.f, Start.date %in% (days[(s25-1)]))
daily.f.25.lag2 = filter(daily.f, Start.date %in% (days[(s25-2)]))
daily.f.25.lag3 = filter(daily.f, Start.date %in% (days[(s25-3)]))
daily.f.25.lag4 = filter(daily.f, Start.date %in% (days[(s25-4)]))
daily.f.25.lag5 = filter(daily.f, Start.date %in% (days[(s25-5)]))

# model seasonality - month, dow, year (num), year^2 - all significane
year = as.factor(year(daily.f.25$Start.date))

# base model
season.mod25.1 = glm(daily.f.25$Freq~daily.f.25$Start.date + daily.f.25$dow +
                      daily.f.25$month+year,
                    family=quasipoisson)

# year month interaction
season.mod25.2 = glm(daily.f.25$Freq~I(1:nrow(daily.f.25))+
                      daily.f.25$dow +
                      daily.f.25$month*year,
                    family=quasipoisson)

# squared
season.mod25.3 = glm(daily.f.25$Freq~I(1:nrow(daily.f.25))+I((1:nrow(daily.f.25))^2)+
                      I((1:nrow(daily.f.25))^3) +
                      daily.f.25$dow +
                      daily.f.25$month*year,
                    family=quasipoisson)

# season mod 4, just lags
season.mod25.4 = glm(daily.f.25$Freq~daily.f.25.lag$Freq+daily.f.25.lag2$Freq+
                       daily.f.25.lag3$Freq,
                    family="quasipoisson")

# add dow
season.mod25.5 = glm(daily.f.25$Freq~daily.f.25.lag$Freq+daily.f.25.lag2$Freq+
                      daily.f.25$dow,
                    family="quasipoisson")

# add overall trend
season.mod25.6 = glm(daily.f.25$Freq~daily.f.25.lag$Freq+daily.f.25.lag2$Freq+
                       daily.f.25.lag$Freq+daily.f.25.lag3$Freq+
                       daily.f.25$dow + I(1:nrow(daily.f.25))+
                      I((1:nrow(daily.f.25))^2)+I((1:nrow(daily.f.25))^3),
                    family="quasipoisson")

plot(resid(season.mod25.3))

plot(resid(season.mod25.4))
plot(resid(season.mod25.5))
plot(resid(season.mod25.6))

#3,3

daily.f.25 = mutate(daily.f.25,strend = predict(season.mod25.6,type="response"))

plot(daily.f.25$Start.date,daily.f.25$Freq,xlab="Date",ylab="frequency",
     main="25% train (n=635)")
lines(daily.f.25$Start.date,daily.f.25$strend)


#exploratory analysis of other features
for (i in 8:38){
  plot(daily.f.25[,i],daily.f.25$Freq - daily.f.25$strend,
       main = paste0(names(daily.f.25)[i]))
}

#1,2,3,8,13,14,16 valid weather types




# now LASSO to choose model
library(glmnet)

L25.features = c(8:20,21,22,23,28,31,32,34)
names(daily.f.25)[L25.features]

temp.mat25 = as.matrix(cbind(daily.f.25[,L25.features]))

mod = cv.glmnet(x=temp.mat25,y=daily.f.25$Freq,alpha=1,family="poisson",
                offset=log(daily.f.25$strend))

plot(mod$glmnet.fit)
coefs=coef(mod, s = mod$lambda.min)

select.25 = L25.features[coefs[2:length(coefs)]!=0]
names(daily.f.25)[select.25]


#model on test set
season.mod25 = glm(Freq~F.lag1+F.lag2+lag3+dow + I(1:nrow(daily.f[-s25,]))+
                    I((1:nrow(daily.f[-s25,]))^2)+
                     I((1:nrow(daily.f[-s25,]))^2),
                  data = daily.f[-s25,],
                  family="quasipoisson")

test25.features = daily.f[-c(s25,1:3),c(2,select.25)]
#test25.features = mutate(test25.features,strend = predict(season.mod25,type="response"))
strend = predict(season.mod25,type="response")

mod.25 = glm(Freq~offset(log(strend))+.,data=test25.features,
            family="quasipoisson")

#CI
m = length(mod.25$coefficients)-1
alf = 1-(.05/m)
CI25 = signif(exp(confint(mod.25,level=alf)),4)


plot(daily.f[-c(s25,1:3),]$Start.date,
     daily.f[-c(s25,1:3),]$Freq,
     xlab="Date",ylab="frequency",
     main="25% test (full model)")
lines(daily.f[-c(s25,1:3),]$Start.date,
      predict(mod.25,type='response'))