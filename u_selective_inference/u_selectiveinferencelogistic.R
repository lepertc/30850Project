#######################################################################
# assumes all code in setup_and_sample.R, 5percent.R, 10percent.R,    #
# 25percent.R has been run                                            #
#######################################################################

#create binary variable on 5% set
daily.f.5$binary = ifelse(daily.f.5$strend < daily.f.5$Freq, 1, 0)

#use 5% to get penalty parameter via CV
features5 = c(8:20,21,23,28,34)
temp5 = as.matrix(cbind(daily.f.5[,L5.features]))

dev_mod = cv.glmnet(y = daily.f.5$binary,x = temp5,family = "binomial", alpha=1)
lambda95 = dev_mod$lambda.min #here's the lambda
coef(dev_mod, s = dev_mod$lambda.min)



#now process the test set
s95 = setdiff(6:N,s5) #6 because you want lags to be valid
daily.f.95 = filter(daily.f, Start.date %in% days[s95])
daily.f.95.lag = filter(daily.f, Start.date %in% (days[(s95-1)]))
daily.f.95.lag2 = filter(daily.f, Start.date %in% (days[(s95-2)]))
daily.f.95.lag3 = filter(daily.f, Start.date %in% (days[(s95-3)]))
daily.f.95.lag4 = filter(daily.f, Start.date %in% (days[(s95-4)]))
daily.f.95.lag5 = filter(daily.f, Start.date %in% (days[(s95-5)]))

year95 = as.factor(year(daily.f.95$Start.date))

# add overall trend
season.mod95.6 = glm(daily.f.95$Freq~daily.f.95.lag$Freq+daily.f.95.lag2$Freq+
                       daily.f.95.lag3$Freq+daily.f.95$dow,
                     family="quasipoisson")

daily.f.95 = mutate(daily.f.95,strend = predict(season.mod95.6,type="response"))

plot(daily.f.95$Start.date,daily.f.95$Freq,xlab="Date",ylab="frequency",
     main="95% test (n=2421)")
lines(daily.f.95$Start.date,daily.f.95$strend,col="blue")

#now run selective inference on the whole set
library("selectiveInference")

daily.f.95$binary = ifelse(daily.f.95$strend < daily.f.95$Freq, 1, 0)
features95 = c(8:20,21,23,28,34)
temp95 = as.matrix(cbind(daily.f.95[,features95]))


n = dim(daily.f.95)[1]
temp95 <- scale(temp95,TRUE,FALSE)
mod95 = glmnet(y = daily.f.95$binary,x = temp95,family = "binomial", alpha=1,thresh = 1e-20,standardize=FALSE)
coef(mod95)
beta = coef(mod95, s=lambda95, exact=TRUE, x=temp95,y=daily.f.95$binary)
library("selectiveInference")
inference95 = fixedLassoInf(temp95, daily.f.95$binary , beta, lambda = lambda95*n, family = "binomial",
                            alpha=0.1)



#compared to naive p-values, correlation between currencies (are they really selecting the same thing?)
selected95 = features95[as.numeric(inference95$vars)]
temp95_2 = as.data.frame(cbind(daily.f.95[,selected95]))
temp95_2$bin = daily.f.95$binary
mod95_naive = glm(temp95_2$bin ~ ., family = "binomial",data = temp95_2)

summary(mod95_naive)


z_pval = function(z_score){
  return ( 2 * ( 1 - pnorm( abs( z_score ) ) ) )
}


pvals95 = sapply(inference95$zscore0, FUN = z_pval) #pvals with max precision

plot(as.numeric(pvals95) ~ as.numeric(coef(summary(mod95_naive))[,4][-1]))
abline(0,1)



library(ggplot2)
pframe_final = data.frame(c(as.numeric(inference95$pv),as.numeric(inference95$pv)[-1],
                            as.numeric(coef(summary(mod95_naive))[,4][-1]),
                            as.numeric(coef(summary(mod95_naive))[,4][-1])[-1]),
                          c(selected95,selected95[-1],selected95,selected95[-1]),
                          c(rep("Corrected",length(selected95)),rep("Corrected",length(selected95)-1),
                            rep("Naive",length(selected95)),rep("Naive",length(selected95)-1)),
                          c(rep("All Values",length(selected95)), rep("Zoom",length(selected95)-1),
                            rep("All Values",length(selected95)),rep("Zoom",length(selected95)-1) ))
colnames(pframe_final) = c("pvalue", "index", "Type","Zoom")
pframe_final[2:5,2] = pframe_final[2:5,2] + .2 #manual jitter
pframe_final[8:9,2] = pframe_final[8:9,2] + .2

plot = ggplot(pframe_final, aes(x=index,y=pvalue,color=Type)) + geom_line(aes(linetype=Type))+
  geom_point(aes(color=Type))
plot + facet_wrap(~Zoom,scales="free") + labs(title="P-values selected on 95% test set",
                                              x ="Parameter Index", y = "P-Value") + theme_bw()



####################################################################################
#create binary variable on 10% set
daily.f.10$binary = ifelse(daily.f.10$strend < daily.f.10$Freq, 1, 0)

#use 10% to get penalty parameter via CV
features10 = c(8:20,21,23,28,31,34)
temp10 = as.matrix(cbind(daily.f.10[,features10]))

dev_mod10 = cv.glmnet(y = daily.f.10$binary,x = temp10,family = "binomial", alpha=1)
lambda90 = dev_mod10$lambda.min #here's the lambda
coef(dev_mod10, s = dev_mod10$lambda.min)



#now process the test set
s90 = setdiff(6:N,s10) #6 because you want lags to be valid
daily.f.90 = filter(daily.f, Start.date %in% days[s90])
daily.f.90.lag = filter(daily.f, Start.date %in% (days[(s90-1)]))
daily.f.90.lag2 = filter(daily.f, Start.date %in% (days[(s90-2)]))
daily.f.90.lag3 = filter(daily.f, Start.date %in% (days[(s90-3)]))
daily.f.90.lag4 = filter(daily.f, Start.date %in% (days[(s90-4)]))
daily.f.90.lag5 = filter(daily.f, Start.date %in% (days[(s90-5)]))

year90 = as.factor(year(daily.f.90$Start.date))


# add overall trend (FINAL MODEL USED)
season.mod90.6 = glm(daily.f.90$Freq~daily.f.90.lag$Freq+daily.f.90.lag2$Freq+
                       daily.f.90$dow + I(1:nrow(daily.f.90))+
                       I((1:nrow(daily.f.90))^2)+I((1:nrow(daily.f.90))^3),
                     family="quasipoisson")

daily.f.90 = mutate(daily.f.90,strend = predict(season.mod90.6,type="response"))

plot(daily.f.90$Start.date,daily.f.90$Freq,xlab="Date",ylab="frequency",
     main="90% test")
lines(daily.f.90$Start.date,daily.f.90$strend,col = "blue")



#run selective inference on the whole set
library("selectiveInference")

daily.f.90$binary = ifelse(daily.f.90$strend < daily.f.90$Freq, 1, 0)
features90 = c(8:20,21,23,28,34)
temp90 = as.matrix(cbind(daily.f.90[,features90]))

n = dim(daily.f.90)[1]
temp90 <- scale(temp90,TRUE,FALSE)
mod90 = glmnet(y = daily.f.90$binary,x = temp90,family = "binomial", alpha=1,thresh = 1e-20,standardize=FALSE)
beta = coef(mod90, s=lambda90, exact=TRUE, x=temp90,y=daily.f.90$binary)
inference90 = fixedLassoInf(temp90, daily.f.90$binary , beta, lambda = lambda90*n, family = "binomial",
                            alpha=0.1)

#compared to naive p-values
selected90 = features90[as.numeric(inference90$vars)]
temp90_2 = as.data.frame(cbind(daily.f.90[,selected90]))
temp90_2$bin = daily.f.90$binary
mod90_naive = glm(temp90_2$bin ~ ., family = "binomial",data = temp90_2)
summary(mod90_naive)

#plot comparison
plot(as.numeric(inference90$pv) ~ as.numeric(coef(summary(mod90_naive))[,4][-1]))
abline(0,1)


nonzoom90 = c(1,2,3,8)
pframe_final90 = data.frame(c(as.numeric(inference90$pv),as.numeric(inference90$pv)[-nonzoom90],
                              as.numeric(coef(summary(mod90_naive))[,4][-1]),
                              as.numeric(coef(summary(mod90_naive))[,4][-1])[-nonzoom90]),
                            c(selected90,selected90[-nonzoom90],selected90,selected90[-nonzoom90]),
                            c(rep("Corrected",length(selected90)),rep("Corrected",length(selected90)-4),
                              rep("Naive",length(selected90)),rep("Naive",length(selected90)-4)),
                            c(rep("All Values",length(selected90)), rep("Zoom",length(selected90)-4),
                              rep("All Values",length(selected90)),rep("Zoom",length(selected90)-4) ))
colnames(pframe_final90) = c("pvalue", "index", "Type","Zoom")
pframe_final90[4:9,2] = pframe_final90[4:9,2] + .2 #manual jitter
pframe_final90[10:13,2] = pframe_final90[10:13,2] + .2


#1,2,3,8
plot = ggplot(pframe_final90, aes(x=index,y=pvalue,color=Type)) + geom_line(aes(linetype=Type))+
  geom_point(aes(color=Type))
plot + facet_wrap(~Zoom,scales="free") + labs(title="P-Values selected on 90% test set",
                                              x ="Parameter Index", y = "P-Value") + theme_bw() 
####################################################################################
#create binary variable on 25% set
daily.f.25$binary = ifelse(daily.f.25$strend < daily.f.25$Freq, 1, 0)

#use 25% to get penalty parameter via CV
features25 = c(8:20,21,23,28,31,34)
temp25 = as.matrix(cbind(daily.f.25[,features25]))

dev_mod25 = cv.glmnet(y = daily.f.25$binary,x = temp25,family = "binomial", alpha=1)
lambda75 = dev_mod25$lambda.min #here's the lambda
coef(dev_mod25, s = dev_mod25$lambda.min)


#now process the test set
s75 = setdiff(6:N,s25) #6 because you want lags to be valid
daily.f.75 = filter(daily.f, Start.date %in% days[s75])
daily.f.75.lag = filter(daily.f, Start.date %in% (days[(s75-1)]))
daily.f.75.lag2 = filter(daily.f, Start.date %in% (days[(s75-2)]))
daily.f.75.lag3 = filter(daily.f, Start.date %in% (days[(s75-3)]))
daily.f.75.lag4 = filter(daily.f, Start.date %in% (days[(s75-4)]))
daily.f.75.lag5 = filter(daily.f, Start.date %in% (days[(s75-5)]))

year75 = as.factor(year(daily.f.75$Start.date))

# add overall trend (FINAL MODEL USED)
season.mod75.6 = glm(daily.f.75$Freq~daily.f.75.lag$Freq+daily.f.75.lag2$Freq+
                       daily.f.75.lag$Freq+daily.f.75.lag3$Freq+
                       daily.f.75$dow + I(1:nrow(daily.f.75))+
                       I((1:nrow(daily.f.75))^2)+I((1:nrow(daily.f.75))^3),
                     family="quasipoisson")

daily.f.75 = mutate(daily.f.75,strend = predict(season.mod75.6,type="response"))

plot(daily.f.75$Start.date,daily.f.75$Freq,xlab="Date",ylab="frequency",
     main="75% test")
lines(daily.f.75$Start.date,daily.f.75$strend,col = "blue")

#run selective inference on the whole set
library("selectiveInference")

daily.f.75$binary = ifelse(daily.f.75$strend < daily.f.75$Freq, 1, 0)
features75 = c(8:20,21,23,28,34)
temp75 = as.matrix(cbind(daily.f.75[,features75]))

n = dim(daily.f.75)[1]
temp75 <- scale(temp75,TRUE,FALSE)
mod75 = glmnet(y = daily.f.75$binary,x = temp75,family = "binomial", alpha=1,thresh = 1e-20,standardize=FALSE)
beta = coef(mod75, s=lambda75, exact=TRUE, x=temp75,y=daily.f.75$binary)
inference75 = fixedLassoInf(temp75, daily.f.75$binary , beta, lambda = lambda75*n, family = "binomial",
                            alpha=0.1)

#compared to naive p-values
selected75 = features75[as.numeric(inference75$vars)]
temp75_2 = as.data.frame(cbind(daily.f.75[,selected75]))
temp75_2$bin = daily.f.75$binary
mod75_naive = glm(temp75_2$bin ~ ., family = "binomial",data = temp75_2)
summary(mod75_naive)

#plot comparison
plot(as.numeric(inference75$pv) ~ as.numeric(coef(summary(mod75_naive))[,4][-1]))
abline(0,1)


nonzoom75 = c(1,length(selected75))
pframe_final75 = data.frame(c(as.numeric(inference75$pv),as.numeric(inference75$pv)[-nonzoom75],
                              as.numeric(coef(summary(mod75_naive))[,4][-1]),
                              as.numeric(coef(summary(mod75_naive))[,4][-1])[-nonzoom75]),
                            c(selected75,selected75[-nonzoom75],selected75,selected75[-nonzoom75]),
                            c(rep("Corrected",length(selected75)),rep("Corrected",length(selected75)-2),
                              rep("Naive",length(selected75)),rep("Naive",length(selected75)-2)),
                            c(rep("All Values",length(selected75)), rep("Zoom",length(selected75)-2),
                              rep("All Values",length(selected75)),rep("Zoom",length(selected75)-2) ))
colnames(pframe_final75) = c("pvalue", "index", "Type","Zoom")
pframe_final75[2:8,2] = pframe_final75[2:8,2] + .2 #manual jitter
pframe_final75[13:15,2] = pframe_final75[13:15,2] + .2


plot = ggplot(pframe_final75, aes(x=index,y=pvalue,color=Type)) + geom_line(aes(linetype=Type))+
  geom_point(aes(color=Type))
plot + facet_wrap(~Zoom,scales="free") + labs(title="P-Values selected on 75% test set",
                                              x ="Parameter Index", y = "P-Value") + theme_bw()

