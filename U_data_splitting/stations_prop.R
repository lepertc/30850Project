#8 = # households, 9 = household size

col = as.numeric((station.15$Dest>.5))+1
plot(station.15$lng,station.15$lat,col=col,pch=col,xlab="lng",ylab="lat",
     main = "Map of start (black) vs. end (red) stations")

plot(station.15$lat,station.15$Dest,xlab )

#full regression for dif
station.15 = mutate(station.15,Dest = End/Total)

mod1 = glm(cbind(station.15$End,station.15$Start)~.,
           family=quasibinomial,data=station.15[,-c(1:3,5:6,58:59)])

#feture selection
is.MD = station.15$state == "MD"
is.VA = station.15$state == "VA"
temp.mat = as.matrix(cbind(is.MD,is.VA,station.15[,-c(1:7,58,59)]))
temp.y = station.15$Dest>.5

mod = cv.glmnet(x=temp.mat,y=temp.y,alpha=1,family="binomial")

plot(mod$glmnet.fit)
coefs=coef(mod, s = mod$lambda.min)

select.15 = c(7, (8:57)[coefs[4:length(coefs)]!=0])
names(station.15)[select.15]

#fit model
test.mod = glm(cbind(station.16$End,station.16$Start)~.,
               family=quasibinomial,
               data=station.16[select.15])

#CI
m = length(test.mod$coefficients)-1
alf = 1-(.05/m)
CI15 = signif(confint(test.mod,level=alf),4)