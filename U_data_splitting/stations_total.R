#8 = # households, 9 = household size

hist(log(station.15$Total),breaks=20,xlab="log(total rides)",
     main="Total rides by station")
hist(log(station.15$Total/station.15$pop),breaks=20,xlab="log(per capita rides)",
     main="Per capita rides by station")
hist(station.15$End/station.15$Total,breaks=20,xlab = "proportion",
     main="Prop. of end rides per station")

#full regression for Total
mod1 = glm(station.15$Total~offset(log(station.15$pop))+.,
           family=quasipoisson,data=station.15[,-c(1:3,5:6,58)])

#feture selection
is.MD = station.15$state == "MD"
is.VA = station.15$state == "VA"
temp.mat = as.matrix(cbind(is.MD,is.VA,station.15[,-c(1:7,58)]))

mod = cv.glmnet(x=temp.mat,y=station.15$Total,alpha=1,family="poisson",
                offset=log(station.15$pop))

plot(mod$glmnet.fit)
coefs=coef(mod, s = mod$lambda.min)

select.15 = c(7, (8:57)[coefs[4:length(coefs)]!=0])
names(station.15)[select.15]

#fit model
test.mod = glm(station.16$Total~offset(log(station.16$pop))+.,
              family=quasipoisson,
              data=station.16[select.15])

#CI
m = length(test.mod$coefficients)-1
alf = 1-(.05/m)
CI15 = signif(exp(confint(test.mod,level=alf)),6)
