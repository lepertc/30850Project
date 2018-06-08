#######################################################################
# assumes all code in setup_and_sample.R, 5percent.R, 10percent.R,    #
# 25percent.R has been run                                            #
#######################################################################

library(glmnet)
library(ggplot2)

###############
# 5% test set #
###############

set.seed(06061995)
# LASSO to choose model

L5.features = c(8:20,21,23,28,34)
names(daily.f.5)[L5.features]

temp.mat5 = as.matrix(cbind(daily.f.5[,L5.features]))

mod = cv.glmnet(x=temp.mat5,y=(daily.f.5$Freq-daily.f.5$strend),alpha=1,family="gaussian",nfolds=10)
lambda <- mod$lambda.min
lambda

coefs=coef(mod, s = mod$lambda.min)

select.5 = L5.features[coefs[2:length(coefs)]!=0]

train5.features = daily.f.5[,c(2,select.5)]
lmod5 <- lm((daily.f.5$Freq-daily.f.5$strend) ~ ., data=train5.features)

plot(residuals(lmod5)~fitted(lmod5), ylab="Residuals", xlab="Fitted values", main="Residuals vs fitted values for linear model on 5% training data")
abline(h=0)
qqnorm(rstandard(lmod5),  ylab="Standardized Residuals", xlab="Normal Scores", main="Q-Q plot for linear model on 5% training data")
qqline(rstandard(lmod5))

#model on test set
season.mod5 = glm(Freq~F.lag1+F.lag2+lag3+dow,
                    data = daily.f[-s5,],
                    family="quasipoisson")

test5.features = daily.f[-c(s5,1:3),c(2,select.5)]
strend = predict(season.mod5,type="response")

# selected model
x <- daily.f[-c(s5,1:3),L5.features]
x <- scale(x,TRUE,FALSE)
y <- test5.features[,1]-strend
n <- length(y)
mod = glmnet(x=x,y=y,family="gaussian",standardize = F,thresh=1e-20)
m = sum(beta!=0)
beta = coef(mod, s=lambda, exact = T,x=x,y=y)[-1]
alpha <- 0.05/m
selectedCI.5 <- fixedLassoInf(x,y,beta,lambda*n,family="gaussian", alpha=0.05/m)
selected5.features <- (beta != 0)

# post-selection model (naive)
mod.selected.5 <- lm(daily.f[-c(s5,1:3),]$Freq-strend~x[,selected5.features])

# data splitting model
mod.5 = lm(Freq-strend~.,data=test5.features)

# p-values from selective inference
pv_selective_inf <- selectedCI.5$pv
names(pv_selective_inf) <- names(selectedCI.5$vars)
bf_selected5 <- pv_selective_inf[pv_selective_inf < alpha]

# naive p-values after selection
pv_naive <- coef(summary(mod.selected.5))[-1,4]
names(pv_naive) <- names(selectedCI.5$vars)

# p-values for data splitting model
pv_splitting <- coef(summary(mod.5))[-1,4]
alpha <- 0.05/length(pv_splitting)
bf_splitting5 <- pv_splitting[pv_splitting < alpha]

vars <- factor(names(pv_naive), levels=names(pv_naive))

pframe_final = data.frame(pval=c(pv_selective_inf,pv_naive),variable=rep(vars,2),type=c(rep(c("Corrected", "Naive"), each=length(pv_selective_inf))))
plot5 <- ggplot(pframe_final, aes(x=variable,y=pval,color=type)) + geom_point(aes(color=type)) + labs(title="P-values on 95% test set", x="variable", y="p-value")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))
plot(plot5)


################
# 10% test set #
################

set.seed(06061995)

# LASSO to choose model

L10.features = c(8:20,21,23,28,31,34)

temp.mat10 = as.matrix(cbind(daily.f.10[,L10.features]))

mod = cv.glmnet(x=temp.mat10,y=(daily.f.10$Freq-daily.f.10$strend),alpha=1,family="gaussian",nfolds=20)
lambda <- mod$lambda.min

coefs=coef(mod, s = mod$lambda.min)

select.10 = L10.features[coefs[2:length(coefs)]!=0]

train10.features = daily.f.10[,c(2,select.10)]
lmod10 <- lm(daily.f.10$Freq-daily.f.10$strend ~ ., data=train10.features)

plot(residuals(lmod10)~fitted(lmod10), ylab="Residuals", xlab="Fitted values", main="Residuals vs fitted values for linear model on 10% training data")
abline(h=0)
qqnorm(rstandard(lmod10),  ylab="Standardized Residuals", xlab="Normal Scores", main="Q-Q plot for linear model on 10% training data")
qqline(rstandard(lmod10))

#model on test set
season.mod10 = glm(Freq~F.lag1+F.lag2+dow + I(1:nrow(daily.f[-s10,]))+
                     I((1:nrow(daily.f[-s10,]))^2)+I((1:nrow(daily.f[-s10,]))^3),
                   data = daily.f[-s10,],
                   family="quasipoisson")

test10.features = daily.f[-c(s10,1:2),c(2,select.10)]
strend = predict(season.mod10,type="response")

# selected model
x <- daily.f[-c(s10,1:2),L10.features]
x <- scale(x,TRUE,FALSE)
y <- test10.features[,1]-strend
n <- length(y)
mod = glmnet(x=x,y=y,family="gaussian",standardize = F,thresh=1e-20)
beta = coef(mod, s=lambda, exact = T,x=x,y=y)[-1]
m = sum(beta!=0)
alpha <- 0.05/m
selectedCI.10 <- fixedLassoInf(x,y,beta,lambda*n,family="gaussian", alpha=0.05/m)
selected10.features <- (beta != 0)

# post-selection model (naive)
mod.selected.10 <- lm(daily.f[-c(s10,1:2),]$Freq-strend~x[,selected10.features])

# data splitting model
mod.10 = lm(Freq-strend~.,data=test10.features)

# p-values from selective inference
pv_selective_inf <- selectedCI.10$pv
names(pv_selective_inf) <- names(selectedCI.10$vars)
bf_selected10 <- pv_selective_inf[pv_selective_inf < alpha]

# naive p-values after selection
pv_naive <- coef(summary(mod.selected.10))[-1,4]
names(pv_naive) <- names(selectedCI.10$vars)

# p-values for data splitting model
pv_splitting <- coef(summary(mod.10))[-1,4]
alpha <- 0.05/length(pv_splitting)
bf_splitting10 <- pv_splitting[pv_splitting < alpha]

vars <- factor(names(pv_naive), levels=names(pv_naive))

pframe_final = data.frame(pval=c(pv_selective_inf,pv_naive),variable=rep(vars,2),type=c(rep(c("Corrected", "Naive"), each=length(pv_selective_inf))))
plot10 <- ggplot(pframe_final, aes(x=variable,y=pval,color=type)) + geom_point(aes(color=type)) + labs(title="P-values on 90% test set", x="variable", y="p-value")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))
plot(plot10)

################
# 25% test set #
################

set.seed(06061995)

# LASSO to choose model

L25.features = c(8:20,21,22,23,28,31,32,34)
temp.mat25 = as.matrix(cbind(daily.f.25[,L25.features]))

mod = cv.glmnet(x=temp.mat25,y=delta,alpha=1,family="gaussian", nfolds=20)
lambda <- mod$lambda.min

coefs=coef(mod, s = mod$lambda.min)
select.25 = L25.features[coefs[2:length(coefs)]!=0]

train25.features = daily.f.25[,c(2,select.25)]
lmod25 <- lm(daily.f.25$Freq-daily.f.25$strend ~ ., data=train25.features)
plot(residuals(lmod25)~fitted(lmod25), ylab="Residuals", xlab="Fitted values", main="Residuals vs fitted values for linear model on 25% training data")
abline(h=0)
qqnorm(rstandard(lmod25),  ylab="Standardized Residuals", xlab="Normal Scores", main="Q-Q plot for linear model on 25% training data")
qqline(rstandard(lmod25))

#model on test set
season.mod25 = glm(Freq~F.lag1+F.lag2+lag3+dow + I(1:nrow(daily.f[-s25,]))+
                     I((1:nrow(daily.f[-s25,]))^2)+
                     I((1:nrow(daily.f[-s25,]))^2),
                   data = daily.f[-s25,],
                   family="quasipoisson")

test25.features = daily.f[-c(s25,1:3),c(2,select.25)]
strend = predict(season.mod25,type="response")

# selected model
x <- daily.f[-c(s25,1:3),L25.features]
x <- scale(x,TRUE,FALSE)
y <- test25.features[,1]-strend
n <- length(y)
mod = glmnet(x=x,y=y,family="gaussian",standardize = F,thresh=1e-20)
beta = coef(mod, s=lambda, exact = T,x=x,y=y)[-1]
m = sum(beta!=0)
alpha <- 0.05/m
selectedCI.25 <- fixedLassoInf(x,y,beta,lambda*n,family="gaussian", alpha=0.05/m)
selected25.features <- (beta != 0)


# post-selection model (naive)
mod.selected.25 <- lm(daily.f[-c(s25,1:3),]$Freq-strend~x[,selected25.features])

# data splitting model
mod.25 = lm(Freq-strend~.,data=test25.features)

# p-values from selective inference
pv_selective_inf <- selectedCI.25$pv
names(pv_selective_inf) <- names(selectedCI.25$vars)
bf_selected25 <- pv_selective_inf[pv_selective_inf < alpha]

# naive p-values after selection
pv_naive <- coef(summary(mod.selected.25))[-1,4]
names(pv_naive) <- names(selectedCI.25$vars)

# p-values for data splitting model
pv_splitting <- coef(summary(mod.25))[-1,4]
alpha <- 0.05/length(pv_splitting)
bf_splitting25 <- pv_splitting[pv_splitting < alpha]

vars <- factor(names(pv_naive), levels=names(pv_naive))

pframe_final = data.frame(pval=c(pv_selective_inf,pv_naive),variable=rep(vars,2),type=c(rep(c("Corrected", "Naive"), each=length(pv_selective_inf))))
plot25 <- ggplot(pframe_final, aes(x=variable,y=pval,color=type)) + geom_point(aes(color=type)) + labs(title="P-values on 75% test set", x="variable", y="p-value")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))
plot(plot25)