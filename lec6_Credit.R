############## lec 6 Credit data ############
######## Linglong Kong @ Edmonton Jan 16, 2016 ########

creditData = read.csv("/Volumes/MHD2/Study/2016/teach/stat441/dataset/lec6/Credit.csv")  # read csv file
library(ISLR)
library(leaps)

attach(creditData)
Genderf = as.numeric(Gender =="Female") 
Studentf = as.numeric(Student =="Yes") 
Marriedf = as.numeric(Married =="Yes") 
Ethnicityf1 = as.numeric(Ethnicity == "Asian")
Ethnicityf2 = as.numeric(Ethnicity == "Caucasian")

mycreditData = data.frame(creditData[,-c(1,8,9,10,11)],  Genderf,  Studentf,  Marriedf,  Ethnicityf1,  Ethnicityf2)
detach(creditData)

x=model.matrix(Balance~.,mycreditData)[,-1]
y=mycreditData$Balance

### Ridge Regression ####

library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod,s=50,type="coefficients")[1:12,]

set.seed(1)
train=sample(1:nrow(x), nrow(x)/4*3)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:12,]

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0,lambda = grid)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:12,]

# The Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod,xvar='lambda')
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)

lasso.coef=predict(out,type="coefficients",s=bestlam)[1:12,]
lasso.coef
lasso.coef[lasso.coef!=0]

