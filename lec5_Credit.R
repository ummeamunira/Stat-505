############## lec 5 Credit data ############
######## Linglong Kong @ Edmonton Jan 16, 2016 ########

creditData = read.csv("/Volumes/MHD2/Study/2016/teach/stat441/dataset/lec5/Credit.csv")  # read csv file
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

### All subset selection ####

regfit.full=regsubsets(Balance~.,mycreditData)
summary(regfit.full) 
regfit.full=regsubsets(Balance~.,data=mycreditData,nvmax=11)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
aa1=which.max(reg.summary$adjr2)
points(aa1,reg.summary$adjr2[aa1], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
aa2=which.min(reg.summary$cp)
points(aa2,reg.summary$cp[aa2],col="red",cex=2,pch=20)
aa3=which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(aa3,reg.summary$bic[aa3],col="red",cex=2,pch=20)
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,aa1)
coef(regfit.full,aa2)
coef(regfit.full,aa3)

# Forward and Backward Stepwise Selection

regfit.fwd=regsubsets(Balance~.,data=mycreditData,nvmax=11,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Balance~.,data=mycreditData,nvmax=11,method="backward")
summary(regfit.bwd)
coef(regfit.full,aa3)
coef(regfit.fwd,aa3)
coef(regfit.bwd,aa3)

# Choosing Among Models

set.seed(1)
train=sample(c(TRUE,FALSE), nrow(mycreditData),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Balance~.,data=mycreditData[train,],nvmax=11)
test.mat=model.matrix(Balance~.,data=mycreditData[test,])
val.errors=rep(NA,11)
for(i in 1:11){
   coefi=coef(regfit.best,id=i)
   pred=test.mat[,names(coefi)]%*%coefi
   val.errors[i]=mean((mycreditData$Balance[test]-pred)^2)
}
val.errors
bb1=which.min(val.errors)
coef(regfit.best,bb1)
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
  }
regfit.best=regsubsets(Balance~.,data=mycreditData,nvmax=11)
coef(regfit.best,bb1)

par(mfrow=c(2,1))
plot(val.errors,type='b')

k=10
set.seed(1)
folds=sample(1:k,nrow(mycreditData),replace=TRUE)
cv.errors=matrix(NA,k,11, dimnames=list(NULL, paste(1:11)))
for(j in 1:k){
  best.fit=regsubsets(Balance~.,data=mycreditData[folds!=j,],nvmax=11)
  for(i in 1:11){
    pred=predict(best.fit,mycreditData[folds==j,],id=i)
    cv.errors[j,i]=mean( (mycreditData$Balance[folds==j]-pred)^2)
    }
  }
mean.cv.errors=apply(cv.errors,2,mean)
plot(mean.cv.errors,type='b')
reg.best=regsubsets(Balance~.,data=mycreditData, nvmax=11)
bb2=which.min(mean.cv.errors)
coef(reg.best,bb2)


