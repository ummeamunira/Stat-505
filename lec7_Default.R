############## lec 7 Credit data ############
######## Linglong Kong @ Edmonton Jan 17, 2016 ########

defaultData = read.table("/Volumes/MHD2/Study/2016/teach/stat441/dataset/lec7/Default.txt",header=T)  


# Logistic Regression

glm.fit=glm(default~balance,data=defaultData,family=binomial)
summary(glm.fit)
predict(glm.fit, list(balance = c(1000,2000)),type="response")

