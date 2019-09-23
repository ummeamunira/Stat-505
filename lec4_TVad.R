############## lec 4 TV advertising data ############
######## Linglong Kong @ Edmonton Jan 11, 2016 ########

TVadData = read.csv("/Volumes/MHD2/Study/2016/teach/stat441/dataset/lec4/Advertising.csv")  # read csv file

attach(TVadData)
TVadlm = lm(Sales~TV+Radio+Newspaper)
summary(TVadlm)

newdata = data.frame(TVadData[1,])
predict(TVadlm, newdata, interval="c", level=0.95)
predict(TVadlm, newdata, interval="p", level=0.95)

####################### end of code ##########################