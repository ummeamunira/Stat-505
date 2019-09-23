############## lec 3 TV advertising data ############
######## Linglong Kong @ Edmonton Jan 10, 2016 ########

TVadData = read.csv("/Volumes/MHD2/Study/2016/teach/stat441/dataset/lec3/Advertising.csv")  # read csv file

attach(TVadData)
TVadlm = lm(Sales~TV)
summary(TVadlm)

# pdf("/Volumes/MHD2/Study/2016/teach/stat441/lec/lec3/lec3_2.pdf")

plot(TV,Sales, col='red',pch =16)
abline(TVadlm,lwd=2,col='blue')
fits = predict(TVadlm)
for (i in 1:length(fits)){
	lines(c(TV[i], TV[i]),c(Sales[i], fits[i]),col='grey')
}

# dev.off()

####################### end of code ##########################