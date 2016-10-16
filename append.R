library(zoo)
path_data="E:/Develop/Rstudio/IMS-H-ESDS/3151.csv"
path_sear="E:/Develop/Rstudio/IMS-H-ESDS/IS-H-ESD.R"

source(path_sear)
data <-read.table(path_data,sep=",",skip=1)
print(data)
test_data=IMSHESD(data)
plot(x=data[[1]], y=data[[2]],xlab="time",ylab="value",col="blue",type="l")
lines(x=test_data[[1]], y=test_data[[2]],col="red",type="p")
