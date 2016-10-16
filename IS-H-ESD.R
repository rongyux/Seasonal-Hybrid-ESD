IMSHESD<-function(data,group_peri=60)
{
path_Fourier="E:/Develop/Rstudio/IMS-H-ESDS/Fourier.R"
path_data_group="E:/Develop/Rstudio/IMS-H-ESDS/data_group.R"
path_anmo_detection="E:/Develop/Rstudio/IMS-H-ESDS/anmo_detection.R"
source(path_Fourier)
source(path_data_group)
source(path_anmo_detection)
#data <-data_group(data,mode="median",group_period=60)
peri=Fourier_trans(data)
print(peri)
if(ncol(data)!=2)
  {
     print("The col of data must be two!")
     stop()
  }
if((2*peri+1)<length(data[[2]])){
   data_sep_length=ceiling(2*peri+1)
   print(data_sep_length)
   all_data <- vector(mode="list", length=ceiling(length(data[[1]])/(data_sep_length)))
   for(j in seq(1,length(data[[1]]), by=data_sep_length)){
      start_data <- data[[1]][j]
      end_data <- data[[1]][min(j + data_sep_length, length(data[[1]]))]
      if(j+data_sep_length<length(data[[1]])){
          all_data[[ceiling(j/(data_sep_length))]] <- subset(data, data[[1]] >= start_data & data[[1]] < end_data)
      }else{
         all_data[[ceiling(j/(data_sep_length))]] <- subset(data,data[[1]] >= data[[1]][length(data[[1]])-data_sep_length] & data[[1]] < end_data)
      }
   } 
   res=c()
   for(i in 1:length(all_data)) 
  {
      res_temp=anmodetection(all_data[[i]],anoms_per=0.1,period=peri,alpha=0.05)
      res=c(res,res_temp)
  }
  data_plot=rep(c(0),length(res))
  for(i in 1:length(res))
  {
     data_plot[i]=data[[2]][which(data[[1]]==res[i])]
  }
  anmo_point<-data.frame(res,data_plot)
}else{
    print("This is not a seasonal time series")
    stop()
}
}

