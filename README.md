# Seasonal-Hybrid-ESD


　Twritters的异常检测算法（Anomaly Detection）做的比较好，Seasonal Hybrid ESD算法是先用STL把序列分解，考察残差项。假定这一项符合正态分布，然后就可以用Generalized ESD提取离群点。

　　目标是检测出时间序列数据集的异常点，如图所示，蓝色线是时间序列数据集，红色是圈是异常点。

 

 

　　R语言实现如下，一些依赖包需要install.packages("")或者手动在cran社区下载(注意依赖包的下载)。本人github下载源码。

　　1 主函数是，包含了主要逻辑，加载数据集，IMSHESD算法检测异常点和画出数据集和异常点。IMSHESD算法是主要功能，下面详细介绍。
复制代码

library(zoo)
path_data="E:/Develop/Rstudio/IMS-H-ESDS/3151.csv"
path_sear="E:/Develop/Rstudio/IMS-H-ESDS/IS-H-ESD.R"

source(path_sear)
data <-read.table(path_data,sep=",",skip=1)
print(data)
test_data=IMSHESD(data)
plot(x=data[[1]], y=data[[2]],xlab="time",ylab="value",col="blue",type="l")
lines(x=test_data[[1]], y=test_data[[2]],col="red",type="p")

复制代码

　　2 IMSHESD算法是主要逻辑如下，通过Fourier转换自动求得时间序列的季节周期peri(必须满足数据集的长度>2*peri+1才可以应用时间序列分析)，按照季节周期对数据集做划分，然后应用anmodetection异常检测算法探测异常。
复制代码

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

复制代码

　　3 Fourier转换自动求得时间序列的季节周期peri。
复制代码

Fourier_trans<-function(data)
{
   install.packages("TSA")
   library(TSA)
   p=periodogram(data[2])
   dd=data.frame(freq=p$freq,spec=p$spec)
   order=dd[order(-dd$spec),]
   top2=head(order,3)
   time=min(1.00/top2$f)
}

复制代码

　　4 异常检测主要逻辑anmodetection函数，需要规定异常点的上限10%，STL分解数据集：周期+趋势+随机噪声=原始时间序列（分解方法有Twitters的Decompose和STL），残差项根据正态分布（方差未知使用学生t分布），提取离散点。假设要检测k个离群点，就对数据重复使用k次ESD检验，如果发现离群点就从数据里剔出去，然后在剩下的数据上重新检测（Generalized ESD）。
复制代码

anmodetection<-function(data,anoms_per=0.10,period=7,alpha=0.05,mode="addi")
{
   num <- length(data[[2]])
   #cat("num",num)
   num_anmo=trunc(anoms_per*length(data[[2]]))
   R_idx=rep(seq(0),length(data[[2]]))
   if(ncol(data)!=2)
     {
       print("The col of data must be two!")
       stop()
     }
   data_decompose <- stl(ts(data[[2L]], frequency =period),"periodic",robust = TRUE)
   seasonal_data <- data_decompose$time.series[,1]
   #cat("seasonal_data",seasonal_data)
   trend_data <- data_decompose$time.series[,2]
   random_decomp <- data_decompose$time.series[,3]
   data<-data.frame(times=data[[1]],count=(data[[2]]-seasonal_data-median(data[[2]])))
   func_med <- match.fun(median)
   func_mad <- match.fun(mad)
   numb_anom=0
   for(n in 1:num_anmo)
  {
     data_norm<-abs(data[[2]]-func_med(data[[2]]))
     data_mad<-func_mad(data[[2]])
     data_res<-data_norm/data_mad
     R_res<-max(data_res)
     max_temp_idx <- which(data_res == R_res)[1]
     R_idx_out=data[[1]][max_temp_idx]
     R_idx[n] <- R_idx_out
     data <- data[-which(data[[1]] == R_idx[n]), ]   
     p <- 1 - alpha/(2*(num-n+1)) 
     t <- qt(p,(num-n-1))
     thres <- t*(num-n) / sqrt((num-n-1+t**2)*(num-n+1))
     if(R_res>thres)
       {
        numb_anom <- n
       }
  }
  if(numb_anom>0)
  {
    R_idx <- R_idx[1:numb_anom]
  } 
  else 
  {
    R_idx = NULL
  }
  return(R_idx)
}

复制代码

 

 　　实验结果：红色圈标出了异常点，异常点是10%的比例。

 

 

　参考

　　Twitters异常检测方法，https://anomaly.io/blog/


  
 
