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
