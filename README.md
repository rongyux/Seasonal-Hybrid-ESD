# Seasonal-Hybrid-ESD


　Twritters的异常检测算法（Anomaly Detection）做的比较好，Seasonal Hybrid ESD算法是先用STL把序列分解，考察残差项。假定这一项符合正态分布，然后就可以用Generalized ESD提取离群点。

　　目标是检测出时间序列数据集的异常点，如图所示，蓝色线是时间序列数据集，红色是圈是异常点。

    <img src='C:\Users\rongyu\Pictures捕获.PNG'>

 

　　R语言实现如下，一些依赖包需要install.packages("")或者手动在cran社区下载(注意依赖包的下载)。本人github下载源码。

　　1 主函数是，包含了主要逻辑，加载数据集，IMSHESD算法检测异常点和画出数据集和异常点。IMSHESD算法是主要功能，下面详细介绍。
   append.R

　　2 IMSHESD算法是主要逻辑如下，通过Fourier转换自动求得时间序列的季节周期peri(必须满足数据集的长度>2*peri+1才可以应用时间序列分析)，按照季节周期对数据集做划分，然后应用anmodetection异常检测算法探测异常。
   IMSHESD.R

　　3 Fourier转换自动求得时间序列的季节周期peri。
    Fourier.R

　　4 异常检测主要逻辑anmodetection函数，需要规定异常点的上限10%，STL分解数据集：周期+趋势+随机噪声=原始时间序列（分解方法有Twitters的Decompose和STL），残差项根据正态分布（方差未知使用学生t分布），提取离散点。假设要检测k个离群点，就对数据重复使用k次ESD检验，如果发现离群点就从数据里剔出去，然后在剩下的数据上重新检测（Generalized ESD）。
    anmodetection.R
 

 　　实验结果：红色圈标出了异常点，异常点是10%的比例。

 

 

　参考

　　Twitters异常检测方法，https://anomaly.io/blog/


  
 
