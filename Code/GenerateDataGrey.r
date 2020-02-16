#定义预测函数
gm11<-function(x,k)
{
    if(is.na(x[1]))
        return(x)
    n<-length(x)
    x1<-numeric(n);
    for(i in 1:n)   ##一次累加
    {
        x1[i]<-sum(x[1:i]);
    }
    b<-numeric(n)
    m<-n-1
    for(j in 1:m)
    {
        b[j+1]<-(0.5*x1[j+1]+0.5*x1[j])   ##紧邻均值生成
    }
    Yn=t(t(x[2:n]))                   ##构造Yn矩阵
    B<-matrix(1,nrow=n-1,ncol=2)      
    B[,1]<-t(t(-b[2:n]))              ##构造B矩阵
    A<-solve(t(B)%*%B)%*%t(B)%*%Yn;   ##使用最小二乘法求得灰参数a,u
    a<-A[1];
    u<-A[2];
    x2<-numeric(k);
    x2[1]<-x[1];
    for(i in 1:k-1)
    {
        x2[1+i]=(x[1]-u/a)*exp(-a*i)+u/a;
    }
    x2=c(0,x2);
    y=diff(x2);   
    y          ##累减生成，获得预测数据数列
    # print(y)
}

#精确度函数
acc<-function(x1,x2)
{
    n<-length(x1);
    sum1=0;
    for(k in 2:n-1)
    {
        sum1<-sum1+(x1[k]-x1[1]);
    }
    s1<-sum1+0.5*(x1[n]-x1[1]);
    sum2=0;
    for(k in 2:n-1)
    {
        sum2<-sum2+(x2[k]-x2[1]);
    }
    s2<-sum2+0.5*(x2[n]-x2[1]);
    abs1<-abs(s1)
    abs2<-abs(s2)
    abs12<-abs(s1-s2)
    ee<-(1+abs1+abs2)/(1+abs1+abs2+abs12)
    print(ee)
}

#参数限定
#经纬度边界
lomin <- 150
lomax <- 178
lamin <- 1
lamax <- 40
#初始经纬度
lomin_0 <- 177
lomax_0 <- 178
lamin_0 <- 16
lamax_0 <- 18
#预测时间长度
futm = 600

#生成未来表面水温的数据
library(ncdf4)

#先预测未来的表面水温
setwd("E:\\meisai\\Code\\")
nc <- nc_open("ERsst.mnmean.nc")
sst <- ncvar_get(nc = nc,varid = "sst")
# 输出预测结果，误差等
# x <- sst[60,50,1:1960]
# x1 <- x
# x2 <- gm11(x,length(x))
# cat("-----------\n")
# acc(x1,x2)
# hist(abs(x1-x2))
# seq  <- c()
# err <- c()
# a  <-  1
# for(i in (x1-x2))
# {
#     if(abs(i) > 1.5)
#     {
#         cat(a)
#         cat(":")
#         cat(i)
#         cat("\n")
#         seq = c(seq,a)
#         err = c(err,i)
#     }
#     a  <-  a+1
# }
# plot(seq,err)
nc_close(nc)
#计算讨论空间内每一点的水温
temp_map <- array(dim=c(200,100,600))
for(i in lomin:lomax)
{
    for(j in lamin:lamax)
    {
        temp <- sst[i,j,]
        temp_fu <- gm11(temp,length(temp) + 600)
        temp_temp = temp_fu[(length(temp) + 1 ): (length(temp) + 600)]
        dim(temp_temp) = c(1,1,600)
        temp_map[i,j,] = temp_temp
    }
}
save(temp_map,file = "modeData.Rdata")