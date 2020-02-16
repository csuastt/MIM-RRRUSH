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


#主函数入口
library(ncdf4)
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

#粒子群模拟鱼的迁徙

#可视化作图
# library(rgl)  #用RGL包绘制三维交互式图形 
# x <- (-100:100)
# y <- (-100:100)
# #mapply调用复合函数，byrow从行到列排
# z=matrix(mapply(function(i){mapply(function(v0){return(sqrt(i^2+v0^2))},x)},y),nrow = 201,byrow = T) 
# open3d()
# surface3d(x,y,z,back = "lines",color = terrain.colors(z^2))
# 该函数为锥形，在最低处，z取得最小值。此处，以求解该函数在定义域上的最小值为例，说明粒子群算法的实现过程

#初始化粒子群
#定义各参数
fnum = 200 #鱼的个数
vmax = 0.35 #每3天移动一个经纬度
pbeast = NULL #历史经过的最合适位置
gbeast = NULL #种群经过的最合适位置
w = 0.6 #设置惯性权重，通常取非负数，用于调节解空间的搜索范围，w = 1 时，算法为基本粒子群算法。w = 0 时，失去粒子对自身速度的记忆。
c1 = 2.8 #设置个人加速度常数
c2 = 1.3 #设置社会加速度常数
iters = 6000 #设置最大迭代次数
t0 = 9 #鱼类的理想温度
a = 300 #温度敏感函数系数
b = 0 #温度敏感函数
c = 2

#定义代价函数
#影响因素：当前坐标的温度t1，a、b为常数因子
#未来还可以增加更多的影响因素(比如捕食者、食物季节等)，影响因素必须是x和y的函数
#f(x,y) = a/|t1(x,y)-t0|^c + b
#求当前函数的最大值

#--在给定定义域内，随机生成位置矩阵如下，
set.seed(1)
xMat = matrix(c(x=runif(fnum,lomin_0,lomax_0),y=runif(fnum,lamin_0,lamax_0)),byrow = F,ncol = 2,dimnames = list(NULL,c("x","y")))
#打印之前粒子位置
cat("before\n")
print(xMat)
plot(xMat,xlim = c(170,180),ylim = c(13,17))
#--在给定的最大速度限制的条件下，随机生成速度矩阵
set.seed(1)
vMat = matrix(c(x=runif(fnum,-vmax,vmax),y=runif(fnum,-vmax,vmax)),byrow = F,ncol = 2,dimnames = list(NULL,c("x","y")))

#每个粒子的适应度
#这里由于是求最小值，因此适应函数可以定义为一个增函数，求出对应增函数的最大值
#更新pbest、gbest
#同时更新所有粒子的位置与速度
#pbest记录每个粒子历史的适应度最高位置
#gbest记录种群历史适应度的最高位置
adjusts<-apply(xMat,1,function(v){a/abs(temp_map[ceiling(v[1]),ceiling(v[2]),1] - t0)^c + b})
pbest = xMat
pbest = cbind(pbest,adjusts)
gbest = pbest[which.max(pbest[,3]),]
cnt <- 0
for(k in 1:iters)
{
    #迭代6000次，模拟度过50年(600个月)，相当于每三天更新一次坐标
    time_now <- k %/% 10 + 1
    if(time_now > 600)
    {
        time_now = 600
    }
    #---更新pbest
    #遍历adjusts,如果对应粒子的适应度是历史中最高的，则完成替换
    mapply(function(no,adj){if(adj>pbest[no,3])
    {pbest[no,] <<- c(xMat[no,],adj)}},1:length #<<-是全局赋值的意思
    (adjusts),adjusts)  
    #更新gbest
    if(max(pbest[,3])>gbest[3])
    {
        gbest.add = max(pbest[,3])-gbest[3]
        gbest = pbest[which.max(pbest[,3]),]  
        # print("--更新gbest")
        # print(gbest)
    }      
    
    #画去对应的位置点
    # plot(xMat[,1],xMat[,2],pch=20,col='blue',xlim=c(-100,100),ylim=c(-100,100))
    # points(gbest[1],gbest[2],pch=8,col='red')
    # points(0,0,pch=20,cex=0.5)
    # points(0,0,pch=21,cex=2)
    # dev.off()

    #更新所有粒子的位置与速度
    old.xMat<-xMat
    xMat<-xMat+vMat
    #如果出了边界，就默认没有移动
    for(q in 1:200)
    {
        if(is.na(temp_map[ceiling(xMat[q,1]),ceiling(xMat[q,2]),time_now]))
        {
            xMat[q,] = old.xMat[q,]
        }
    }
    vMat<-w*vMat+c1*runif(1,0,1)*(pbest[,1:2]-old.xMat)+c2*runif(1,0,1)*
    (matrix(rep(gbest[1:2],fnum),ncol=2,byrow=T)-old.xMat)
    #----如果vMat有值超过了边界值，则设定为边界值
    vMat[vMat<(-vmax)]<-(-vmax)
    vMat[vMat>vmax]<-vmax
    #计算更新后种群中所有粒子的代价函数
    adjusts<-apply(xMat,1,function(v){a/abs(temp_map[ceiling(v[1]),ceiling(v[2]),time_now] - t0)^c + b})

    #计数输出散点图
    cnt <- cnt + 1
    if(cnt == 10)
    {
        plot(xMat,xlim = c(170,180),ylim = c(13,17))
        cnt  <- 0

    }
}

#输出最后的例子位置
cat("after\n")
print(xMat)
plot(xMat,xlim = c(170,180),ylim = c(13,17))