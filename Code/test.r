library(rgl)  #用RGL包绘制三维交互式图形 
x <- (-100:100)
y <- (-100:100)
#mapply调用复合函数，byrow从行到列排
z=matrix(mapply(function(i){mapply(function(v0){return(sqrt(i^2+v0^2))},x)},y),nrow = 201,byrow = T) 
open3d()
surface3d(x,y,z,back = "lines",color = terrain.colors(z^2))
#该函数为锥形，在最低处，z取得最小值。此处，以求解该函数在定义域上的最小值为例，说明粒子群算法的实现过程

#1.由于函数z有x和y两个输入变量，因此针对的是二维空间，在给定定义域的x,y属于【-100。100】上随机生成20个粒子，设置粒
#的最大速度为 30
#初始化粒子群（包括20个粒子）
vmax = 30
pbeast = NULL #历史经过的最合适位置
gbeast = NULL #种群经过的最合适位置
gbest.add = NULL 
w = 1 #设置惯性权重，通常取非负数，用于调节解空间的搜索范围，w = 1 时，算法为基本粒子群算法。w = 0 时，失去粒子对自身速度的记忆。
c1 = c2 =2 #设置加速度常数、
iters = 1000 #设置最大迭代次数
alpha = 0.001 #设置最佳适应度值的增量阈值
#--在给定定义域内，随机生成位置矩阵如下
set.seed(1)
xMat = matrix(c(x=runif(20,-100,100),y=runif(20,-100,100)),byrow = F,ncol = 2,dimnames = list(NULL,c("x","y")))
#--在给定的最大速度限制的条件下，随机生成速度矩阵
set.seed(1)
vMat = matrix(c(x=runif(20,-vmax,vmax),y=runif(20,-vmax,vmax)),byrow = F,ncol = 2,dimnames = list(NULL,c("x","y")))

#这里由于是求最小值，因此适应函数可以定义为一个增函数，求出对应增函数的最大值
adjusts = apply(xMat,1,function(v){1/sqrt(sum(v^2)+1)})

#同时更新所有粒子的位置与速度
#pbest记录每个粒子历史的适应度最高位置
#gbest记录种群历史适应度的最高位置
#更新完成后要计算每个粒子的适应度，以进入循环，当达到迭代次数与最佳适应度小于0.0002时，算法结束
pbest = xMat
pbest = cbind(pbest,adjusts)
gbest = pbest[which.max(pbest[,3]),]
for(k in 1:iters)
{
  #---更新pbest
  #遍历adjusts,如果对应粒子的适应度是历史中最高的，则完成替换
mapply(function(no,adj){if(adj>pbest[no,3])
{pbest[no,] <<- c(xMat[no,],adj)}},1:length #<<-是全局赋值的意思
(adjusts),adjusts)  
 print(pbest)
#更新gbest

if(max(pbest[,3])>gbest[3]){
     gbest.add = max(pbest[,3])-gbest[3]
     gbest = pbest[which.max(pbest[,3]),]  
     print("--更新gbest")
     print(gbest)}       
#画去对应的位置点
plot(xMat[,1],xMat[,2],pch=20,col='blue',xlim=c(-100,100),ylim=c(-100,100))
points(gbest[1],gbest[2],pch=8,col='red')
points(0,0,pch=20,cex=0.5)
points(0,0,pch=21,cex=2)
dev.off()
#更新所有粒子的位置与速度
old.xMat<-xMat
xMat<-xMat+vMat
vMat<-w*vMat+c1*runif(1,0,1)*(pbest[,1:2]-old.xMat)+c2*runif(1,0,1)*
  (matrix(rep(gbest[1:2],20),ncol=2,byrow=T)-old.xMat)
#----如果vMat有值超过了边界值，则设定为边界值
vMat[vMat<(-vmax)]<-(-vmax)
vMat[vMat>vmax]<-vmax
#计算更新后种群中所有粒子的适应度
adjusts<-apply(xMat,1,function(v){1/sqrt(sum(v^2)+1)})
#检查全局适应度的增量，如果小于0.0002，则算法停止
if(!is.null(gbest.add) && gbest.add<0.0002){
  print(paste("k=",k,"算法结束！"))
  break;
} 
}