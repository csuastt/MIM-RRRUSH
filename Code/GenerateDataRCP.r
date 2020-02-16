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

#VAR向量自回归模型预测水温
library(ncdf4)
library(vars)
library(plm)
library(openxlsx)
setwd("E:\\meisai\\Code\\")
nc <- nc_open("ERsst.mnmean.nc")
sst <- ncvar_get(nc = nc,varid = "sst")
nc_close(nc)
data <- read.xlsx("RCP3-PD.xlsx", sheet = 1)
CO <- c(data$data3,data$data5)
dim(CO) <- c(164,2)

#计算讨论空间内每一点的水温
# temp_map <- array(dim=c(200,100,600))
# for(i in lomin:lomax)
# {
#     for(j in lamin:lamax)
#     {
#         temp <- sst[i,j,]
#         temp_fu <- gm11(temp,length(temp) + 600)
#         temp_temp = temp_fu[(length(temp) + 1 ): (length(temp) + 600)]
#         dim(temp_temp) = c(1,1,600)
#         temp_map[i,j,] = temp_temp
#     }
# }


j <- 0
y <- c()
tot <- 0
for(i in 1:1973)
{
    j <- j + 1
    tot <- tot + sst[177,16,i]
    if(j == 12)
    {
        y <- c(y,tot/12)
        tot <- 0
        j <- 0
    }
}
dim(y) <- c(164,1)
# write.xlsx(y,"result.xlsx")
# print(VARselect(y, lag.max = 20, type = c("const", "trend", "both", "none"),
# season = NULL, exogen = NULL))
# 最优是阶数为15
y  <- c(CO,y)
dim(y) = c(164,3)
# y.ts  <-  ts(y,start = c(1854,1),frequency = 12)
y <- data.frame(SeaSurfaceTemperature = y[,3],AtmosphericCO2Concentrations = y[,1],AtmosphericN2OConcentrations = y[,2])
mode <- VAR(y,p = 15)
# plot(stability(mode, type = "OLS-CUSUM", h = 0.15, dynamic = FALSE, rescale = TRUE))
# print(causality(mode, cause = c("AtmosphericCO2Concentrations","AtmosphericN2OConcentrations"), vcov.=NULL, boot=FALSE, boot.runs=100))
# plot(irf(mode))
print(fevd(mode,n.head = 15))
result <- predict(mode,n.ahead = 50,ci = 0.95)