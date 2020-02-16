#根据理论，修正灰色预测的海水表面温度
#假设，温度的增量满足线性关系；依据是月碳排放量的增量基本满足线性关系
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
#预载
library(vars)
library(plm)
library(openxlsx)
#修正一些系数
setwd("E:\\meisai\\Code\\")
data <- read.xlsx("Edit.xlsx", sheet = 1)
KVAR <- data$KVAR[1]
K36 <- data$K36[1]
K45 <- data$K45[1]
K85 <- data$K85[1]
K <- c(KVAR,K36,K45,K85)
Dat <- c(data$VAR,data$R36,data$R45,data$R85)
dim(Dat) <- c(50,4)
Raw <- data$Grey
raw_temp_map <- array(dim=c(200,100,600))
load("modeData.Rdata")
raw_temp_map <- temp_map
result_map <- array(dim = c(4,200,100,600))

for(k in 1:4)
{
    new_temp_map <- array(dim=c(200,100,600))
    for(i in lomin:lomax)
    {
        for(j in lamin:lamax)
        {
            if(is.na(raw_temp_map[i,j,1]))
            {
                next;
            }
            for(year in 1:50)
            {
                avt <- Dat[year,k] - Raw[year]
                kyear <- K[k]
                for(u in 1:12)
                {
                    indmon <- (year-1)*12 + u
                    new_temp_map[i,j,indmon] <- raw_temp_map[i,j,indmon] + 
                    avt + kyear*(u-6.5)/12
                }
            }
        }
    }
    result_map[k,,,] <- new_temp_map
}
save(result_map,file = "modeNewData.Rdata")
# j <- 0
# y <- c()
# tot <- 0
# for(i in 1:600)
# {
#     j <- j + 1
#     tot <- tot + temp_map[177,16,i]
#     if(j == 12)
#     {
#         y <- c(y,tot/12)
#         tot <- 0
#         j <- 0
#     }
# }
# dim(y) <- c(50,1)
# write.xlsx(y,"resultGrey.xlsx")