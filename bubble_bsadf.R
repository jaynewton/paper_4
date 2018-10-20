library(data.table)
library(lubridate)
library(MultipleBubbles)
library(zoo)

#################################
#### House Bubble
load("F:/我的论文/第八篇/RData/da_house.RData")

#da_house <- da_house[country=="United States"]

FUN_GSADF <- function(da) {
  gsadf_house <- sadf_gsadf(da[,house],adflag=1,mflag=1,IC=2,parallel=T)
  return(c(rep(NA,nrow(da)-length(as.vector(gsadf_house[["badfs"]]))),
          as.vector(gsadf_house[["badfs"]])>-1))
}
now()
da_house[,bubble_house:=FUN_GSADF(.SD),by=country]
now()

da_bubble_house <- na.omit(da_house[,.(country,ym,bubble_house)])
da_bubble_house[,bubble_house_adj:=ifelse(rollsum(bubble_house,3,fill=0,align="center")>=2,1,0)]
# Note: A bubble lasts at least 2 quarters.
# adj denotes adjusted.
da_bubble_house_bs <- da_bubble_house[,.(country,ym,bubble_house=bubble_house_adj)]
# bs denotes BSADF method

save(da_bubble_house_bs,file="C:/Users/Ding/Desktop/da_bubble_house_bs.RData")

#################################
#### Stock Bubble
load("F:/我的论文/第八篇/RData/da_stock.RData")

#da_stock <- da_stock[country=="United States"]

FUN_GSADF <- function(da) {
  gsadf_stock <- sadf_gsadf(da[,stock],adflag=1,mflag=1,IC=2,parallel=T)
  return(c(rep(NA,nrow(da)-length(as.vector(gsadf_stock[["badfs"]]))),
           as.vector(gsadf_stock[["badfs"]])>-1))
}
now()
da_stock[,bubble_stock:=FUN_GSADF(.SD),by=country]
now()

da_bubble_stock <- na.omit(da_stock[,.(country,ym,bubble_stock)])
da_bubble_stock[,bubble_stock_adj:=ifelse(rollsum(bubble_stock,3,fill=0,align="center")>=2,1,0)]
# Note: A bubble lasts at least 2 quarters.
# adj denotes adjusted.
da_bubble_stock_bs <- da_bubble_stock[,.(country,ym,bubble_stock=bubble_stock_adj)]
# bs denotes BSADF method

save(da_bubble_stock_bs,file="C:/Users/Ding/Desktop/da_bubble_stock_bs.RData")

