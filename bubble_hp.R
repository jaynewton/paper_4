library(data.table)
library(lubridate)
library(mFilter)
library(zoo)

#################################
#### House Bubble
load("F:/我的论文/第八篇/RData/da_house.RData")

da_house[,house_hp:=residuals(hpfilter(log(house),freq=400000)),by=country]
da_house[,bubble_house:=ifelse(house_hp>sd(house_hp),1,0),by=country]
da_bubble_house <- da_house[,.(country,ym,bubble_house)]
da_bubble_house[,bubble_house_adj:=ifelse(rollsum(bubble_house,3,fill=0,align="center")>=2,1,0)]
# Note: A bubble lasts at least 2 quarters.
# adj denotes adjusted.
da_bubble_house_hp <- da_bubble_house[,.(country,ym,bubble_house=bubble_house_adj)]

save(da_bubble_house_hp,file="C:/Users/Ding/Desktop/da_bubble_house_hp.RData")

#################################
#### Stock Bubble
load("F:/我的论文/第八篇/RData/da_stock.RData")

da_stock[,stock_hp:=residuals(hpfilter(log(stock),freq=400000)),by=country]
da_stock[,bubble_stock:=ifelse(stock_hp>sd(stock_hp),1,0),by=country]
da_bubble_stock <- da_stock[,.(country,ym,bubble_stock)]
da_bubble_stock[,bubble_stock_adj:=ifelse(rollsum(bubble_stock,3,fill=0,align="center")>=2,1,0)]
# Note: A bubble lasts at least 2 quarters.
# adj denotes adjusted.
da_bubble_stock_hp <- da_bubble_stock[ym>=ymd("1970-1-1"),.(country,ym,bubble_stock=bubble_stock_adj)]

save(da_bubble_stock_hp,file="C:/Users/Ding/Desktop/da_bubble_stock_hp.RData")






