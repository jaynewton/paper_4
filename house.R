library(data.table)
library(lubridate)

load("F:/我的论文/第八篇/RData/da_house_oecd.RData")
load("F:/我的论文/第八篇/RData/da_house_bis.RData")
da_china_volume <- read.csv("C:/Users/Ding/Desktop/数据/csv格式/WIND/商品房销售额.csv",stringsAsFactors=F)
da_china_area <- read.csv("C:/Users/Ding/Desktop/数据/csv格式/WIND/商品房销售面积.csv",stringsAsFactors=F)
# volume denotes sale volume, area denotes sale area.
load("F:/我的论文/第八篇/RData/da_cpi_index.RData")

#### House data execpt for China
da_house_oecd_length <- da_house_oecd[,.(length_oecd=.N),by=country]
da_house_bis_length <- da_house_bis[,.(length_bis=.N),by=country]

da_house_length <- merge(da_house_oecd_length,da_house_bis_length,by="country",all.x=T)
country_bis <- da_house_length[length_oecd<length_bis,country]

da_house <- da_house_oecd[!country %in% country_bis,]
da_house <- rbind(da_house,da_house_bis[country %in% country_bis,])

da_house <- da_house[country %in% da_house[,.N,by=country][N>60,country] | country=="China",]

save(da_house,file="C:/Users/Ding/Desktop/da_house.RData")

#################################
#### Extend China House Price Index further
# Note: The quality of extend house price index data for China is not that good.
# That's why we discard the code below.
da_china_volume <- as.data.table(da_china_volume)
names(da_china_volume) <- c("date_ymd","volume")
da_china_volume[,date_ymd:=ymd(date_ymd)]
da_china_volume[,ym:=ymd(paste0(year(date_ymd),"-",month(date_ymd),"-1"))]
da_china_volume[,date_ymd:=NULL]

da_china_area <- as.data.table(da_china_area)
names(da_china_area) <- c("date_ymd","area")
da_china_area[,date_ymd:=ymd(date_ymd)]
da_china_area[,ym:=ymd(paste0(year(date_ymd),"-",month(date_ymd),"-1"))]
da_china_area[,date_ymd:=NULL]

da_house_china <- merge(da_china_volume,da_china_area,by="ym")
da_house_china[,house_nominal:=volume/area]

da_house_china <- merge(da_house_china,
                        da_cpi_index[country=="China",.(ym,cpi_index)],
                        by="ym")
da_house_china[,house_real_raw:=house_nominal/cpi_index]
house_price_2010 <- mean(da_house_china[year(ym)==2010,house_real_raw])
da_house_china[,house:=house_real_raw/house_price_2010*100]
da_house_china[,country:="China"]
da_house_china <- da_house_china[,.(country,ym,house)]

#### Renew China House Prcie Data 
da_house <- da_house[country!="China",]
da_house <- rbind(da_house,da_house_china)
da_house <- da_house[order(country,ym),]

da_house <- da_house[country %in% da_house[,.N,by=country][N>60,country],]



