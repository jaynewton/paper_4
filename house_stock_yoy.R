#################################
#### Year on Year change of House price 
load("F:/我的论文/第八篇/RData/da_house.RData")
da_house[,house_yoy:=c(rep(NA,4),diff(log(house),lag=4)),by=country]
da_house_yoy <- na.omit(da_house[,.(country,ym,house_yoy)])

save(da_house_yoy,file="C:/Users/Ding/Desktop/da_house_yoy.RData")

#################################
#### Year on Year change of Stock price 
load("F:/我的论文/第八篇/RData/da_stock.RData")
da_stock[,stock_yoy:=c(rep(NA,4),diff(log(stock),lag=4)),by=country]
da_stock_yoy <- na.omit(da_stock[,.(country,ym,stock_yoy)])

save(da_stock_yoy,file="C:/Users/Ding/Desktop/da_stock_yoy.RData")


