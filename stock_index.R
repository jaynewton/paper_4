library(data.table)
library(stringr)
library(lubridate)

#################################
load("F:/我的论文/第八篇/RData/da_cpi_index.RData")

da_stock <- data.table()

FUN_STOCK <- function(country_name) {
  file_path <- paste0("C:/Users/Ding/Desktop/数据/csv格式/OECD/quarterly stock price/",country_name,".csv")
  da_stock_a <- read.csv(file_path,header=F,stringsAsFactors=F)
  # a denotes append
  da_stock_a <- as.data.table(da_stock_a)
  names(da_stock_a) <- c("date_my","stock")
  da_stock_a[,`:=`(y=as.numeric(str_sub(date_my,-4,-1)),
                   m=as.numeric(str_sub(date_my,1,2)))]
  da_stock_a[,ym:=ymd(paste0(y,"-",m,"-01"))]
  da_stock_a <- da_stock_a[stock!=0,]
  da_stock_a[,country:=country_name]
  da_stock_a <- da_stock_a[,.(country,ym,stock)]
  da_stock <<- rbind(da_stock,da_stock_a)
}

countries_names <- c("Australia","Austria","Belgium","Brazil","Canada",
                     "Chile","China","Czech","Denmark","Estonia",
                     "Finland","France","Germany","Greece","Hungary",
                     "Iceland","India","Ireland","Israel","Italy",
                     "Japan","Luxembourg","Mexico","Netherlands","New Zealand",
                     "Norway","Poland","Portugal","Russia","Slovakia",
                     "Slovenia","South Africa","South Korea","Spain","Sweden",
                     "Switzerland","Turkey","United Kingdom","United States")

for (i in countries_names) {
  FUN_STOCK(i)
}

da_stock <- merge(da_stock,da_cpi_index,by=c("country","ym"))
da_stock[,stock_real:=stock/cpi_index*100]
da_stock <- da_stock[,.(country,ym,stock=stock_real)]

save(da_stock,file="C:/Users/Ding/Desktop/da_stock.RData")


