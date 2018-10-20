library(data.table)
library(lubridate)

#### Calculate the Monthly Return
da_ret <- data.table()

#country_name <- "Australia"
FUN_RET <- function(country_name) { # RET denotes reture.
  file_path <- paste0("C:/Users/Ding/Desktop/数据/csv格式/datastream/daily stock price/",
                      country_name,".csv")
  da_price <- read.csv(file_path,header=T,stringsAsFactors=F)
  da_price <- as.data.table(da_price)
  if (ncol(da_price)-1 >= 5) { # the lower bound of the number of banks in certain country
    names(da_price)[1] <- "date_ymd"
    da_price[,date_ymd:=ymd(date_ymd)]
    
    da_price <- melt(da_price,id.vars="date_ymd",variable.name="bank",
                     value.name="price",variable.factor=F)
    da_price[,price:=as.numeric(price)]
    da_price[price==0,price:=NA] 
    da_price <- na.omit(da_price)
    da_price <- da_price[order(bank,date_ymd),]
    da_price[,`:=`(y=year(date_ymd),m=month(date_ymd))]
    da_price[,ym:=ymd(paste0(y,"-",m,"-01"))]
    da_price <- da_price[,.(price=price[.N]),by=.(bank,ym)]
    
    da_allmonth <- da_price[,.(ym=seq.Date(ym[1],ym[.N],by="month")),by=bank]
    da_price <- merge(da_price,da_allmonth,by=c("bank","ym"),all.y=T)
    da_price[,ret:=c(NA,diff(log(price)))*100,by=bank]
    da_price[,country:=country_name]
    da_ret_a <- na.omit(da_price[,.(country,bank,ym,ret)])
  } else {
    da_ret_a <- NULL
  }
  da_ret <<- rbind(da_ret,da_ret_a)
}

FUN_RET("Australia")
FUN_RET("Austria")
FUN_RET("Brazil")
FUN_RET("Canada")
FUN_RET("Chile")
FUN_RET("China")
FUN_RET("Denmark")
FUN_RET("France")
FUN_RET("Germany")
FUN_RET("Greece")
FUN_RET("India")
FUN_RET("Israel")
FUN_RET("Italy")
FUN_RET("Japan")
FUN_RET("Mexico")
FUN_RET("Norway")
FUN_RET("Poland")
FUN_RET("Russia")
FUN_RET("Slovakia")
FUN_RET("South Africa")
FUN_RET("South Korea")
FUN_RET("Spain")
FUN_RET("Sweden")
FUN_RET("Switzerland")
FUN_RET("Turkey")
FUN_RET("United Kingdom")
FUN_RET("United States_Nasdaq")
FUN_RET("United States_NYSE")

da_ret[country %in% c("United States_Nasdaq","United States_NYSE"),
       country:="United States"]
da_ret_m <- copy(da_ret)

save(da_ret_m,file="C:/Users/Ding/Desktop/da_ret_m.RData")


#### Calculate the Daily Return
da_ret <- data.table()

#country_name <- "Australia"
FUN_RET <- function(country_name) { # RET denotes reture.
  file_path <- paste0("C:/Users/Ding/Desktop/数据/csv格式/datastream/daily stock price/",
                      country_name,".csv")
  da_price <- read.csv(file_path,header=T,stringsAsFactors=F)
  da_price <- as.data.table(da_price)
  if (ncol(da_price)-1 >= 5) { # the lower bound of the number of banks in certain country
    names(da_price)[1] <- "date_ymd"
    da_price[,date_ymd:=ymd(date_ymd)]
    da_price <- melt(da_price, id.vars="date_ymd",
                     variable.name="bank",value.name="price")
    da_price[,price:=as.numeric(price)]
    da_price[price==0,price:=NA] 
    da_price <- na.omit(da_price)
    da_price <- da_price[order(bank,date_ymd),]
    
    da_allday <- da_price[,.(date_ymd=seq.Date(date_ymd[1],date_ymd[.N],by="day")),by=bank]
    da_price <- merge(da_price,da_allday,by=c("bank","date_ymd"),all.y=T)
    da_price[,ret:=c(NA,diff(log(price)))*100,by=bank]
    da_price <- da_price[ret!=0,]
    da_price[,country:=country_name]
    da_ret_a <- na.omit(da_price[,.(country,bank,date_ymd,ret)])
  } else {
    da_ret_a <- NULL
  }
  da_ret <<- rbind(da_ret,da_ret_a)
}

FUN_RET("Australia")
FUN_RET("Austria")
FUN_RET("Brazil")






