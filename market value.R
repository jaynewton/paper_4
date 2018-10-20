library(data.table)
library(lubridate)
library(stringr)

#### Calculate the Monthly Market Value
da_mv <- data.table() # mv denites market value

#country_name <- "Australia"
FUN_MV <- function(country_name) { 
  file_path <- paste0("C:/Users/Ding/Desktop/数据/csv格式/datastream/daily market value/",
                      country_name,".csv")
  da_size <- read.csv(file_path,header=T,stringsAsFactors=F)
  da_size <- as.data.table(da_size)
  if (ncol(da_size)-1 >= 5) { # the lower bound of the number of banks in certain country
    names(da_size)[1] <- "date_ymd"
    da_size[,date_ymd:=ymd(date_ymd)]
    
    da_size <- melt(da_size,id.vars="date_ymd",
                     variable.name="bank",value.name="size")
    da_size[,size:=as.numeric(size)]
    da_size[size==0,size:=NA] 
    da_size <- na.omit(da_size)
    da_size <- da_size[order(bank,date_ymd),]
    da_size[,`:=`(y=year(date_ymd),m=month(date_ymd))]
    da_size[,ym:=ymd(paste0(y,"-",m,"-01"))]
    da_size <- da_size[,.(size=size[.N]),by=.(bank,ym)]
    da_size[,bank:=str_sub(bank,1,-16)]
    
    da_size[,country:=country_name]
    da_mv_a <- na.omit(da_size[,.(country,bank,ym,size)])
  } else {
    da_mv_a <- NULL
  }
  da_mv <<- rbind(da_mv,da_mv_a)
}

FUN_MV("Australia")
FUN_MV("Austria")
FUN_MV("Brazil")
FUN_MV("Canada")
FUN_MV("Chile")
FUN_MV("China")
FUN_MV("Denmark")
FUN_MV("France")
FUN_MV("Germany")
FUN_MV("Greece")
FUN_MV("India")
FUN_MV("Israel")
FUN_MV("Italy")
FUN_MV("Japan")
FUN_MV("Mexico")
FUN_MV("Norway")
FUN_MV("Poland")
FUN_MV("Russia")
FUN_MV("Slovakia")
FUN_MV("South Africa")
FUN_MV("South Korea")
FUN_MV("Spain")
FUN_MV("Sweden")
FUN_MV("Switzerland")
FUN_MV("Turkey")
FUN_MV("United Kingdom")
FUN_MV("United States_Nasdaq")
FUN_MV("United States_NYSE")

da_mv[country %in% c("United States_Nasdaq","United States_NYSE"),
      country:="United States"]
da_mv_m <- copy(da_mv)

save(da_mv_m,file="C:/Users/Ding/Desktop/da_mv_m.RData")

