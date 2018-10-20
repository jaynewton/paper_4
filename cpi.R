library(data.table)
library(stringr)
library(lubridate)

#### CPI: Year on Year Changes
da_cpi <- read.csv("C:/Users/Ding/Desktop/数据/csv格式/BIS/CPI.csv",header=F,stringsAsFactors=F)
da_cpi <- as.data.table(da_cpi)
names(da_cpi)[1] <- "date_dmy"
date_ymd <- dmy(da_cpi[-(1:4),date_dmy])
date_ym <- ymd(paste0(year(date_ymd),"-",month(date_ymd),"-01"))

da_cpi <- da_cpi[,str_detect(da_cpi[1,],"Year-on-year changes"),with=F]

names(da_cpi) <- as.character(da_cpi[3,])
da_cpi <- da_cpi[-(1:4),]
da_cpi[,ym:=date_ym]
da_cpi <- melt(da_cpi, id.vars="ym",variable.name="country",
               value.name="cpi_yoy",variable.factor=F)
da_cpi[,cpi_yoy:=as.numeric(cpi_yoy)]
da_cpi <- na.omit(da_cpi)
da_cpi <- da_cpi[order(country,ym),]
da_cpi[,m:=month(ym)]
da_cpi_yoy <- da_cpi[m %in% c(3,6,9,12),]
da_cpi_yoy[,m:=NULL]

save(da_cpi_yoy,file="C:/Users/Ding/Desktop/da_cpi_yoy.RData")

#### CPI Index
da_cpi <- read.csv("C:/Users/Ding/Desktop/数据/csv格式/BIS/CPI.csv",header=F,stringsAsFactors=F)
da_cpi <- as.data.table(da_cpi)
names(da_cpi)[1] <- "date_dmy"
date_ymd <- dmy(da_cpi[-(1:4),date_dmy])
date_ym <- ymd(paste0(year(date_ymd),"-",month(date_ymd),"-01"))

da_cpi <- da_cpi[,str_detect(da_cpi[1,],"Index"),with=F]

names(da_cpi) <- as.character(da_cpi[3,])
da_cpi <- da_cpi[-(1:4),]
da_cpi[,ym:=date_ym]
da_cpi <- melt(da_cpi, id.vars="ym",variable.name="country",
               value.name="cpi_index",variable.factor=F)
da_cpi[,cpi_index:=as.numeric(cpi_index)]
da_cpi <- na.omit(da_cpi[cpi_index!=0,])
da_cpi <- da_cpi[order(country,ym),]
da_cpi[,m:=month(ym)]
da_cpi_index <- da_cpi[m %in% c(3,6,9,12),]
da_cpi_index[,m:=NULL]

save(da_cpi_index,file="C:/Users/Ding/Desktop/da_cpi_index.RData")

