library(data.table)
library(stringr)
library(lubridate)

da_house <- read.csv("C:/Users/Ding/Desktop/数据/csv格式/BIS/Residential property prices selected series (nominal and real).csv",header=F,stringsAsFactors=F)
da_house <- as.data.table(da_house)
names(da_house)[1] <- "date_dmy"
date_ymd <- dmy(da_house[-(1:4),date_dmy])
date_ym <- ymd(paste0(year(date_ymd),"-",month(date_ymd),"-01"))

da_house <- da_house[,str_detect(da_house[1,],"real"),with=F]
da_house <- da_house[,str_detect(da_house[2,],"Index"),with=F]

names(da_house) <- as.character(da_house[3,])
da_house <- da_house[-(1:4),]
da_house[,ym:=date_ym]
da_house <- melt(da_house, id.vars="ym",variable.name="country",
                 value.name="house",variable.factor=F)
da_house[,house:=as.numeric(house)]
da_house <- na.omit(da_house)
da_house <- da_house[order(country,ym),]
#unique(da_house[,country])
da_house <- da_house[!country %in% c("Emerging markets (aggregate)",
                                     "Advanced economies (aggregate)",
                                     "Euro area"),]
da_house[country=="Czech Republic",country:="Czech"]
da_house[country=="Korea",country:="South Korea"]
da_house[country=="Slovak Republic",country:="Slovakia"]

da_house_bis <- copy(da_house)

save(da_house_bis,file="C:/Users/Ding/Desktop/da_house_bis.RData")



