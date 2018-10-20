library(data.table)
library(stringr)
library(lubridate)

da_house <- read.csv("C:/Users/Ding/Desktop/数据/csv格式/OECD/house price/real house price.csv",header=F,stringsAsFactors=F)[,c(1,6,7)]
da_house <- as.data.table(da_house)
names(da_house) <- c("country","yq","house")
da_house[,`:=`(y=as.numeric(str_sub(yq, 1, 4)),
               m=as.numeric(str_sub(yq, -1, -1))*3)]
da_house[,ym:=ymd(paste0(y,"-",m,"-01"))]
da_house <- da_house[,.(country,ym,house)]
da_house <- da_house[order(country,ym),]

#unique(da_house[,country])

da_house <- da_house[!country %in% c("EA","EA17","OECD"),]
da_house[country=="AUS",country:="Australia"]
da_house[country=="AUT",country:="Austria"]
da_house[country=="BEL",country:="Belgium"]
da_house[country=="BRA",country:="Brazil"]
da_house[country=="CAN",country:="Canada"]
da_house[country=="CHE",country:="Switzerland"]
da_house[country=="CHL",country:="Chile"]
da_house[country=="CHN",country:="China"]
da_house[country=="COL",country:="Colombia"]
da_house[country=="CZE",country:="Czech"]
da_house[country=="DEU",country:="Germany"]
da_house[country=="DNK",country:="Denmark"]
da_house[country=="ESP",country:="Spain"]
da_house[country=="EST",country:="Estonia"]
da_house[country=="FIN",country:="Finland"]
da_house[country=="FRA",country:="France"]
da_house[country=="GBR",country:="United Kingdom"]
da_house[country=="GRC",country:="Greece"]
da_house[country=="HUN",country:="Hungary"]
da_house[country=="IDN",country:="Indonesia"]
da_house[country=="IND",country:="India"]
da_house[country=="IRL",country:="Ireland"]
da_house[country=="ISL",country:="Iceland"]
da_house[country=="ISR",country:="Israel"]
da_house[country=="ITA",country:="Italy"]
da_house[country=="JPN",country:="Japan"]
da_house[country=="KOR",country:="South Korea"]
da_house[country=="LTU",country:="Lithuania"]
da_house[country=="LUX",country:="Luxembourg"]
da_house[country=="LVA",country:="Latvia"]
da_house[country=="MEX",country:="Mexico"]
da_house[country=="NLD",country:="Netherlands"]
da_house[country=="NOR",country:="Norway"]
da_house[country=="NZL",country:="New Zealand"]
da_house[country=="POL",country:="Poland"]
da_house[country=="PRT",country:="Portugal"]
da_house[country=="RUS",country:="Russia"]
da_house[country=="SVK",country:="Slovakia"]
da_house[country=="SVN",country:="Slovenia"]
da_house[country=="SWE",country:="Sweden"]
da_house[country=="TUR",country:="Turkey"]
da_house[country=="USA",country:="United States"]
da_house[country=="ZAF",country:="South Africa"]

da_house_oecd <- copy(da_house)

save(da_house_oecd,file="C:/Users/Ding/Desktop/da_house_oecd.RData")

