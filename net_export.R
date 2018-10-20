library(data.table)
library(stringr)
library(lubridate)

#################################
da_net_export <- data.table()

FUN_COUNTRY <- function(country_name) {
  da_net_export_a <- read.csv(str_c("F:/我的论文/第八篇/数据/csv格式/OECD/export and import/",country_name,".csv"),
                              header=F,stringsAsFactors=F)
  da_net_export_a <- as.data.table(na.omit(da_net_export_a))
  names(da_net_export_a) <- c("ym","export","import")
  da_net_export_a[,net_export:=(export-import)/((export+import)/2)]
  da_net_export_a[,`:=`(export=NULL,import=NULL)]
  da_net_export_a[,ym:=ymd(str_c(str_sub(da_net_export_a[,ym],4,7),"-",str_sub(da_net_export_a[,ym],1,2),"-1"))]
  da_net_export_a[,country:=country_name]
  da_net_export <<- rbind(da_net_export,da_net_export_a)
}

countries_names <- c("Australia","Austria","Belgium","Canada","Chile",
                     "Czech","Denmark","Estonia","Finland","France",
                     "Germany","Greece","Hungary","Iceland","Ireland",
                     "Israel","Italy","Japan","South Korea","Latvia",
                     "Luxembourg","Mexico","Netherlands","New Zealand","Norway",
                     "Poland","Portugal","Slovakia","Slovenia","Spain",
                     "Sweden","Switzerland","Turkey","United Kingdom","United States",
                     "Argentina","Brazil","China","Colombia","Costa Rica",
                     "India","Indonesia","Lithuania","Russia","Saudi Arabia",
                     "South Africa")

for (i in countries_names) {
  FUN_COUNTRY(i)
}

save(da_net_export,file="C:/Users/Ding/Desktop/da_net_export.RData")

