sink(file="C:/Users/Ding/Desktop/主代码.txt",append=T)
sink()
rm(list=ls())

#################################
#### GDP Growth Rate 
load("F:/我的论文/第八篇/RData/da_crisis_hp.RData")
#load("F:/我的论文/第八篇/RData/da_crisis_bs.RData")
load("F:/我的论文/第八篇/RData/da_gdp_yoy.RData")
da_crisis_a <- merge(da_crisis_a,da_gdp_yoy,by=c("country","ym"))
da_crisis_a[,mean(gdp_yoy)]

da_crisis_start <- da_crisis_a[crisis_start==1,ym,by=country]
da_crisis_bubble_house_start <- da_crisis_a[crisis_bubble_house==1,ym,by=country]
da_crisis_bubble_stock_start <- da_crisis_a[crisis_bubble_stock==1,ym,by=country]
da_crisis_bubble_house_credit_start <- da_crisis_a[crisis_bubble_house_credit==1,ym,by=country]
da_crisis_bubble_stock_credit_start <- da_crisis_a[crisis_bubble_stock_credit==1,ym,by=country]

da_crisis_result <- da_crisis_a[,.(country,ym,crisis_start,
                                   crisis_bubble_house,crisis_bubble_stock,
                                   crisis_bubble_house_credit,crisis_bubble_stock_credit)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,gdp:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(gdp_yoy)]]
  }
}

da_crisis_result[crisis_start==1,mean(gdp)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_house_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,gdp_bubble_house:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(gdp_yoy)]]
  }
}

#da_crisis_result[crisis_bubble_house==1,mean(gdp)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_stock_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,gdp_bubble_stock:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(gdp_yoy)]]
  }
}

#da_crisis_result[crisis_bubble_stock==1,mean(gdp)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_house_credit_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,gdp_bubble_house_credit:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(gdp_yoy)]]
  }
}

da_crisis_result[crisis_bubble_house_credit==1,mean(gdp)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_stock_credit_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,gdp_bubble_stock_credit:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(gdp_yoy)]]
  }
}

da_crisis_result[crisis_bubble_stock_credit==1,mean(gdp)]

#################################
#### House Price Growth Rate
load("F:/我的论文/第八篇/RData/da_crisis_hp.RData")
#load("F:/我的论文/第八篇/RData/da_crisis_bs.RData")
load("F:/我的论文/第八篇/RData/da_house_yoy.RData")
da_crisis_a <- merge(da_crisis_a,da_house_yoy,by=c("country","ym"))
da_crisis_a[,mean(house_yoy)]

da_crisis_start <- da_crisis_a[crisis_start==1,ym,by=country]
da_crisis_bubble_house_start <- da_crisis_a[crisis_bubble_house==1,ym,by=country]
da_crisis_bubble_stock_start <- da_crisis_a[crisis_bubble_stock==1,ym,by=country]
da_crisis_bubble_house_credit_start <- da_crisis_a[crisis_bubble_house_credit==1,ym,by=country]
da_crisis_bubble_stock_credit_start <- da_crisis_a[crisis_bubble_stock_credit==1,ym,by=country]

da_crisis_result <- da_crisis_a[,.(country,ym,crisis_start,
                                   crisis_bubble_house,crisis_bubble_stock,
                                   crisis_bubble_house_credit,crisis_bubble_stock_credit)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,house:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(house_yoy)]]
  }
}

da_crisis_result[crisis_start==1,mean(house)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_house_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,house_bubble_house:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(house_yoy)]]
  }
}

#da_crisis_result[crisis_bubble_house==1,mean(house)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_stock_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,house_bubble_stock:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(house_yoy)]]
  }
}

#da_crisis_result[crisis_bubble_stock==1,mean(house)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_house_credit_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,house_bubble_house_credit:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(house_yoy)]]
  }
}

da_crisis_result[crisis_bubble_house_credit==1,mean(house)]


for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_stock_credit_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,house_bubble_stock_credit:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(house_yoy)]]
  }
}

da_crisis_result[crisis_bubble_stock_credit==1,mean(house)]

#################################
#### Stock Price Growth Rate 
load("F:/我的论文/第八篇/RData/da_crisis_hp.RData")
#load("F:/我的论文/第八篇/RData/da_crisis_bs.RData")
load("F:/我的论文/第八篇/RData/da_stock_yoy.RData")
da_crisis_a <- merge(da_crisis_a,da_stock_yoy,by=c("country","ym"))
da_crisis_a[,mean(stock_yoy)]

da_crisis_start <- da_crisis_a[crisis_start==1,ym,by=country]
da_crisis_bubble_house_start <- da_crisis_a[crisis_bubble_house==1,ym,by=country]
da_crisis_bubble_stock_start <- da_crisis_a[crisis_bubble_stock==1,ym,by=country]
da_crisis_bubble_house_credit_start <- da_crisis_a[crisis_bubble_house_credit==1,ym,by=country]
da_crisis_bubble_stock_credit_start <- da_crisis_a[crisis_bubble_stock_credit==1,ym,by=country]

da_crisis_result <- da_crisis_a[,.(country,ym,crisis_start,
                                   crisis_bubble_house,crisis_bubble_stock,
                                   crisis_bubble_house_credit,crisis_bubble_stock_credit)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,stock:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(stock_yoy)]]
  }
}

da_crisis_result[crisis_start==1,mean(stock)]


for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_house_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,stock_bubble_house:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(stock_yoy)]]
  }
}

#da_crisis_result[crisis_bubble_house==1,mean(stock)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_stock_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,stock_bubble_stock:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(stock_yoy)]]
  }
}

#da_crisis_result[crisis_bubble_stock==1,mean(stock)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_house_credit_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,stock_bubble_house_credit:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(stock_yoy)]]
  }
}

da_crisis_result[crisis_bubble_house_credit==1,mean(stock)]


for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_stock_credit_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,stock_bubble_stock_credit:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(stock_yoy)]]
  }
}

da_crisis_result[crisis_bubble_stock_credit==1,mean(stock)]

#################################
#### Household Credit Growth Rate 
load("F:/我的论文/第八篇/RData/da_crisis_hp.RData")
#load("F:/我的论文/第八篇/RData/da_crisis_bs.RData")
load("F:/我的论文/第八篇/RData/da_household_yoy.RData")
da_crisis_a <- merge(da_crisis_a,da_household_yoy,by=c("country","ym"))
da_crisis_a[,mean(household_yoy)]

da_crisis_start <- da_crisis_a[crisis_start==1,ym,by=country]
da_crisis_bubble_house_start <- da_crisis_a[crisis_bubble_house==1,ym,by=country]
da_crisis_bubble_stock_start <- da_crisis_a[crisis_bubble_stock==1,ym,by=country]
da_crisis_bubble_house_credit_start <- da_crisis_a[crisis_bubble_house_credit==1,ym,by=country]
da_crisis_bubble_stock_credit_start <- da_crisis_a[crisis_bubble_stock_credit==1,ym,by=country]

da_crisis_result <- da_crisis_a[,.(country,ym,crisis_start,
                                   crisis_bubble_house,crisis_bubble_stock,
                                   crisis_bubble_house_credit,crisis_bubble_stock_credit)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,household:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(household_yoy)]]
  }
}

da_crisis_result[crisis_start==1,mean(household)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_house_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,household_bubble_house:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(household_yoy)]]
  }
}

#da_crisis_result[crisis_bubble_house==1,mean(household)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_stock_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,household_bubble_stock:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(household_yoy)]]
  }
}

#da_crisis_result[crisis_bubble_stock==1,mean(household)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_house_credit_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,household_bubble_house_credit:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(household_yoy)]]
  }
}

da_crisis_result[crisis_bubble_house_credit==1,mean(household)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_stock_credit_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,household_bubble_stock_credit:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(household_yoy)]]
  }
}

da_crisis_result[crisis_bubble_stock_credit==1,mean(household)]

#################################
#### Corporation Credit Growth Rate 
load("F:/我的论文/第八篇/RData/da_crisis_hp.RData")
#load("F:/我的论文/第八篇/RData/da_crisis_bs.RData")
load("F:/我的论文/第八篇/RData/da_corporation_yoy.RData")
da_crisis_a <- merge(da_crisis_a,da_corporation_yoy,by=c("country","ym"))
da_crisis_a[,mean(corporation_yoy)]

da_crisis_start <- da_crisis_a[crisis_start==1,ym,by=country]
da_crisis_bubble_house_start <- da_crisis_a[crisis_bubble_house==1,ym,by=country]
da_crisis_bubble_stock_start <- da_crisis_a[crisis_bubble_stock==1,ym,by=country]
da_crisis_bubble_house_credit_start <- da_crisis_a[crisis_bubble_house_credit==1,ym,by=country]
da_crisis_bubble_stock_credit_start <- da_crisis_a[crisis_bubble_stock_credit==1,ym,by=country]

da_crisis_result <- da_crisis_a[,.(country,ym,crisis_start,
                                   crisis_bubble_house,crisis_bubble_stock,
                                   crisis_bubble_house_credit,crisis_bubble_stock_credit)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,corporation:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(corporation_yoy)]]
  }
}

da_crisis_result[crisis_start==1,mean(corporation)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_house_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,corporation_bubble_house:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(corporation_yoy)]]
  }
}

#da_crisis_result[crisis_bubble_house==1,mean(corporation)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_stock_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,corporation_bubble_stock:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(corporation_yoy)]]
  }
}

#da_crisis_result[crisis_bubble_stock==1,mean(corporation)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_house_credit_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,corporation_bubble_house_credit:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(corporation_yoy)]]
  }
}

da_crisis_result[crisis_bubble_house_credit==1,mean(corporation)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_stock_credit_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,corporation_bubble_stock_credit:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(corporation_yoy)]]
  }
}

da_crisis_result[crisis_bubble_stock_credit==1,mean(corporation)]

#################################
#### Government Credit Growth Rate 
load("F:/我的论文/第八篇/RData/da_crisis_hp.RData")
#load("F:/我的论文/第八篇/RData/da_crisis_bs.RData")
load("F:/我的论文/第八篇/RData/da_government_yoy.RData")
da_crisis_a <- merge(da_crisis_a,da_government_yoy,by=c("country","ym"))
da_crisis_a[,mean(government_yoy)]

da_crisis_start <- da_crisis_a[crisis_start==1,ym,by=country]
da_crisis_bubble_house_start <- da_crisis_a[crisis_bubble_house==1,ym,by=country]
da_crisis_bubble_stock_start <- da_crisis_a[crisis_bubble_stock==1,ym,by=country]
da_crisis_bubble_house_credit_start <- da_crisis_a[crisis_bubble_house_credit==1,ym,by=country]
da_crisis_bubble_stock_credit_start <- da_crisis_a[crisis_bubble_stock_credit==1,ym,by=country]

da_crisis_result <- da_crisis_a[,.(country,ym,crisis_start,
                                   crisis_bubble_house,crisis_bubble_stock,
                                   crisis_bubble_house_credit,crisis_bubble_stock_credit)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,government:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(government_yoy)]]
  }
}

da_crisis_result[crisis_start==1,mean(government)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_house_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,government_bubble_house:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(government_yoy)]]
  }
}

#da_crisis_result[crisis_bubble_house==1,mean(government)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_stock_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,government_bubble_stock:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(government_yoy)]]
  }
}

#da_crisis_result[crisis_bubble_stock==1,mean(government)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_house_credit_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,government_bubble_house_credit:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(government_yoy)]]
  }
}

da_crisis_result[crisis_bubble_house_credit==1,mean(government)]

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_bubble_stock_credit_start[country==i,ym]) {
    da_crisis_result[country==i & ym==j,government_bubble_stock_credit:=da_crisis_a[country==i & ym-years(4)<j & ym-years(1)>=j,mean(government_yoy)]]
  }
}

da_crisis_result[crisis_bubble_stock_credit==1,mean(government)]

