#### Replace CPI by GDP 

load("F:/我的论文/第八篇/RData/da_non_finance.RData")
load("F:/我的论文/第八篇/RData/da_private.RData")
load("F:/我的论文/第八篇/RData/da_household.RData")
load("F:/我的论文/第八篇/RData/da_corporation.RData")
load("F:/我的论文/第八篇/RData/da_government.RData")
load("F:/我的论文/第八篇/RData/da_gdp.RData")

#################################
#### Credit to Non-Financial Sector
da_non_finance <- merge(da_non_finance,da_gdp,by=c("country","ym"))
da_non_finance[,non_finance_real:=non_finance/gdp*10^9]
da_non_finance[,non_finance_yoy:=c(rep(NA,4),diff(log(non_finance_real),lag=4)),by=country]
da_non_finance_yoy_a <- na.omit(da_non_finance[,.(country,ym,non_finance_yoy)])
da_non_finance_yoy_a[,boom_non_finance:=ifelse(non_finance_yoy>=mean(non_finance_yoy),1,0),by=country]

#save(da_non_finance_yoy_a,file="C:/Users/Ding/Desktop/da_non_finance_yoy_a.RData")

#### Credit to Private Non-Financial Sector
da_private <- merge(da_private,da_gdp,by=c("country","ym"))
da_private[,private_real:=private/gdp*10^9]
da_private[,private_yoy:=c(rep(NA,4),diff(log(private_real),lag=4)),by=country]
da_private_yoy_a <- na.omit(da_private[,.(country,ym,private_yoy)])
da_private_yoy_a[,boom_private:=ifelse(private_yoy>=mean(private_yoy),1,0),by=country]

#save(da_private_yoy_a,file="C:/Users/Ding/Desktop/da_private_yoy_a.RData")

#### Credit to Households
da_household <- merge(da_household,da_gdp,by=c("country","ym"))
da_household[,household_real:=household/gdp*10^9]
da_household[,household_yoy:=c(rep(NA,4),diff(log(household_real),lag=4)),by=country]
da_household_yoy_a <- na.omit(da_household[,.(country,ym,household_yoy)])
da_household_yoy_a[,boom_household:=ifelse(household_yoy>=mean(household_yoy),1,0),by=country]

#save(da_household_yoy_a,file="C:/Users/Ding/Desktop/da_household_yoy_a.RData")

#### Credit to Non-Financial Corporations
da_corporation <- merge(da_corporation,da_gdp,by=c("country","ym"))
da_corporation[,corporation_real:=corporation/gdp*10^9]
da_corporation[,corporation_yoy:=c(rep(NA,4),diff(log(corporation_real),lag=4)),by=country]
da_corporation_yoy_a <- na.omit(da_corporation[,.(country,ym,corporation_yoy)])
da_corporation_yoy_a[,boom_corporation:=ifelse(corporation_yoy>=mean(corporation_yoy),1,0),by=country]

#save(da_corporation_yoy_a,file="C:/Users/Ding/Desktop/da_corporation_yoy_a.RData")

#### Credit to General Government
da_government <- merge(da_government,da_gdp,by=c("country","ym"))
da_government[,government_real:=government/gdp*10^9]
da_government[,government_yoy:=c(rep(NA,4),diff(log(government_real),lag=4)),by=country]
da_government_yoy_a <- na.omit(da_government[,.(country,ym,government_yoy)])
da_government_yoy_a[,boom_government:=ifelse(government_yoy>=mean(government_yoy),1,0),by=country]

#save(da_government_yoy_a,file="C:/Users/Ding/Desktop/da_government_yoy_a.RData")
