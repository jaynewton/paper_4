load("F:/我的论文/第八篇/RData/da_non_finance.RData")
load("F:/我的论文/第八篇/RData/da_private.RData")
load("F:/我的论文/第八篇/RData/da_household.RData")
load("F:/我的论文/第八篇/RData/da_corporation.RData")
load("F:/我的论文/第八篇/RData/da_government.RData")
load("F:/我的论文/第八篇/RData/da_cpi_index.RData")

#################################
#### Credit to Non-Financial Sector
da_non_finance <- merge(da_non_finance,da_cpi_index,by=c("country","ym"))
da_non_finance[,non_finance_real:=non_finance/cpi_index]
da_non_finance[,non_finance_yoy:=c(rep(NA,4),diff(log(non_finance_real),lag=4)),by=country]
da_non_finance_yoy <- na.omit(da_non_finance[,.(country,ym,non_finance_yoy)])
da_non_finance_yoy[,boom_non_finance:=ifelse(non_finance_yoy>=mean(non_finance_yoy),1,0),by=country]

#save(da_non_finance_yoy,file="C:/Users/Ding/Desktop/da_non_finance_yoy.RData")

#### Credit to Private Non-Financial Sector
da_private <- merge(da_private,da_cpi_index,by=c("country","ym"))
da_private[,private_real:=private/cpi_index]
da_private[,private_yoy:=c(rep(NA,4),diff(log(private_real),lag=4)),by=country]
da_private_yoy <- na.omit(da_private[,.(country,ym,private_yoy)])
da_private_yoy[,boom_private:=ifelse(private_yoy>=mean(private_yoy),1,0),by=country]

#save(da_private_yoy,file="C:/Users/Ding/Desktop/da_private_yoy.RData")

#### Credit to Households
da_household <- merge(da_household,da_cpi_index,by=c("country","ym"))
da_household[,household_real:=household/cpi_index]
da_household[,household_yoy:=c(rep(NA,4),diff(log(household_real),lag=4)),by=country]
da_household_yoy <- na.omit(da_household[,.(country,ym,household_yoy)])
da_household_yoy[,boom_household:=ifelse(household_yoy>=mean(household_yoy),1,0),by=country]

#save(da_household_yoy,file="C:/Users/Ding/Desktop/da_household_yoy.RData")

#### Credit to Non-Financial Corporations
da_corporation <- merge(da_corporation,da_cpi_index,by=c("country","ym"))
da_corporation[,corporation_real:=corporation/cpi_index]
da_corporation[,corporation_yoy:=c(rep(NA,4),diff(log(corporation_real),lag=4)),by=country]
da_corporation_yoy <- na.omit(da_corporation[,.(country,ym,corporation_yoy)])
da_corporation_yoy[,boom_corporation:=ifelse(corporation_yoy>=mean(corporation_yoy),1,0),by=country]

#save(da_corporation_yoy,file="C:/Users/Ding/Desktop/da_corporation_yoy.RData")

#### Credit to General Government
da_government <- merge(da_government,da_cpi_index,by=c("country","ym"))
da_government[,government_real:=government/cpi_index]
da_government[,government_yoy:=c(rep(NA,4),diff(log(government_real),lag=4)),by=country]
da_government_yoy <- na.omit(da_government[,.(country,ym,government_yoy)])
da_government_yoy[,boom_government:=ifelse(government_yoy>=mean(government_yoy),1,0),by=country]

#save(da_government_yoy,file="C:/Users/Ding/Desktop/da_government_yoy.RData")
