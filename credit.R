library(data.table)
library(stringr)
library(lubridate)

#################################
da_credit <- read.csv("C:/Users/Ding/Desktop/数据/csv格式/BIS/credit.csv",header=F,stringsAsFactors=F)
da_credit <- as.data.table(da_credit)
names(da_credit)[1] <- "date_dmy"
date_ymd <- dmy(da_credit[-(1:4),date_dmy])
date_ym <- ymd(paste0(year(date_ymd),"-",month(date_ymd),"-01"))

da_credit <- da_credit[,str_detect(da_credit[1,],"Domestic currency"),with=F]
da_credit <- da_credit[,str_detect(da_credit[1,],"Market value"),with=F]
da_credit <- da_credit[,str_detect(da_credit[1,],"All sectors"),with=F]
da_credit <- da_credit[,str_detect(da_credit[1,],"Adjusted for breaks"),with=F]

names(da_credit) <- as.character(da_credit[3,])

da_non_finance <- da_credit[,str_detect(da_credit[1,],"Non financial sector"),with=F]
da_private <- da_credit[,str_detect(da_credit[1,],"Private non-financial sector"),with=F]
da_household <- da_credit[,str_detect(da_credit[1,],"Households and NPISHs"),with=F]
da_corporation <- da_credit[,str_detect(da_credit[1,],"Non-financial corporations"),with=F]
da_government <- da_credit[,str_detect(da_credit[1,],"General government"),with=F]

#################################
#### Credit to Non-Financial Sector
da_non_finance <- da_non_finance[-(1:4),]
da_non_finance[,ym:=date_ym]
da_non_finance <- melt(da_non_finance, id.vars="ym",variable.name="country",
                       value.name="non_finance",variable.factor=F)
da_non_finance[,non_finance:=as.numeric(non_finance)]
da_non_finance <- na.omit(da_non_finance[ym>=ymd("1970-1-1"),])
da_non_finance <- da_non_finance[order(country,ym),]
#save(da_non_finance,file="C:/Users/Ding/Desktop/da_non_finance.RData")

#### Credit to Private Non-Financial Sector
da_private <- da_private[-(1:4),]
da_private[,ym:=date_ym]
da_private <- melt(da_private, id.vars="ym",variable.name="country",
                   value.name="private",variable.factor=F)
da_private[,private:=as.numeric(private)]
da_private <- na.omit(da_private[ym>=ymd("1970-1-1"),])
da_private <- da_private[order(country,ym),]
#save(da_private,file="C:/Users/Ding/Desktop/da_private.RData")

#### Credit to Households
da_household <- da_household[-(1:4),]
da_household[,ym:=date_ym]
da_household <- melt(da_household, id.vars="ym",variable.name="country",
                     value.name="household",variable.factor=F)
da_household[,household:=as.numeric(household)]
da_household <- na.omit(da_household[ym>=ymd("1970-1-1"),])
da_household <- da_household[order(country,ym),]
#save(da_household,file="C:/Users/Ding/Desktop/da_household.RData")

#### Credit to Non-Financial Corporations
da_corporation <- da_corporation[-(1:4),]
da_corporation[,ym:=date_ym]
da_corporation <- melt(da_corporation, id.vars="ym",variable.name="country",
                       value.name="corporation",variable.factor=F)
da_corporation[,corporation:=as.numeric(corporation)]
da_corporation <- na.omit(da_corporation[ym>=ymd("1970-1-1"),])
da_corporation <- da_corporation[order(country,ym),]
#save(da_corporation,file="C:/Users/Ding/Desktop/da_corporation.RData")

#### Credit to General Government
da_government <- da_government[-(1:4),]
da_government[,ym:=date_ym]
da_government <- melt(da_government, id.vars="ym",variable.name="country",
                      value.name="government",variable.factor=F)
da_government[,government:=as.numeric(government)]
da_government <- na.omit(da_government[ym>=ymd("1970-1-1"),])
da_government <- da_government[order(country,ym),]
#save(da_government,file="C:/Users/Ding/Desktop/da_government.RData")

# Note: Non-Financial Sector= Households+Corporations+Government
# Private Non-Financial Sector= Households+Corporations

