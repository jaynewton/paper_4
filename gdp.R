library(data.table)
library(stringr)
library(lubridate)

#################################
#### Calculate the Nominal Quarterly GDP Data

da_gdp_y <- read.csv("C:/Users/Ding/Desktop/数据/csv格式/world bank/GDP (current LCU).csv",header=F,stringsAsFactors=F)
# LCU denotes local currency.
da_gdp_y <- as.data.table(t(da_gdp_y))
names(da_gdp_y)[1] <- "date_y"
date_ym <- ymd(paste0(as.numeric(da_gdp_y[-(1:4),date_y]),"-12-1"))

da_gdp_y[,date_y:=NULL]

names(da_gdp_y) <- as.character(da_gdp_y[1,])
da_gdp_y <- da_gdp_y[-(1:4),]
da_gdp_y[,ym:=date_ym]

da_gdp_y <- melt(da_gdp_y, id.vars="ym",variable.name="country",
                 value.name="gdp",variable.factor=F)
#class(da_gdp_y[,gdp])

# GDP data for Switzerland and New Zealand are not continuous
# However, we want to include them.
da_gdp_y[country=="Switzerland" & ym<=ymd("1969-12-31"),gdp:=NA]
da_gdp_y[country=="New Zealand" & ym<=ymd("1969-12-31"),gdp:=NA]

da_gdp_y <- na.omit(da_gdp_y)
da_gdp_y <- da_gdp_y[order(country,ym),]

# Note: GDP data for some countries are not continuous. Says, Afghanistan.
# We delete the observations for these countries.
country_name_raw <- da_gdp_y[,unique(country)]
country_no_omit <- as.character(da_gdp_y[,.(no_omit=(year(ym[.N])-year(ym[1])==.N-1)),
                                         by=country][no_omit==T,country])
da_gdp_y <- da_gdp_y[country %in% country_no_omit,]
country_name_continuous <- da_gdp_y[,unique(country)]
for (i in country_name_raw) {
  if (!i %in% country_name_continuous) print(i)
}

# Note: The cubic spline method below would result in negative GDP.
#da_gdp <- da_gdp_y[,.(ym=seq.Date(ym[1],ym[.N],by="quarter"),
#                      gdp=spline(gdp,method="natural",n=4*.N-3)$y),by=country]

# That's why we use linear interpolation.
da_gdp <- da_gdp_y[,.(ym=seq.Date(ym[1],ym[.N],by="quarter"),
                      gdp=approx(gdp,n=4*.N-3)$y),by=country]

#da_gdp[,unique(country)]
da_gdp[country=="Czech Republic",country:="Czech"]
da_gdp[country=="Korea, Rep.",country:="South Korea"]
da_gdp[country=="Russian Federation",country:="Russia"]
da_gdp[country=="Slovak Republic",country:="Slovakia"]

da_gdp <- da_gdp[country %in% c("Australia","Austria","Belgium","Brazil","Canada",
                                "Chile","China","Czech","Denmark","Estonia",
                                "Finland","France","Germany","Greece","Hungary",
                                "Iceland","India","Ireland","Israel","Italy",
                                "Japan","Latvia","Luxembourg","Mexico","Netherlands",
                                "New Zealand","Norway","Poland","Portugal","Russia",
                                "Slovakia","Slovenia","South Africa","South Korea","Spain",
                                "Sweden","Switzerland","Turkey","United Kingdom","United States"),]
#da_gdp[,unique(country)]

save(da_gdp,file="C:/Users/Ding/Desktop/da_gdp.RData")


