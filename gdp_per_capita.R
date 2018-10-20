library(data.table)
library(stringr)
library(lubridate)

#################################
#### Calculate the Real Quarterly GDP Per Capita Data

da_gdp_per_y <- read.csv("C:/Users/Ding/Desktop/数据/csv格式/world bank/GDP per capita (constant 2010 US$).csv",header=F,stringsAsFactors=F)
# LCU denotes local currency.
da_gdp_per_y <- as.data.table(t(da_gdp_per_y))
names(da_gdp_per_y)[1] <- "date_y"
date_ym <- ymd(paste0(as.numeric(da_gdp_per_y[-(1:4),date_y]),"-12-1"))

da_gdp_per_y[,date_y:=NULL]

names(da_gdp_per_y) <- as.character(da_gdp_per_y[1,])
da_gdp_per_y <- da_gdp_per_y[-(1:4),]
da_gdp_per_y[,ym:=date_ym]

da_gdp_per_y <- melt(da_gdp_per_y, id.vars="ym",variable.name="country",
                 value.name="gdp_per",variable.factor=F)
#class(da_gdp_per_y[,gdp_per])

da_gdp_per_y <- na.omit(da_gdp_per_y)
da_gdp_per_y <- da_gdp_per_y[order(country,ym),]

# Note: Code below shows that GDP per capita data for all countries are continuous.
country_name_raw <- da_gdp_per_y[,unique(country)]
country_no_omit <- as.character(da_gdp_per_y[,.(no_omit=(year(ym[.N])-year(ym[1])==.N-1)),
                                         by=country][no_omit==T,country])
da_gdp_per_y <- da_gdp_per_y[country %in% country_no_omit,]
country_name_continuous <- da_gdp_per_y[,unique(country)]
for (i in country_name_raw) {
  if (!i %in% country_name_continuous) print(i)
}

# Note: Four small countries has only one observation respectively. 
# For linear interpolation we need at least two observations.
da_gdp_per_y <- da_gdp_per_y[country %in% da_gdp_per_y[,.N,by=country][N>1,country],]

# linear interpolation.
da_gdp_per <- da_gdp_per_y[,.(ym=seq.Date(ym[1],ym[.N],by="quarter"),
                      gdp_per=approx(gdp_per,n=4*.N-3)$y),by=country]

#da_gdp_per[,unique(country)]
da_gdp_per[country=="Czech Republic",country:="Czech"]
da_gdp_per[country=="Korea, Rep.",country:="South Korea"]
da_gdp_per[country=="Russian Federation",country:="Russia"]
da_gdp_per[country=="Slovak Republic",country:="Slovakia"]

da_gdp_per <- da_gdp_per[country %in% c("Australia","Austria","Belgium","Brazil","Canada",
                                        "Chile","China","Czech","Denmark","Estonia",
                                        "Finland","France","Germany","Greece","Hungary",
                                        "Iceland","India","Ireland","Israel","Italy",
                                        "Japan","Latvia","Luxembourg","Mexico","Netherlands",
                                        "New Zealand","Norway","Poland","Portugal","Russia",
                                        "Slovakia","Slovenia","South Africa","South Korea","Spain",
                                        "Sweden","Switzerland","Turkey","United Kingdom","United States"),]
#da_gdp_per[,unique(country)]

da_gdp_per[,gdp_per:=gdp_per/10000]
# Thus the unit is 10,000 dollar

save(da_gdp_per,file="C:/Users/Ding/Desktop/da_gdp_per.RData")
