load("F:/我的论文/第八篇/RData/da_gdp.RData")
load("F:/我的论文/第八篇/RData/da_cpi_index.RData")

#################################
da_gdp <- merge(da_gdp,da_cpi_index,by=c("country","ym"))
da_gdp[,gdp_real:=gdp/cpi_index]
da_gdp[,gdp_yoy:=c(rep(NA,4),diff(log(gdp_real),lag=4)),by=country]
da_gdp_yoy <- na.omit(da_gdp[,.(country,ym,gdp_yoy)])

save(da_gdp_yoy,file="C:/Users/Ding/Desktop/da_gdp_yoy.RData")

