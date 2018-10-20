#################################
load("F:/我的论文/第八篇/RData/da_covar.RData")
load("F:/我的论文/第八篇/RData/da_household_yoy.RData")
load("F:/我的论文/第八篇/RData/da_house_yoy.RData")
load("F:/我的论文/第八篇/RData/da_stock_yoy.RData")
load("F:/我的论文/第八篇/RData/da_gdp_yoy.RData")
load("F:/我的论文/第八篇/RData/da_cpi_yoy.RData")
load("F:/我的论文/第八篇/RData/da_net_export.RData")

pre <- 2
da_covar[,ym_pre:=ym-years(pre)]
da_covar <- da_covar[,.(country,ym=ym_pre,covar)]

da_risk <- merge(da_covar,da_household_yoy,by=c("country","ym"))
da_risk <- merge(da_risk,da_house_yoy,by=c("country","ym"))
da_risk <- merge(da_risk,da_stock_yoy,by=c("country","ym"))
da_risk <- merge(da_risk,da_gdp_yoy,by=c("country","ym"))
da_risk <- merge(da_risk,da_cpi_yoy,by=c("country","ym"))
da_risk <- merge(da_risk,da_net_export,by=c("country","ym"))

model_1 <- plm(covar ~ household_yoy+house_yoy+stock_yoy+gdp_yoy+cpi_yoy+net_export,
               data=da_risk,model="within",effect="individual",index=c("country","ym"))
#summary(model_1)
summary(model_1)$coefficients

model_2 <- plm(covar ~ household_yoy+house_yoy+stock_yoy+
                 I(house_yoy*household_yoy)+ I(stock_yoy*household_yoy)+
                 gdp_yoy+cpi_yoy,
               data=da_risk,model="within",effect="individual",index=c("country","ym"))
#summary(model_2)
summary(model_2)$coefficients

#################################
load("F:/我的论文/第八篇/RData/da_covar.RData")
load("F:/我的论文/第八篇/RData/da_household_yoy.RData")
load("F:/我的论文/第八篇/RData/da_bubble_house_hp.RData")
load("F:/我的论文/第八篇/RData/da_bubble_stock_hp.RData")
load("F:/我的论文/第八篇/RData/da_gdp_yoy.RData")
load("F:/我的论文/第八篇/RData/da_cpi_yoy.RData")
load("F:/我的论文/第八篇/RData/da_net_export.RData")

pre <- 2
da_covar[,ym_pre:=ym-years(pre)]
da_covar <- da_covar[,.(country,ym=ym_pre,covar)]

da_risk <- merge(da_covar,da_household_yoy,by=c("country","ym"))
da_risk <- merge(da_risk,da_bubble_house_hp,by=c("country","ym"))
da_risk <- merge(da_risk,da_bubble_stock_hp,by=c("country","ym"))
da_risk <- merge(da_risk,da_gdp_yoy,by=c("country","ym"))
da_risk <- merge(da_risk,da_cpi_yoy,by=c("country","ym"))
da_risk <- merge(da_risk,da_net_export,by=c("country","ym"))

model_1 <- plm(covar ~ household_yoy+bubble_house+bubble_stock+gdp_yoy+cpi_yoy+net_export,
               data=da_risk,model="within",effect="individual",index=c("country","ym"))
#summary(model_1)
summary(model_1)$coefficients

model_2 <- plm(covar ~ household_yoy+bubble_house+bubble_stock+
                 I(bubble_house*household_yoy)+ I(bubble_stock*household_yoy)+
                 gdp_yoy+cpi_yoy+net_export,
               data=da_risk,model="within",effect="individual",index=c("country","ym"))
#summary(model_2)
summary(model_2)$coefficients

#################################
load("F:/我的论文/第八篇/RData/da_covar.RData")
load("F:/我的论文/第八篇/RData/da_household_yoy.RData")
load("F:/我的论文/第八篇/RData/da_bubble_house_bs.RData")
load("F:/我的论文/第八篇/RData/da_bubble_stock_bs.RData")
load("F:/我的论文/第八篇/RData/da_gdp_yoy.RData")
load("F:/我的论文/第八篇/RData/da_cpi_yoy.RData")
load("F:/我的论文/第八篇/RData/da_net_export.RData")

pre <- 2
da_covar[,ym_pre:=ym-years(pre)]
da_covar <- da_covar[,.(country,ym=ym_pre,covar)]

da_risk <- merge(da_covar,da_household_yoy,by=c("country","ym"))
da_risk <- merge(da_risk,da_bubble_house_bs,by=c("country","ym"))
da_risk <- merge(da_risk,da_bubble_stock_bs,by=c("country","ym"))
da_risk <- merge(da_risk,da_gdp_yoy,by=c("country","ym"))
da_risk <- merge(da_risk,da_cpi_yoy,by=c("country","ym"))
da_risk <- merge(da_risk,da_net_export,by=c("country","ym"))

model_1 <- plm(covar ~ household_yoy+bubble_house+bubble_stock+gdp_yoy+cpi_yoy+net_export,
               data=da_risk,model="within",effect="individual",index=c("country","ym"))
#summary(model_1)
summary(model_1)$coefficients

model_2 <- plm(covar ~ household_yoy+bubble_house+bubble_stock+
                 I(bubble_house*household_yoy)+ I(bubble_stock*household_yoy)+
                 gdp_yoy+cpi_yoy+net_export,
               data=da_risk,model="within",effect="individual",index=c("country","ym"))
#summary(model_2)
summary(model_2)$coefficients


