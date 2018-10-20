library(data.table)
library(lubridate)
library(plm)
library(lmtest) # coeftest

sink(file="C:/Users/Ding/Desktop/主代码.txt",append=T)
sink()
rm(list=ls())

#################################
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_non_finance_yoy.RData")

da_crisis <- merge(da_crisis,da_non_finance_yoy,by=c("country","ym"))
da_crisis[,pre_crisis_1_5:=pre_crisis_1+pre_crisis_2+pre_crisis_3+pre_crisis_4+pre_crisis_5]
#write.csv(da_crisis,file="C:/Users/Ding/Desktop/da_crisis_1.csv",row.names=F)

model_1 <- plm(pre_crisis_1_5 ~ non_finance_yoy,data=da_crisis,
               model="within",effect="individual",index=c("country","ym"))
#summary(model_1)
summary(model_1)$coefficients

#################################
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_household_yoy.RData")
load("F:/我的论文/第八篇/RData/da_corporation_yoy.RData")
load("F:/我的论文/第八篇/RData/da_government_yoy.RData")

da_crisis <- merge(da_crisis,da_household_yoy,by=c("country","ym"))
da_crisis <- merge(da_crisis,da_corporation_yoy,by=c("country","ym"))
da_crisis <- merge(da_crisis,da_government_yoy,by=c("country","ym"))
da_crisis[,pre_crisis_1_5:=pre_crisis_1+pre_crisis_2+pre_crisis_3+pre_crisis_4+pre_crisis_5]
#write.csv(da_crisis,file="C:/Users/Ding/Desktop/da_crisis_2.csv",row.names=F)

model_1 <- plm(pre_crisis_1_5 ~ household_yoy+corporation_yoy+government_yoy,
               data=da_crisis,model="within",effect="individual",index=c("country","ym"))
#summary(model_1)
summary(model_1)$coefficients

#################################
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_house_yoy.RData")
load("F:/我的论文/第八篇/RData/da_stock_yoy.RData")

da_crisis <- merge(da_crisis,da_house_yoy,by=c("country","ym"))
da_crisis <- merge(da_crisis,da_stock_yoy,by=c("country","ym"))
da_crisis[,pre_crisis_1_5:=pre_crisis_1+pre_crisis_2+pre_crisis_3+pre_crisis_4+pre_crisis_5]
#write.csv(da_crisis,file="C:/Users/Ding/Desktop/da_crisis_3.csv",row.names=F)

model_1 <- plm(pre_crisis_1_5 ~ house_yoy+stock_yoy,
               data=da_crisis,model="within",effect="individual",index=c("country","ym"))
#summary(model_1)
summary(model_1)$coefficients

#################################
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_household_yoy.RData")
load("F:/我的论文/第八篇/RData/da_house_yoy.RData")
load("F:/我的论文/第八篇/RData/da_stock_yoy.RData")

da_crisis <- merge(da_crisis,da_household_yoy,by=c("country","ym"))
da_crisis <- merge(da_crisis,da_house_yoy,by=c("country","ym"))
da_crisis <- merge(da_crisis,da_stock_yoy,by=c("country","ym"))
da_crisis[,pre_crisis_1_5:=pre_crisis_1+pre_crisis_2+pre_crisis_3+pre_crisis_4+pre_crisis_5]
#write.csv(da_crisis,file="C:/Users/Ding/Desktop/da_crisis_4.csv",row.names=F)

model_1 <- plm(pre_crisis_1_5 ~ household_yoy+house_yoy+stock_yoy,
               data=da_crisis,model="within",effect="individual",index=c("country","ym"))
#summary(model_1)
summary(model_1)$coefficients

model_2 <- plm(pre_crisis_1_5 ~ household_yoy+house_yoy+stock_yoy+
                 I(house_yoy*household_yoy)+ I(stock_yoy*household_yoy),
               data=da_crisis,model="within",effect="individual",index=c("country","ym"))
#summary(model_2)
summary(model_2)$coefficients

#################################
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_household_yoy.RData")
load("F:/我的论文/第八篇/RData/da_bubble_house_hp.RData")
load("F:/我的论文/第八篇/RData/da_bubble_stock_hp.RData")

da_crisis <- merge(da_crisis,da_household_yoy,by=c("country","ym"))
da_crisis <- merge(da_crisis,da_bubble_house_hp,by=c("country","ym"))
da_crisis <- merge(da_crisis,da_bubble_stock_hp,by=c("country","ym"))
da_crisis[,pre_crisis_1_5:=pre_crisis_1+pre_crisis_2+pre_crisis_3+pre_crisis_4+pre_crisis_5]
#write.csv(da_crisis,file="C:/Users/Ding/Desktop/da_crisis_5.csv",row.names=F)

model_1 <- plm(pre_crisis_1_5 ~ household_yoy+bubble_house+bubble_stock,
               data=da_crisis,model="within",effect="individual",index=c("country","ym"))
#summary(model_1)
summary(model_1)$coefficients

model_2 <- plm(pre_crisis_1_5 ~ household_yoy+bubble_house+bubble_stock+
                 I(bubble_house*household_yoy)+ I(bubble_stock*household_yoy),
               data=da_crisis,model="within",effect="individual",index=c("country","ym"))
#summary(model_2)
summary(model_2)$coefficients

#################################
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_household_yoy.RData")
load("F:/我的论文/第八篇/RData/da_bubble_house_bs.RData")
load("F:/我的论文/第八篇/RData/da_bubble_stock_bs.RData")

da_crisis <- merge(da_crisis,da_household_yoy,by=c("country","ym"))
da_crisis <- merge(da_crisis,da_bubble_house_bs,by=c("country","ym"))
da_crisis <- merge(da_crisis,da_bubble_stock_bs,by=c("country","ym"))
da_crisis[,pre_crisis_1_5:=pre_crisis_1+pre_crisis_2+pre_crisis_3+pre_crisis_4+pre_crisis_5]
#write.csv(da_crisis,file="C:/Users/Ding/Desktop/da_crisis_6.csv",row.names=F)

model_1 <- plm(pre_crisis_1_5 ~ household_yoy+bubble_house+bubble_stock,
               data=da_crisis,model="within",effect="individual",index=c("country","ym"))
#summary(model_1)
summary(model_1)$coefficients

model_2 <- plm(pre_crisis_1_5 ~ household_yoy+bubble_house+bubble_stock+
                 I(bubble_house*household_yoy)+ I(bubble_stock*household_yoy),
               data=da_crisis,model="within",effect="individual",index=c("country","ym"))
#summary(model_2)
summary(model_2)$coefficients

