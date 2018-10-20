library(data.table)
library(lubridate)
library(plm)
library(ggplot2)
library(Rmisc) # for multiplot

sink(file="C:/Users/Ding/Desktop/主代码.txt",append=T)
sink()
rm(list=ls())

#################################
#### Credit to Non-Financial Sector
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_non_finance_yoy.RData")

da_crisis_non_finance <- merge(da_crisis,da_non_finance_yoy,by=c("country","ym"))
result_crisis_non_finance <- matrix(NA,nrow=4,ncol=10)
colnames(result_crisis_non_finance) <- c(paste0("pre_",5:1),paste0("post_",1:5))

#### Pre-Crisis
model_pre_5 <- plm(non_finance_yoy ~ pre_crisis_5,data=da_crisis_non_finance,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_5)
model_pre_4 <- plm(non_finance_yoy ~ pre_crisis_4,data=da_crisis_non_finance,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_4)
model_pre_3 <- plm(non_finance_yoy ~ pre_crisis_3,data=da_crisis_non_finance,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_3)
model_pre_2 <- plm(non_finance_yoy ~ pre_crisis_2,data=da_crisis_non_finance,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_2)
model_pre_1 <- plm(non_finance_yoy ~ pre_crisis_1,data=da_crisis_non_finance,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_1)

for (i in 1:5) {
  result_crisis_non_finance[,i] <- summary(get(paste0("model_pre_",6-i)))$coefficients[1,]
}

#### Post-Crisis
model_post_1 <- plm(non_finance_yoy ~ post_crisis_1,data=da_crisis_non_finance,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_post_1)
model_post_2 <- plm(non_finance_yoy ~ post_crisis_2,data=da_crisis_non_finance,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_post_2)
model_post_3 <- plm(non_finance_yoy ~ post_crisis_3,data=da_crisis_non_finance,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_post_3)
model_post_4 <- plm(non_finance_yoy ~ post_crisis_4,data=da_crisis_non_finance,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_post_4)
model_post_5 <- plm(non_finance_yoy ~ post_crisis_5,data=da_crisis_non_finance,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_5)

for (i in 6:10) {
  result_crisis_non_finance[,i] <- summary(get(paste0("model_post_",i-5)))$coefficients[1,]
}
result_crisis_non_finance

data_plot <- data.table(Time=factor(c(paste0("前",5:1),paste0("后",1:5)),
                                    levels=c(paste0("前",5:1),paste0("后",1:5))),
                        non_finance=result_crisis_non_finance[1,]*100)

p <- ggplot(data_plot,aes(x=Time,y=non_finance,group=1)) 
p_1 <- p+geom_line()+geom_hline(aes(yintercept=0),col="grey",linetype="dashed")+
  ylab("非金融部门信贷")+theme_bw()+
  theme(panel.grid=element_blank())+theme(axis.title.x=element_blank())+
  theme(axis.title=element_text(size=rel(1.25)),axis.text=element_text(size=rel(1.25)))
#p_1
save(p_1,file="C:/Users/Ding/Desktop/p_1.RData")

#################################
#### Credit to Households
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_household_yoy.RData")

da_crisis_household <- merge(da_crisis,da_household_yoy,by=c("country","ym"))
result_crisis_household <- matrix(NA,nrow=4,ncol=10)
colnames(result_crisis_household) <- c(paste0("pre_",5:1),paste0("post_",1:5))

#### Pre-Crisis
model_pre_5 <- plm(household_yoy ~ pre_crisis_5,data=da_crisis_household,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_5)
model_pre_4 <- plm(household_yoy ~ pre_crisis_4,data=da_crisis_household,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_4)
model_pre_3 <- plm(household_yoy ~ pre_crisis_3,data=da_crisis_household,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_3)
model_pre_2 <- plm(household_yoy ~ pre_crisis_2,data=da_crisis_household,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_2)
model_pre_1 <- plm(household_yoy ~ pre_crisis_1,data=da_crisis_household,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_1)

for (i in 1:5) {
  result_crisis_household[,i] <- summary(get(paste0("model_pre_",6-i)))$coefficients[1,]
}

#### Post-Crisis
model_post_1 <- plm(household_yoy ~ post_crisis_1,data=da_crisis_household,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_1)
model_post_2 <- plm(household_yoy ~ post_crisis_2,data=da_crisis_household,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_2)
model_post_3 <- plm(household_yoy ~ post_crisis_3,data=da_crisis_household,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_3)
model_post_4 <- plm(household_yoy ~ post_crisis_4,data=da_crisis_household,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_4)
model_post_5 <- plm(household_yoy ~ post_crisis_5,data=da_crisis_household,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_5)

for (i in 6:10) {
  result_crisis_household[,i] <- summary(get(paste0("model_post_",i-5)))$coefficients[1,]
}
result_crisis_household

data_plot <- data.table(Time=factor(c(paste0("前",5:1),paste0("后",1:5)),
                                    levels=c(paste0("前",5:1),paste0("后",1:5))),
                        household=result_crisis_household[1,]*100)

p <- ggplot(data_plot,aes(x=Time,y=household,group=1)) 
p_2 <- p+geom_line()+geom_hline(aes(yintercept=0),col="grey",linetype="dashed")+
  ylab("家庭部门信贷")+theme_bw()+
  theme(panel.grid=element_blank())+theme(axis.title.x=element_blank())+
  theme(axis.title=element_text(size=rel(1.25)),axis.text=element_text(size=rel(1.25)))
#p_2
save(p_2,file="C:/Users/Ding/Desktop/p_2.RData")

#################################
#### Credit to Non-Financial Corporations
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_corporation_yoy.RData")

da_crisis_corporation <- merge(da_crisis,da_corporation_yoy,by=c("country","ym"))
result_crisis_corporation <- matrix(NA,nrow=4,ncol=10)
colnames(result_crisis_corporation) <- c(paste0("pre_",5:1),paste0("post_",1:5))

#### Pre-Crisis
model_pre_5 <- plm(corporation_yoy ~ pre_crisis_5,data=da_crisis_corporation,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_5)
model_pre_4 <- plm(corporation_yoy ~ pre_crisis_4,data=da_crisis_corporation,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_4)
model_pre_3 <- plm(corporation_yoy ~ pre_crisis_3,data=da_crisis_corporation,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_3)
model_pre_2 <- plm(corporation_yoy ~ pre_crisis_2,data=da_crisis_corporation,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_2)
model_pre_1 <- plm(corporation_yoy ~ pre_crisis_1,data=da_crisis_corporation,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_1)

for (i in 1:5) {
  result_crisis_corporation[,i] <- summary(get(paste0("model_pre_",6-i)))$coefficients[1,]
}

#### Post-Crisis
model_post_1 <- plm(corporation_yoy ~ post_crisis_1,data=da_crisis_corporation,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_1)
model_post_2 <- plm(corporation_yoy ~ post_crisis_2,data=da_crisis_corporation,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_2)
model_post_3 <- plm(corporation_yoy ~ post_crisis_3,data=da_crisis_corporation,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_3)
model_post_4 <- plm(corporation_yoy ~ post_crisis_4,data=da_crisis_corporation,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_4)
model_post_5 <- plm(corporation_yoy ~ post_crisis_5,data=da_crisis_corporation,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_5)

for (i in 6:10) {
  result_crisis_corporation[,i] <- summary(get(paste0("model_post_",i-5)))$coefficients[1,]
}
result_crisis_corporation

data_plot <- data.table(Time=factor(c(paste0("前",5:1),paste0("后",1:5)),
                                    levels=c(paste0("前",5:1),paste0("后",1:5))),
                        corporation=result_crisis_corporation[1,]*100)

p <- ggplot(data_plot,aes(x=Time,y=corporation,group=1)) 
p_3 <- p+geom_line()+geom_hline(aes(yintercept=0),col="grey",linetype="dashed")+
  ylab("企业部门信贷")+theme_bw()+
  theme(panel.grid=element_blank())+theme(axis.title.x=element_blank())+
  theme(axis.title=element_text(size=rel(1.25)),axis.text=element_text(size=rel(1.25)))
#p_3
save(p_3,file="C:/Users/Ding/Desktop/p_3.RData")

#################################
#### Credit to General Government
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_government_yoy.RData")

da_crisis_government <- merge(da_crisis,da_government_yoy,by=c("country","ym"))
result_crisis_government <- matrix(NA,nrow=4,ncol=10)
colnames(result_crisis_government) <- c(paste0("pre_",5:1),paste0("post_",1:5))

#### Pre-Crisis
model_pre_5 <- plm(government_yoy ~ pre_crisis_5,data=da_crisis_government,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_5)
model_pre_4 <- plm(government_yoy ~ pre_crisis_4,data=da_crisis_government,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_4)
model_pre_3 <- plm(government_yoy ~ pre_crisis_3,data=da_crisis_government,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_3)
model_pre_2 <- plm(government_yoy ~ pre_crisis_2,data=da_crisis_government,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_2)
model_pre_1 <- plm(government_yoy ~ pre_crisis_1,data=da_crisis_government,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_1)

for (i in 1:5) {
  result_crisis_government[,i] <- summary(get(paste0("model_pre_",6-i)))$coefficients[1,]
}

#### Post-Crisis
model_post_1 <- plm(government_yoy ~ post_crisis_1,data=da_crisis_government,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_1)
model_post_2 <- plm(government_yoy ~ post_crisis_2,data=da_crisis_government,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_2)
model_post_3 <- plm(government_yoy ~ post_crisis_3,data=da_crisis_government,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_3)
model_post_4 <- plm(government_yoy ~ post_crisis_4,data=da_crisis_government,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_4)
model_post_5 <- plm(government_yoy ~ post_crisis_5,data=da_crisis_government,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_5)

for (i in 6:10) {
  result_crisis_government[,i] <- summary(get(paste0("model_post_",i-5)))$coefficients[1,]
}
result_crisis_government

data_plot <- data.table(Time=factor(c(paste0("前",5:1),paste0("后",1:5)),
                                    levels=c(paste0("前",5:1),paste0("后",1:5))),
                        government=result_crisis_government[1,]*100)

p <- ggplot(data_plot,aes(x=Time,y=government,group=1)) 
p_4 <- p+geom_line()+geom_hline(aes(yintercept=0),col="grey",linetype="dashed")+
  ylab("政府部门信贷")+theme_bw()+
  theme(panel.grid=element_blank())+theme(axis.title.x=element_blank())+
  theme(axis.title=element_text(size=rel(1.25)),axis.text=element_text(size=rel(1.25)))
#p_4
save(p_4,file="C:/Users/Ding/Desktop/p_4.RData")

#################################
#### Year-on-Year Growth of House Price 
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_house_yoy.RData")

da_crisis_house_yoy <- merge(da_crisis,da_house_yoy,by=c("country","ym"))
result_crisis_house_yoy <- matrix(NA,nrow=4,ncol=10)
colnames(result_crisis_house_yoy) <- c(paste0("pre_",5:1),paste0("post_",1:5))

#### Pre-Crisis
model_pre_5 <- plm(house_yoy ~ pre_crisis_5,data=da_crisis_house_yoy,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_5)
model_pre_4 <- plm(house_yoy ~ pre_crisis_4,data=da_crisis_house_yoy,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_4)
model_pre_3 <- plm(house_yoy ~ pre_crisis_3,data=da_crisis_house_yoy,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_3)
model_pre_2 <- plm(house_yoy ~ pre_crisis_2,data=da_crisis_house_yoy,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_2)
model_pre_1 <- plm(house_yoy ~ pre_crisis_1,data=da_crisis_house_yoy,
               model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_1)

for (i in 1:5) {
  result_crisis_house_yoy[,i] <- summary(get(paste0("model_pre_",6-i)))$coefficients[1,]
}

#### Post-Crisis
model_post_1 <- plm(house_yoy ~ post_crisis_1,data=da_crisis_house_yoy,
               model="within",effect="individual",index=c("country","ym"))
#summary(model_post_1)
model_post_2 <- plm(house_yoy ~ post_crisis_2,data=da_crisis_house_yoy,
               model="within",effect="individual",index=c("country","ym"))
#summary(model_post_2)
model_post_3 <- plm(house_yoy ~ post_crisis_3,data=da_crisis_house_yoy,
               model="within",effect="individual",index=c("country","ym"))
#summary(model_post_3)
model_post_4 <- plm(house_yoy ~ post_crisis_4,data=da_crisis_house_yoy,
               model="within",effect="individual",index=c("country","ym"))
#summary(model_post_4)
model_post_5 <- plm(house_yoy ~ post_crisis_5,data=da_crisis_house_yoy,
               model="within",effect="individual",index=c("country","ym"))
#summary(model_post_5)

for (i in 6:10) {
  result_crisis_house_yoy[,i] <- summary(get(paste0("model_post_",i-5)))$coefficients[1,]
}
result_crisis_house_yoy

data_plot <- data.table(Time=factor(c(paste0("前",5:1),paste0("后",1:5)),
                                    levels=c(paste0("前",5:1),paste0("后",1:5))),
                        house_yoy=result_crisis_house_yoy[1,]*100)

p <- ggplot(data_plot,aes(x=Time,y=house_yoy,group=1))
p_5 <- p+geom_line()+geom_hline(aes(yintercept=0),col="grey",linetype="dashed")+
  ylab("房价")+theme_bw()+
  theme(panel.grid=element_blank())+theme(axis.title.x=element_blank())+
  theme(axis.title=element_text(size=rel(1.25)),axis.text=element_text(size=rel(1.25)))
#p_5
save(p_5,file="C:/Users/Ding/Desktop/p_5.RData")

#################################
#### Year-on-Year Growth of Stock Price 
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_stock_yoy.RData")

da_crisis_stock_yoy <- merge(da_crisis,da_stock_yoy,by=c("country","ym"))
result_crisis_stock_yoy <- matrix(NA,nrow=4,ncol=10)
colnames(result_crisis_stock_yoy) <- c(paste0("pre_",5:1),paste0("post_",1:5))

#### Pre-Crisis
model_pre_5 <- plm(stock_yoy ~ pre_crisis_5,data=da_crisis_stock_yoy,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_5)
model_pre_4 <- plm(stock_yoy ~ pre_crisis_4,data=da_crisis_stock_yoy,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_4)
model_pre_3 <- plm(stock_yoy ~ pre_crisis_3,data=da_crisis_stock_yoy,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_3)
model_pre_2 <- plm(stock_yoy ~ pre_crisis_2,data=da_crisis_stock_yoy,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_2)
model_pre_1 <- plm(stock_yoy ~ pre_crisis_1,data=da_crisis_stock_yoy,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_1)

for (i in 1:5) {
  result_crisis_stock_yoy[,i] <- summary(get(paste0("model_pre_",6-i)))$coefficients[1,]
}

#### Post-Crisis
model_post_1 <- plm(stock_yoy ~ post_crisis_1,data=da_crisis_stock_yoy,
               model="within",effect="individual",index=c("country","ym"))
#summary(model_post_1)
model_post_2 <- plm(stock_yoy ~ post_crisis_2,data=da_crisis_stock_yoy,
               model="within",effect="individual",index=c("country","ym"))
#summary(model_post_2)
model_post_3 <- plm(stock_yoy ~ post_crisis_3,data=da_crisis_stock_yoy,
               model="within",effect="individual",index=c("country","ym"))
#summary(model_post_3)
model_post_4 <- plm(stock_yoy ~ post_crisis_4,data=da_crisis_stock_yoy,
               model="within",effect="individual",index=c("country","ym"))
#summary(model_post_4)
model_post_5 <- plm(stock_yoy ~ post_crisis_5,data=da_crisis_stock_yoy,
               model="within",effect="individual",index=c("country","ym"))
#summary(model_post_5)

for (i in 6:10) {
  result_crisis_stock_yoy[,i] <- summary(get(paste0("model_post_",i-5)))$coefficients[1,]
}
result_crisis_stock_yoy

data_plot <- data.table(Time=factor(c(paste0("前",5:1),paste0("后",1:5)),
                                    levels=c(paste0("前",5:1),paste0("后",1:5))),
                        stock_yoy=result_crisis_stock_yoy[1,]*100)

p <- ggplot(data_plot,aes(x=Time,y=stock_yoy,group=1))
p_6 <- p+geom_line()+geom_hline(aes(yintercept=0),col="grey",linetype="dashed")+
  ylab("股价")+theme_bw()+
  theme(panel.grid=element_blank())+theme(axis.title.x=element_blank())+
  theme(axis.title=element_text(size=rel(1.25)),axis.text=element_text(size=rel(1.25)))
#p_6
save(p_6,file="C:/Users/Ding/Desktop/p_6.RData")

#################################
#### Year-on-Year Growth of GDP 
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_gdp_yoy.RData")

da_crisis_gdp_yoy <- merge(da_crisis,da_gdp_yoy,by=c("country","ym"))
result_crisis_gdp_yoy <- matrix(NA,nrow=4,ncol=10)
colnames(result_crisis_gdp_yoy) <- c(paste0("pre_",5:1),paste0("post_",1:5))

#### Pre-Crisis
model_pre_5 <- plm(gdp_yoy ~ pre_crisis_5,data=da_crisis_gdp_yoy,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_5)
model_pre_4 <- plm(gdp_yoy ~ pre_crisis_4,data=da_crisis_gdp_yoy,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_4)
model_pre_3 <- plm(gdp_yoy ~ pre_crisis_3,data=da_crisis_gdp_yoy,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_3)
model_pre_2 <- plm(gdp_yoy ~ pre_crisis_2,data=da_crisis_gdp_yoy,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_2)
model_pre_1 <- plm(gdp_yoy ~ pre_crisis_1,data=da_crisis_gdp_yoy,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_1)

for (i in 1:5) {
  result_crisis_gdp_yoy[,i] <- summary(get(paste0("model_pre_",6-i)))$coefficients[1,]
}

#### Post-Crisis
model_post_1 <- plm(gdp_yoy ~ post_crisis_1,data=da_crisis_gdp_yoy,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_1)
model_post_2 <- plm(gdp_yoy ~ post_crisis_2,data=da_crisis_gdp_yoy,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_2)
model_post_3 <- plm(gdp_yoy ~ post_crisis_3,data=da_crisis_gdp_yoy,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_3)
model_post_4 <- plm(gdp_yoy ~ post_crisis_4,data=da_crisis_gdp_yoy,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_4)
model_post_5 <- plm(gdp_yoy ~ post_crisis_5,data=da_crisis_gdp_yoy,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_5)

for (i in 6:10) {
  result_crisis_gdp_yoy[,i] <- summary(get(paste0("model_post_",i-5)))$coefficients[1,]
}
result_crisis_gdp_yoy

data_plot <- data.table(Time=factor(c(paste0("前",5:1),paste0("后",1:5)),
                                    levels=c(paste0("前",5:1),paste0("后",1:5))),
                        gdp_yoy=result_crisis_gdp_yoy[1,]*100)

p <- ggplot(data_plot,aes(x=Time,y=gdp_yoy,group=1))
p_7 <- p+geom_line()+geom_hline(aes(yintercept=0),col="grey",linetype="dashed")+
  ylab("GDP")+theme_bw()+
  theme(panel.grid=element_blank())+theme(axis.title.x=element_blank())+
  theme(axis.title=element_text(size=rel(1.25)),axis.text=element_text(size=rel(1.25)))
#p_7
save(p_7,file="C:/Users/Ding/Desktop/p_7.RData")

#################################
for (i in 1:7) {
  load(paste0("F:/我的论文/第八篇/RData/p_",i,".RData"))
}

multiplot(p_1,p_2,p_3,p_4,p_5,p_6,p_7,layout=matrix(c(1:7,0),ncol=2,byrow=T))

