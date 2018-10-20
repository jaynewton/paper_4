library(data.table)
library(lubridate)
library(plm)
library(ggplot2)

sink(file="C:/Users/Ding/Desktop/主代码.txt",append=T)
sink()
rm(list=ls())

#################################
#### Credit to Non-Financial Sector
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_non_finance_yoy_a.RData")

da_crisis_non_finance <- merge(da_crisis,da_non_finance_yoy_a,by=c("country","ym"))
result_crisis_non_finance <- matrix(NA,nrow=3,ncol=10)
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
  result_crisis_non_finance[,i] <- summary(get(paste0("model_pre_",6-i)))$coefficients[1,c(1,2,4)]
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
  result_crisis_non_finance[,i] <- summary(get(paste0("model_post_",i-5)))$coefficients[1,c(1,2,4)]
}
result_crisis_non_finance

data_plot <- data.table(Time=factor(colnames(result_crisis_non_finance),
                                    levels=colnames(result_crisis_non_finance)),
                        non_finance=result_crisis_non_finance[1,])

p <- ggplot(data_plot, aes(x=Time,y=non_finance,group=1)) 
p+theme(axis.title=element_text(size=rel(2)),
        axis.text=element_text(size=rel(1.5)))+
  geom_line()
ggsave("C:/Users/Ding/Desktop/non_finance_a.png",width=1.618*20,height=20,units="cm",dpi=500)

#################################
#### Credit to Private Non-Financial Sector
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_private_yoy_a.RData")

da_crisis_private <- merge(da_crisis,da_private_yoy_a,by=c("country","ym"))
result_crisis_private <- matrix(NA,nrow=3,ncol=10)
colnames(result_crisis_private) <- c(paste0("pre_",5:1),paste0("post_",1:5))

#### Pre-Crisis
model_pre_5 <- plm(private_yoy ~ pre_crisis_5,data=da_crisis_private,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_5)
model_pre_4 <- plm(private_yoy ~ pre_crisis_4,data=da_crisis_private,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_4)
model_pre_3 <- plm(private_yoy ~ pre_crisis_3,data=da_crisis_private,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_3)
model_pre_2 <- plm(private_yoy ~ pre_crisis_2,data=da_crisis_private,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_2)
model_pre_1 <- plm(private_yoy ~ pre_crisis_1,data=da_crisis_private,
                   model="within",effect="individual",index=c("country","ym"))
#summary(model_pre_1)

for (i in 1:5) {
  result_crisis_private[,i] <- summary(get(paste0("model_pre_",6-i)))$coefficients[1,c(1,2,4)]
}

#### Post-Crisis
model_post_1 <- plm(private_yoy ~ post_crisis_1,data=da_crisis_private,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_1)
model_post_2 <- plm(private_yoy ~ post_crisis_2,data=da_crisis_private,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_2)
model_post_3 <- plm(private_yoy ~ post_crisis_3,data=da_crisis_private,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_3)
model_post_4 <- plm(private_yoy ~ post_crisis_4,data=da_crisis_private,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_4)
model_post_5 <- plm(private_yoy ~ post_crisis_5,data=da_crisis_private,
                    model="within",effect="individual",index=c("country","ym"))
#summary(model_post_5)

for (i in 6:10) {
  result_crisis_private[,i] <- summary(get(paste0("model_post_",i-5)))$coefficients[1,c(1,2,4)]
}
result_crisis_private

data_plot <- data.table(Time=factor(colnames(result_crisis_private),
                                    levels=colnames(result_crisis_private)),
                        private=result_crisis_private[1,])

p <- ggplot(data_plot, aes(x=Time,y=private,group=1)) 
p+theme(axis.title=element_text(size=rel(2)),
        axis.text=element_text(size=rel(1.5)))+
  geom_line()
ggsave("C:/Users/Ding/Desktop/private_non_finance_a.png",width=1.618*20,height=20,units="cm",dpi=500)

#################################
#### Credit to Households
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_household_yoy_a.RData")

da_crisis_household <- merge(da_crisis,da_household_yoy_a,by=c("country","ym"))
result_crisis_household <- matrix(NA,nrow=3,ncol=10)
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
  result_crisis_household[,i] <- summary(get(paste0("model_pre_",6-i)))$coefficients[1,c(1,2,4)]
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
  result_crisis_household[,i] <- summary(get(paste0("model_post_",i-5)))$coefficients[1,c(1,2,4)]
}
result_crisis_household

data_plot <- data.table(Time=factor(colnames(result_crisis_household),
                                    levels=colnames(result_crisis_household)),
                        household=result_crisis_household[1,])

p <- ggplot(data_plot, aes(x=Time,y=household,group=1)) 
p+theme(axis.title=element_text(size=rel(2)),
        axis.text=element_text(size=rel(1.5)))+
  geom_line()
ggsave("C:/Users/Ding/Desktop/households_a.png",width=1.618*20,height=20,units="cm",dpi=500)

#################################
#### Credit to Non-Financial Corporations
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_corporation_yoy_a.RData")

da_crisis_corporation <- merge(da_crisis,da_corporation_yoy_a,by=c("country","ym"))
result_crisis_corporation <- matrix(NA,nrow=3,ncol=10)
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
  result_crisis_corporation[,i] <- summary(get(paste0("model_pre_",6-i)))$coefficients[1,c(1,2,4)]
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
  result_crisis_corporation[,i] <- summary(get(paste0("model_post_",i-5)))$coefficients[1,c(1,2,4)]
}
result_crisis_corporation

data_plot <- data.table(Time=factor(colnames(result_crisis_corporation),
                                    levels=colnames(result_crisis_corporation)),
                        corporation=result_crisis_corporation[1,])

p <- ggplot(data_plot, aes(x=Time,y=corporation,group=1)) 
p+theme(axis.title=element_text(size=rel(2)),
        axis.text=element_text(size=rel(1.5)))+
  geom_line()
ggsave("C:/Users/Ding/Desktop/corporations_a.png",width=1.618*20,height=20,units="cm",dpi=500)

#################################
#### Credit to General Government
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_government_yoy_a.RData")

da_crisis_government <- merge(da_crisis,da_government_yoy_a,by=c("country","ym"))
result_crisis_government <- matrix(NA,nrow=3,ncol=10)
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
  result_crisis_government[,i] <- summary(get(paste0("model_pre_",6-i)))$coefficients[1,c(1,2,4)]
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
  result_crisis_government[,i] <- summary(get(paste0("model_post_",i-5)))$coefficients[1,c(1,2,4)]
}
result_crisis_government

data_plot <- data.table(Time=factor(colnames(result_crisis_government),
                                    levels=colnames(result_crisis_government)),
                        government=result_crisis_government[1,])

p <- ggplot(data_plot, aes(x=Time,y=government,group=1)) 
p+theme(axis.title=element_text(size=rel(2)),
        axis.text=element_text(size=rel(1.5)))+
  geom_line()
ggsave("C:/Users/Ding/Desktop/government_a.png",width=1.618*20,height=20,units="cm",dpi=500)
