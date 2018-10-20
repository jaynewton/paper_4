#################################
load("F:/我的论文/第八篇/RData/da_crisis.RData")
load("F:/我的论文/第八篇/RData/da_household_yoy.RData")
load("F:/我的论文/第八篇/RData/da_bubble_house_bs.RData")
load("F:/我的论文/第八篇/RData/da_bubble_stock_bs.RData")

#da_crisis <- da_crisis[country=="United States",]

da_crisis <- merge(da_crisis,da_household_yoy,by=c("country","ym"))
da_crisis <- merge(da_crisis,da_bubble_house_bs,by=c("country","ym"))
da_crisis <- merge(da_crisis,da_bubble_stock_bs,by=c("country","ym"))

da_crisis[,pre_crisis_1_3:=pre_crisis_1+pre_crisis_2+pre_crisis_3]

# whether or not there is house or stock bubble in the three years before bank crisis
da_crisis[,crisis_bubble_house:=pre_crisis_1_3*bubble_house]
da_crisis[,crisis_bubble_stock:=pre_crisis_1_3*bubble_stock]

# whether or not there is credit fueled house or stock bubble in the three years before bank crisis
da_crisis[,crisis_bubble_house_credit:=pre_crisis_1_3*bubble_house*boom_household]
da_crisis[,crisis_bubble_stock_credit:=pre_crisis_1_3*bubble_stock*boom_household]

da_crisis_a <- da_crisis[,.(country,ym,crisis_start)]
# a denotes alternative 
da_crisis_a[,`:=`(crisis_bubble_house=0,crisis_bubble_stock=0,
                  crisis_bubble_house_credit=0,crisis_bubble_stock_credit=0)]
da_crisis_start <- da_crisis[crisis_start==1,ym,by=country]

# If there is a bubble in the three years before bank crisis,
# we mark it in the start year of the bank crisis.
for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_start[country==i,ym]) {
    da_crisis_a[country==i & ym==j,crisis_bubble_house:=
                  ifelse(da_crisis[country==i & ym<j & ym+years(3)>=j,sum(crisis_bubble_house)]>=1,1,0)]
    da_crisis_a[country==i & ym==j,crisis_bubble_stock:=
                  ifelse(da_crisis[country==i & ym<j & ym+years(3)>=j,sum(crisis_bubble_stock)]>=1,1,0)]
    da_crisis_a[country==i & ym==j,crisis_bubble_house_credit:=
                  ifelse(da_crisis[country==i & ym<j & ym+years(3)>=j,sum(crisis_bubble_house_credit)]>=1,1,0)]
    da_crisis_a[country==i & ym==j,crisis_bubble_stock_credit:=
                  ifelse(da_crisis[country==i & ym<j & ym+years(3)>=j,sum(crisis_bubble_stock_credit)]>=1,1,0)]
    
  }
}

save(da_crisis_a,file="C:/Users/Ding/Desktop/da_crisis_bs.RData")

