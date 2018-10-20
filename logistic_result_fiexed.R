library(data.table)
library(lubridate)
library(plm)
library(lmtest) # coeftest

#################################
#### da_crisis_1

# pre_crisis_1_3 ~ non_finance_yoy
4.532384

#################################
#### da_crisis_2

# pre_crisis_1_3 ~ household_yoy+corporation_yoy+government_yoy
c(12.79256,3.612352,-3.137679)

#################################
#### da_crisis_3

# pre_crisis_1_3 ~ house_yoy+stock_yoy
c(5.103003,1.371745)

#################################
#### da_crisis_4

#pre_crisis_1_3 ~ household_yoy+house_yoy+stock_yoy
c(7.490501,3.817845,1.166388)

#pre_crisis_1_3 ~ household_yoy+house_yoy+stock_yoy+
#                 I(house_yoy*household_yoy)+ I(stock_yoy*household_yoy)
c(5.990332,.0122448,1.185255,51.61083,1.231692)

#################################
#### da_crisis_5

pre_crisis_1_3 ~ household_yoy+bubble_house+bubble_stock
c(7.030591,1.556423,.995501)

pre_crisis_1_3 ~ household_yoy+bubble_house+bubble_stock+
                 I(bubble_house*household_yoy)+ I(bubble_stock*household_yoy)
c(5.797956,.6932572,.9595529,10.89094,-.7640199)

#################################
#### da_crisis_6

pre_crisis_1_3 ~ household_yoy+bubble_house+bubble_stock
c(9.609429,.3915763,.8632316)

pre_crisis_1_3 ~ household_yoy+bubble_house+bubble_stock+
  I(bubble_house*household_yoy)+ I(bubble_stock*household_yoy)
c(1.599905,-.4293577,.1590879,13.19817,8.59791)


