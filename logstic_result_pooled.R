library(data.table)
library(lubridate)
library(plm)
library(lmtest) # coeftest

# Note: the first figure is the intercept

#################################
#### da_crisis_1

# pre_crisis_1_3 ~ non_finance_yoy
c(-2.390103,3.205918)

#################################
#### da_crisis_2

# pre_crisis_1_3 ~ household_yoy+corporation_yoy+government_yoy
c(-2.36873,2.701484,3.633298,-2.308914)

#################################
#### da_crisis_3

# pre_crisis_1_3 ~ house_yoy+stock_yoy
c(-2.337823,4.778926,1.179061)

#################################
#### da_crisis_4

#pre_crisis_1_3 ~ household_yoy+house_yoy+stock_yoy
c(-2.433318,2.473167,4.613779,.9723113)

#pre_crisis_1_3 ~ household_yoy+house_yoy+stock_yoy+
#                 I(house_yoy*household_yoy)+ I(stock_yoy*household_yoy)
c(-2.410897,1.443309,2.568718,1.249843,24.00875,-2.229711)

#################################
#### da_crisis_5

pre_crisis_1_3 ~ household_yoy+bubble_house+bubble_stock
c(-2.874582,2.474134,1.494163,1.050304)

pre_crisis_1_3 ~ household_yoy+bubble_house+bubble_stock+
  I(bubble_house*household_yoy)+ I(bubble_stock*household_yoy)
c(-2.844035,2.227837,1.06558,1.219235,4.942098,-2.230255)

#################################
#### da_crisis_6

pre_crisis_1_3 ~ household_yoy+bubble_house+bubble_stock
c(-2.787743,5.733255,.2339731,.9463796)

pre_crisis_1_3 ~ household_yoy+bubble_house+bubble_stock+
  I(bubble_house*household_yoy)+ I(bubble_stock*household_yoy)
c(-2.51071,1.079054,-.175009,.6268682,5.703383,4.129306)

#################################
coef_logit <- list(logit_1=c(-2.390103,3.205918),
                   logit_2=c(-2.36873,2.701484,3.633298,-2.308914),
                   logit_3=c(-2.337823,4.778926,1.179061),
                   logit_4_1=c(-2.433318,2.473167,4.613779,.9723113),
                   logit_4_2=c(-2.410897,1.443309,2.568718,1.249843,24.00875,-2.229711),
                   logit_5_1=c(-2.874582,2.474134,1.494163,1.050304),
                   logit_5_2=c(-2.844035,2.227837,1.06558,1.219235,4.942098,-2.230255),
                   logit_6_1=c(-2.787743,5.733255,.2339731,.9463796),
                   logit_6_2=c(-2.51071,1.079054,-.175009,.6268682,5.703383,4.129306))

save(coef_logit,file="C:/Users/Ding/Desktop/coef_logit.RData")

