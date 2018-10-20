library(data.table)
library(lubridate)

#################################
da_crisis <- data.table()

FUN_COUNTRY <- function(country_name) {
  da_crisis_a <- data.table(ym=seq.Date(ymd("1975-3-1"),ymd("2017-12-1"),by="quarter"),
                            country=country_name,crisis=0,crisis_start=0)
  da_crisis <<- rbind(da_crisis,da_crisis_a) 
}

countries_names <- c("Australia","Austria","Belgium","Canada","Chile",
                   "Czech","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Iceland","Ireland",
                   "Israel","Italy","Japan","South Korea","Latvia",
                   "Luxembourg","Mexico","Netherlands","New Zealand",
                   "Norway","Poland","Portugal","Slovakia","Slovenia","Spain",
                   "Sweden","Switzerland","Turkey","United Kingdom","United States",
                   "Argentina","Brazil","Bulgaria","China","Colombia",
                   "Costa Rica","Cyprus","India","Indonesia","Lithuania",
                   "Malta","Romania","Russia","Saudi Arabia","South Africa")
for (i in countries_names) {
  FUN_COUNTRY(i)
}

#################################
FUN_CRISIS <- function(country_name,start_year,end_year) {
  da_crisis[country==country_name & ym>=ymd(paste0(as.character(start_year),"-1-1")) 
          & ym<=ymd(paste0(as.character(end_year),"-12-31")),crisis:=1]
  da_crisis[country==country_name & ym==ymd(paste0(as.character(start_year),"-3-1")),
            crisis_start:=1]
}

#### Mainly Based on Laeven and Valencia (2013) (LV in Short)
#### Appended and Adjusted Based on Reinhart and Rogoff (2009) (RR in Short)
# Note: If a crisis is pointed out by both LV and RR,
# the start time of this crisis is defined as the earlier one.

FUN_CRISIS("Argentina",1980,1982)
FUN_CRISIS("Argentina",1985,1985) # Reinhart
FUN_CRISIS("Argentina",1989,1991)
FUN_CRISIS("Argentina",1995,1995)
FUN_CRISIS("Argentina",2001,2003)
FUN_CRISIS("Australia",1989,1992) # Reinhart
FUN_CRISIS("Austria",2008,2011)
FUN_CRISIS("Belgium",2008,2011)
FUN_CRISIS("Brazil",1985,1985) # Reinhart
FUN_CRISIS("Brazil",1990,1994)
FUN_CRISIS("Brazil",1994,1998) # Laeven 1994; Reinhart 1995
FUN_CRISIS("Canada",1983,1985) # Reinhart
FUN_CRISIS("Chile",1976,1976) 
FUN_CRISIS("Chile",1980,1985) # Laeven 1981; Reinhart 1980
FUN_CRISIS("China",1992,1992) 
FUN_CRISIS("China",1997,1999) # Laeven 1998-1998; Reinhart 1997-1999
#FUN_CRISIS("China",2013,2013) # Chinese Banking Liquidity Crisis of 2013
FUN_CRISIS("Colombia",1982,1982)
FUN_CRISIS("Colombia",1998,2000)
FUN_CRISIS("Costa Rica",1987,1991)
FUN_CRISIS("Costa Rica",1994,1995)
FUN_CRISIS("Czech",1991,1991) # Reinhart
FUN_CRISIS("Czech",1996,2000)
FUN_CRISIS("Denmark",1987,1992)  # Reinhart
FUN_CRISIS("Denmark",2008,2011)
FUN_CRISIS("Estonia",1992,1994)
FUN_CRISIS("Finland",1991,1995)
FUN_CRISIS("France",1994,1995) # Reinhart
FUN_CRISIS("France",2008,2011)
FUN_CRISIS("Germany",1977,1977) # Reinhart
FUN_CRISIS("Germany",2008,2011)
FUN_CRISIS("Greece",1991,1995) # Reinhart
FUN_CRISIS("Greece",2008,2011)
FUN_CRISIS("Hungary",1991,1995)
FUN_CRISIS("Hungary",2008,2011)
FUN_CRISIS("Iceland",2007,2011) # Laeven 2008; Reinhart 2007
FUN_CRISIS("India",1993,1993)
FUN_CRISIS("Indonesia",1992,1992) # Reinhart
FUN_CRISIS("Indonesia",1997,2001)
FUN_CRISIS("Ireland",2008,2011)
FUN_CRISIS("Israel",1977,1977)
FUN_CRISIS("Italy",1990,1995) # Reinhart
FUN_CRISIS("Italy",2008,2011)
FUN_CRISIS("Japan",1992,1997) # Reinhart
FUN_CRISIS("Japan",1997,2001)
FUN_CRISIS("Latvia",1994,1996) # Laeven 1995; Reinhart 1994
FUN_CRISIS("Latvia",2008,2011)
FUN_CRISIS("Lithuania",1995,1996)
FUN_CRISIS("Luxembourg",2008,2011)
FUN_CRISIS("Mexico",1981,1985)
FUN_CRISIS("Mexico",1994,1996)
FUN_CRISIS("Netherlands",2008,2011)
FUN_CRISIS("New Zealand",1987,1990) # Reinhart
FUN_CRISIS("Norway",1987,1993) # Reinhart
FUN_CRISIS("Poland",1991,1994) # Laeven 1992; Reinhart 1991
FUN_CRISIS("Portugal",2008,2011)
FUN_CRISIS("Romania",1990,1992)
FUN_CRISIS("Russia",1995,1995) # Reinhart
FUN_CRISIS("Russia",1998,1998) 
FUN_CRISIS("Russia",2008,2011)
FUN_CRISIS("Slovakia",1991,1991) # Reinhart
FUN_CRISIS("Slovakia",1998,2002)
FUN_CRISIS("Slovenia",1992,1992) # Laeven 1992; Reinhart 1993
FUN_CRISIS("Slovenia",2008,2011)
FUN_CRISIS("South Africa",1977,1978) # Reinhart
FUN_CRISIS("South Africa",1989,1989) # Reinhart
FUN_CRISIS("South Korea",1986,1986) # Reinhart
FUN_CRISIS("South Korea",1997,1998)
FUN_CRISIS("Spain",1977,1981)
FUN_CRISIS("Spain",2008,2011)
FUN_CRISIS("Sweden",1991,1995)
FUN_CRISIS("Sweden",2008,2011)
FUN_CRISIS("Switzerland",2008,2011)
FUN_CRISIS("Turkey",1982,1984)
FUN_CRISIS("Turkey",1994,1994) # Reinhart
FUN_CRISIS("Turkey",2000,2001)
FUN_CRISIS("United Kingdom",1974,1976) # Reinhart
FUN_CRISIS("United Kingdom",1984,1984) # Reinhart
FUN_CRISIS("United Kingdom",1991,1991) # Reinhart
FUN_CRISIS("United Kingdom",1995,1995) # Reinhart
FUN_CRISIS("United Kingdom",2007,2011)
FUN_CRISIS("United States",1984,1984) # Reinhart
FUN_CRISIS("United States",1988,1988)
FUN_CRISIS("United States",2007,2011)

da_crisis[,`:=`(pre_crisis_1=0,pre_crisis_2=0,pre_crisis_3=0,
                pre_crisis_4=0,pre_crisis_5=0)]

da_crisis[,`:=`(post_crisis_1=0,post_crisis_2=0,post_crisis_3=0,
                post_crisis_4=0,post_crisis_5=0)]

now()
da_crisis_start <- da_crisis[crisis_start==1,ym,by=country]
for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_start[country==i,ym]) {
    da_crisis[country==i & ym<j & ym+years(1)>=j, pre_crisis_1:=1]
    da_crisis[country==i & ym+years(1)<j & ym+years(2)>=j, pre_crisis_2:=1]
    da_crisis[country==i & ym+years(2)<j & ym+years(3)>=j, pre_crisis_3:=1]
    da_crisis[country==i & ym+years(3)<j & ym+years(4)>=j, pre_crisis_4:=1]
    da_crisis[country==i & ym+years(4)<j & ym+years(5)>=j, pre_crisis_5:=1]
  }
}

da_crisis[crisis==1,`:=`(pre_crisis_1=0,pre_crisis_2=0,pre_crisis_3=0,
                         pre_crisis_4=0,pre_crisis_5=0)]
# deal with the cases that pre crisis period coincides with another crisis 

for (i in unique(da_crisis_start[,country])) {
  for (j in da_crisis_start[country==i,ym]) {
    da_crisis[country==i & ym-years(2)<j & ym-years(1)>=j, post_crisis_1:=1]
    da_crisis[country==i & ym-years(3)<j & ym-years(2)>=j, post_crisis_2:=1]
    da_crisis[country==i & ym-years(4)<j & ym-years(3)>=j, post_crisis_3:=1]
    da_crisis[country==i & ym-years(5)<j & ym-years(4)>=j, post_crisis_4:=1]
    da_crisis[country==i & ym-years(6)<j & ym-years(5)>=j, post_crisis_5:=1]
  }
}
now()

save(da_crisis,file="C:/Users/Ding/Desktop/da_crisis.RData")


