library(data.table)
library(lubridate)
library(rmgarch)

#################################
load("F:/我的论文/第八篇/RData/da_ret_m.RData")
load("F:/我的论文/第八篇/RData/da_mv_m.RData")

da_all_m <- merge(da_ret_m,da_mv_m,by=c("country","bank","ym"))

selected_banks <- da_all_m[,.N,by=bank][N>=100,bank]
da_all_m <- da_all_m[bank %in% selected_banks,]

da_all_m[,length(unique(bank))]
nrow(da_all_m[,unique(bank),by=country])
# Note: The code above shows no two or more banks share the same bank name.

# The banks below fails in the CoVaR computation.
deleted_banks <- c("BANK.EST.PARA.ON","SEB.HYPBK.GSH..6.85..31.12.2011","T.BANK",
                   "TT.HELLENIC.POSTBANK","I.N.G.VYSYA.BANK.SUSP...SUSP.15.04.15",
                   "BANK.PETROCOMMERCE","VOLGA.KREDIT.BNK","DEVIN.BANKA",
                   "BANK.SARASIN...CIE..OTC.","WESBANCO")
                                       
da_all_m <- da_all_m[!bank %in% deleted_banks,]

da_system_m <- da_all_m[,.(ret_system=weighted.mean(ret,size)),keyby=.(country,ym)]
da_all_m <- merge(da_all_m,da_system_m,by=c("country","ym"))[order(country,bank,ym)]

####
garch.spec_i <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                           variance.model = list(garchOrder=c(1,1),model="sGARCH"),
                           distribution.model = "norm")
# i denotes individual bank.
garch.spec_system <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                                variance.model = list(garchOrder=c(1,1),model="sGARCH"),
                                distribution.model = "norm")
adcc.garch.spec <- dccspec(uspec=multispec(c(garch.spec_i,garch.spec_system)),dccOrder=c(1,1),
                           model="aDCC",distribution="mvnorm")

FUN_COVAR <- function(da) {
  garch.fit_i <- ugarchfit(spec=garch.spec_i,data=da[,ret])
  garch.fit_system <- ugarchfit(spec=garch.spec_system,data=da[,ret_system])
  adcc.fit <- dccfit(adcc.garch.spec,data=da[,.(ret,ret_system)])
  corr <- unlist(adcc.fit@mfit$R)[(1:nrow(da))*4-2]
  sigma_system <- garch.fit_system@fit$sigma
  covar <- qnorm(.975)*corr*sigma_system
  return(covar)
}

now()
da_all_m[,covar:=FUN_COVAR(.SD),by=bank]
now()

da_all_m <- da_all_m[ym>=ymd("1970-1-1"),]
da_covar_m <- da_all_m[,.(covar=weighted.mean(covar,size)),keyby=.(country,ym)]

save(da_covar_m,file="C:/Users/Ding/Desktop/da_covar_m.RData")

#################################
load("F:/我的论文/第八篇/RData/da_covar_m.RData")
da_covar_m[,m:=month(ym)]
da_covar <- da_covar_m[m %in% c(3,6,9,12),.(country,ym,covar)] # quarterly

save(da_covar,file="C:/Users/Ding/Desktop/da_covar.RData")
