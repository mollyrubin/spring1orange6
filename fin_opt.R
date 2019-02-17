#----------------------------------#
#         Financial and Opt HW     #
#----------------------------------#

#################################################
#STEP 1: GATHER STOCK DATA FOR FULL DIJA PORTFOLIO
#################################################

if(!require('graphics'))install.packages('graphics')
if(!require('quantmod'))install.packages('quantmod')
if(!require('TTR'))install.packages('TTR')
if(!require('ks'))install.packages('ks')
if(!require('scales'))install.packages('scales')
if(!require('forecast'))install.packages('forecast')
if(!require('aTSA'))install.packages('aTSA')
if(!require('ccgarch'))install.packages('ccgarch')
if(!require('fGarch'))install.packages('fGarch')
if(!require('rugarch'))install.packages('rugarch')
if(!require('stringr'))install.packages('stringr')
if(!require('tidyverse'))install.packages('tidyverse')

library(graphics)
library(quantmod)
library(TTR)
library(ks)
library(scales)
library(forecast)
library(aTSA)
library(ccgarch)
library(fGarch)
library(rugarch)
library(stringr)
library(tidyverse)

# Load Stock Data 
tickers = c("MMM", "AXP", "AAPL", "BA", "CAT", 
            "CVX", "CSCO", "KO", "DIS", "DWDP",
            "XOM", "GS", "HD", "IBM", "INTC", 
            "JNJ", "JPM", "MCD", "MRK", "MSFT",
            "NKE", "PFE", "PG", "TRV", "UTX",
            "UNH", "VZ", "V", "WMT", "WBA", "DJI")

getSymbols(tickers)

nas <- rep(NA, length(MMM$MMM.Close) - length(V$V.Close))
new_v <- c(nas, V$V.Close)

stocks <- data.frame(
                MMM = MMM$MMM.Close,
                AXP = AXP$AXP.Close, 
                AAPL = AAPL$AAPL.Close,
                BA = BA$BA.Close, 
                CAT = CAT$CAT.Close,
                CVX = CVX$CVX.Close,
                CSCO = CSCO$CSCO.Close, 
                KO = KO$KO.Close,
                DIS = DIS$DIS.Close, 
                DWDP = DWDP$DWDP.Close,
                XOM = XOM$XOM.Close, 
                GS = GS$GS.Close, 
                HD = HD$HD.Close,
                IBM = IBM$IBM.Close, 
                INTC = INTC$INTC.Close,
                JNJ = JNJ$JNJ.Close, 
                JPM = JPM$JPM.Close, 
                MCD = MCD$MCD.Close,
                MRK = MRK$MRK.Close, 
                MSFT = MSFT$MSFT.Close,
                NKE = NKE$NKE.Close,
                PFE = PFE$PFE.Close, 
                PG = PG$PG.Close,
                TRV = TRV$TRV.Close, 
                UTX = UTX$UTX.Close,
                UNH = UNH$UNH.Close,
                VZ = VZ$VZ.Close, 
                WMT = WMT$WMT.Close, 
                WBA = WBA$WBA.Close, 
                V.Close = new_v)






#ROC = Rate of Change calculation, appending returns for each stock to main dataset

for(stock_name in names(stocks)){
  new_name <- str_split(stock_name, "[.]")[[1]][1]
  new_name <- paste0(new_name, "_r")
  
  stocks[new_name] <- ROC( stocks[, stock_name])
}

write.csv(stocks, file = "../../../fin_opt_project/stocks.csv")

#------------------------------------------------------------------------------------
# ranking stocks by most significant to be predicted by arch/garch models
#------------------------------------------------------------------------------------


arch_effects <- list()
resid_r2 <- numeric()
resid_test_stat <- numeric()
resid_p <- numeric()
stock_names <- character()
i = 1

for(stock_name in names(stocks)){
  
  if( str_sub( stock_name, nchar(stock_name) - 1) == "_r"){
    new_name <- str_sub(stock_name, 1, nchar(stock_name) - 2)
    print(new_name)
    stock_ts <- na.trim( as.vector( stocks[, stock_name] ) )
    ts <- 1:length(stock_ts)
    
    lin_mod <- lm(stock_ts ~ ts)
    
    squared_resids <- lin_mod$residuals ^ 2
    lag_resids <- squared_resids[-length(squared_resids)]
    squared_resids <- squared_resids[-1]
    
    resid_mod <- lm(squared_resids ~ lag_resids)
    
    r_square <- summary(resid_mod)$r.squared
    test_stat <- r_square * ( length( squared_resids ) )
    p_value <- pchisq(test_stat, 1, lower.tail = F)
    
    resid_r2[i] <- r_square
    resid_test_stat[i] <- test_stat
    resid_p[i] <- p_value
    stock_names[i] <- new_name
    
    arch_effects[[new_name]] <- arch.test( arima(stock_ts[-1], order = c(0,0,0)), output = F )
    i = i + 1
  }
 
}

arch_effects_df <- data.frame(
  stock = stock_names,
  resid_r2 = resid_r2,
  test_stat = resid_test_stat,
  p_value = resid_p
)

arch_effects_df <- arch_effects_df %>% arrange(desc(test_stat))

