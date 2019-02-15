#----------------------------------#
#         Financial and Opt HW     #
#----------------------------------#

#################################################
#STEP 1: GATHER STOCK DATA FOR FULL DIJA PORTFOLIO
#################################################

install.packages('graphics')
install.packages('quantmod')
install.packages('TTR')
install.packages('ks')
install.packages('scales')
install.packages('forecast')
install.packages('aTSA')
install.packages('ccgarch')
install.packages('fGarch')
install.packages('rugarch')

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

# Load Stock Data 
tickers = c("MMM", "AXP", "AAPL", "BA", "CAT", 
            "CVX", "CSCO", "KO", "DIS", "DWDP",
            "XOM", "GS", "HD", "IBM", "INTC", 
            "JNJ", "JPM", "MCD", "MRK", "MSFT",
            "NKE", "PFE", "PG", "TRV", "UTX",
            "UNH", "VZ", "V", "WMT", "WBA", "DJI")

getSymbols(tickers)

View(V)

stocks <- cbind(MMM[,4] ,AXP[,4], AAPL[,4] ,BA[,4], CAT[,4],
                CVX[,4] ,CSCO[,4], KO[,4] ,DIS[,4], DWDP[,4],
                XOM[,4] ,GS[,4], HD[,4] ,IBM[,4], INTC[,4],
                JNJ[,4] ,JPM[,4], MCD[,4] ,MRK[,4], MSFT[,4],
                NKE[,4] ,PFE[,4], PG[,4] ,TRV[,4], UTX[,4],
                UNH[,4] ,VZ[,4], V[,4] ,WMT[,4], WBA[,4])

View(stocks)


?ROC
#ROC = Rate of Change calculation, appending returns for each stock to main dataset

stocks$MMM_r <- ROC(stocks$MMM.Close)
stocks$AXP_r <- ROC(stocks$AXP.Close)
stocks$AAPL_r <- ROC(stocks$AAPL.Close)
stocks$BA_r <- ROC(stocks$BA.Close)
stocks$CAT_r <- ROC(stocks$CAT.Close)
stocks$CVX_r <- ROC(stocks$CVX.Close)
stocks$CSCO_r <- ROC(stocks$CSCO.Close)
stocks$KO_r <- ROC(stocks$KO.Close)
stocks$DIS_r <- ROC(stocks$DIS.Close)
stocks$DWDP_r <- ROC(stocks$DWDP.Close)
stocks$XOM_r <- ROC(stocks$XOM.Close)
stocks$GS_r <- ROC(stocks$GS.Close)
stocks$HD_r <- ROC(stocks$HD.Close)
stocks$IBM_r <- ROC(stocks$IBM.Close)
stocks$INTC_r <- ROC(stocks$INTC.Close)
stocks$JNJ_r <- ROC(stocks$JNJ.Close)
stocks$JPM_r <- ROC(stocks$JPM.Close)
stocks$MCD_r <- ROC(stocks$MCD.Close)
stocks$MRK_r <- ROC(stocks$MRK.Close)
stocks$MSFT_r <- ROC(stocks$MSFT.Close)
stocks$NKE_r <- ROC(stocks$NKE.Close)
stocks$PFE_r <- ROC(stocks$PFE.Close)
stocks$PG_r <- ROC(stocks$PG.Close)
stocks$TRV_r <- ROC(stocks$TRV.Close)
stocks$UTX_r <- ROC(stocks$UTX.Close)
stocks$UNH_r <- ROC(stocks$UNH.Close)
stocks$VZ_r <- ROC(stocks$VZ.Close)
stocks$V_r <- ROC(stocks$V.Close)
stocks$WMT_r <- ROC(stocks$WMT.Close)
stocks$WBA_r <- ROC(stocks$WBA.Close)

write.zoo(stocks, file = "C:\\Users\\molly\\OneDrive\\Documents\\_MSA 2018 COURSEWORK\\Financial\\stocks.csv", sep=",")

#filter to just feb 1 17 - feb 8 19



