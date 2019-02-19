#----------------------------------#
#         Financial and Opt HW     #
#----------------------------------#

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
if(!require('quadprog'))install.packages('quadprog')

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
library(quadprog)

#************************************************************************
# Part 1
#************************************************************************

#################################################
#STEP 1: GATHER STOCK DATA FOR FULL DIJA PORTFOLIO
#################################################

# Load Stock Data 
tickers = c("MMM", "AXP", "AAPL", "BA", "CAT", 
            "CVX", "CSCO", "KO", "DIS", "DWDP",
            "XOM", "GS", "HD", "IBM", "INTC", 
            "JNJ", "JPM", "MCD", "MRK", "MSFT",
            "NKE", "PFE", "PG", "TRV", "UTX",
            "UNH", "VZ", "V", "WMT", "WBA", "DJI")

getSymbols(tickers)

# replace missing values for V with NA
nas <- rep(NA, length(MMM$MMM.Close) - length(V$V.Close))
new_v <- c(nas, V$V.Close)

# create dataframe of all stocks
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

# looping through each stock
for(stock_name in names(stocks)){
  # getting rid of .close in stock name
  new_name <- str_split(stock_name, "[.]")[[1]][1]
  # adding _r to represent a return
  new_name <- paste0(new_name, "_r")
  # calculating return and adding returns to dataframe
  stocks[new_name] <- ROC( stocks[, stock_name])
}

# write stock values and returns to csv
write.csv(stocks, file = "../../../fin_opt_project/stocks.csv")

#####################################################
#STEP 2: Rank stocks by most significant arch effects
#####################################################

# creating empty list and vectors to add in test results
arch_effects <- list()
resid_r2 <- numeric()
resid_test_stat <- numeric()
resid_p <- numeric()
stock_names <- character()

# loop through all columns in the dataframe
i <- 1
for(stock_name in names(stocks)){
  # only test for arch effects on the returns
  if( str_sub( stock_name, nchar(stock_name) - 1) == "_r"){
    # create new name that gets rid of _r
    new_name <- str_sub(stock_name, 1, nchar(stock_name) - 2)
    # get rid of leading and trailing NA's
    stock_ts <- na.trim( as.vector( stocks[, stock_name] ) )
    # create a time period variable
    ts <- 1:length(stock_ts)
    # OLS model for the returns over time
    lin_mod <- lm(stock_ts ~ ts)
    # squaring residuals from the model
    squared_resids <- lin_mod$residuals ^ 2
    # lag the residuals one period
    lag_resids <- squared_resids[-length(squared_resids)]
    squared_resids <- squared_resids[-1]
    # modeling residuals with 1 lag residuals as requested 
    resid_mod <- lm(squared_resids ~ lag_resids)
    # getting R squared from the lagged residuals model
    r_square <- summary(resid_mod)$r.squared
    # calculating LM test stat
    test_stat <- r_square * ( length( squared_resids ) )
    # getting p-value of LM test stat from chi sqaured distribution
    p_value <- pchisq(test_stat, 1, lower.tail = F)
    # adding test stat and p-values to vectors
    resid_r2[i] <- r_square
    resid_test_stat[i] <- test_stat
    resid_p[i] <- p_value
    stock_names[i] <- new_name
    # adding arch effects result to list
    arch_effects[[new_name]] <- arch.test( arima(stock_ts[-1], order = c(0,0,0)), output = F )
    i = i + 1
  }
 
}

# creating dataframe of the arch effects
arch_effects_df <- data.frame(
  stock = stock_names,
  resid_r2 = resid_r2,
  test_stat = resid_test_stat,
  p_value = resid_p
)
# sorting in descending order by LM test statistic
arch_effects_df <- arch_effects_df %>% arrange(desc(test_stat))
# getting the top 5 most significant
top_5_stocks <- as.character(arch_effects_df$stock[1:5])
# creating vector to index stocks df for only returns
# true if _r false if not
returns <- vector(length = ncol(stocks))
index <- 1
for(stock_name in names(stocks)){
  returns[index] <- str_sub( stock_name, nchar(stock_name) - 1) == "_r"
  index <- index + 1
}
# subsetting stocks df to only get returns
stock_returns <- stocks[, returns]
# creating vector to index stock_returns for only top 5 most significant stocks
# true if top 5 false if not
top_5 <- vector(length = ncol(stock_returns))
index <- 1
for(stock_name in names(stock_returns)){
  current_name <- str_sub(stock_name, 1, nchar(stock_name) - 2)
  top_5[index] <- current_name %in% top_5_stocks
  index <- index + 1
}
# subsetting stocks df to only have top 5 most significant stocks
top_5_returns <- stock_returns[, top_5]

#####################################################
#STEP 3: Model top 5 stocks and pic best model by AIC
#####################################################

# creating empty dataframe to include the AIC of the 4 possible models for all 5 stocks
model_evalutation <- data.frame(
  stock_name = top_5_stocks,
  garch_norm_aic = numeric( length = length(top_5_stocks)),
  garch_t_aic = numeric(length = length(top_5_stocks)),
  q_garch_norm_aic = numeric(length = length(top_5_stocks)),
  q_garch_t_aic = numeric(length = length(top_5_stocks)),
  jb_test_p = numeric(length = length(top_5_stocks)),
  stringsAsFactors = F
)
# looping through all 5 stocks
for(stock in names(top_5_returns) ){
  # getting rid of _r
  name <- str_sub(stock, 1, nchar(stock) - 2)
  # formal test for normality
  jb_test_temp <- jb.test( top_5_returns[-1, stock])
  # normal garch model fit
  garch_norm <- garchFit(formula = ~ garch(1,1), data=top_5_returns[-1, stock],
                         cond.dist = "norm", include.mean = FALSE)
  # t garch model fit
  garch_t <- garchFit(formula = ~ garch(1,1), data=top_5_returns[-1, stock],
                      cond.dist = "std", include.mean = FALSE)
  # skewed normal garch model fit
  q_garch_norm <- garchFit(formula= ~ garch(1,1), data=top_5_returns[-1, stock], 
                      cond.dist="snorm", include.mean = FALSE)
  # skewed t garch model fit
  q_garch_t <- garchFit(formula= ~ garch(1,1), data=top_5_returns[-1, stock], 
                             cond.dist="sstd", include.mean = FALSE)
  # adding AIC & jb.test p-value to the model_evaluation df
  model_evalutation[ model_evalutation$stock_name == name, "garch_norm_aic"] <-  garch_norm@fit$ics[1]
  model_evalutation[ model_evalutation$stock_name == name, "garch_t_aic"] <-  garch_t@fit$ics[1]
  model_evalutation[ model_evalutation$stock_name == name, "q_garch_norm_aic"] <-  q_garch_norm@fit$ics[1]
  model_evalutation[ model_evalutation$stock_name == name, "q_garch_t_aic"] <-  q_garch_t@fit$ics[1]
  model_evalutation[ model_evalutation$stock_name == name, "jb_test_p"] <- jb_test_temp[2]
}


#####################################################
#STEP 3: Forecast next 5 days of volatility
#####################################################

# determining best model for each stock
possible_models <- names(model_evalutation)[2:5]

for(i in 1:nrow(model_evalutation)){
  model_scores <- model_evalutation[i, 2:5]
  best_mod_index <- min(model_scores) == model_scores
  best_model <- possible_models[best_mod_index]
  model_evalutation[i, "best_model"] <- str_sub(best_model, 1, nchar(best_model) - 4) 
}

model_predictions <- data.frame(
  date = 1:5
)

model_parameters <- data.frame(
  stock_name = character( length(top_5_stocks)),
  omega = numeric( length(top_5_stocks)),
  alpha1 = numeric( length(top_5_stocks)),
  beta1 = numeric( length(top_5_stocks)),
  stringsAsFactors = F
)

for(i in 1:nrow(model_evalutation)){
  current_stock <- as.character(model_evalutation[i, "stock_name"])
  current_stock_r <- paste0(current_stock, "_r")
  best_model <- model_evalutation[i, "best_model"]
  if(best_model == "garch_norm"){
    model_dist <- "norm"
  } else if(best_model == "garch_t"){
    model_dist <- "std"
  } else if(best_model == "q_garch_norm"){
    model_dist <- "snorm"
  } else if(best_model == "q_garch_t"){
    model_dist <- "sstd"
  } else {
    model_dist <- "norm"
    print("some error occured")
  }
  
  current_stock_pred <- paste0(current_stock, "_preds")
  print(current_stock_pred)
  
  model_fit <- garchFit(formula = ~ garch(1,1), data=top_5_returns[-1, current_stock_r],
                        cond.dist = model_dist, include.mean = FALSE)
  
  preds <- predict(model_fit, n.ahead = 5)
  
  model_predictions[, current_stock_pred] <- preds$standardDeviation ^ 2
  
  model_parameters[i, "stock_name" ] <- current_stock
  model_parameters[i, "omega"] <- model_fit@fit$coef[1]
  model_parameters[i, "alpha1"] <- model_fit@fit$coef[2]
  model_parameters[i, "beta1"] <- model_fit@fit$coef[3]
}

# sorting by largest shock

sorted_alphas <- model_parameters %>% arrange(desc(alpha1))

# sorting by longest shock

sorted_betas <- model_parameters %>% arrange(desc(beta1))

# looking at the stocks with top 5 arch effects
View(arch_effects_df)
# looking at model evaluation stats
View(model_evalutation)
# model parameters sorted by alpha
View(sorted_alphas)
# model paramters sorted by beta
View(sorted_betas)

#************************************************************************
# Part 2
#************************************************************************

#################################################
#STEP 1: Portfolio Optimization
#################################################

# removing first row of na's
top_5_returns1 <- top_5_returns[-1,]
# getting rid of date from model predictions df
variance_preds <- model_predictions[, -1]

historical_median_returns <- sapply(top_5_returns1, median) 

predicted_variance <- sapply(variance_preds, median) 

historical_cov <- cov(top_5_returns1)

# replacing historical variance with predicted variance
cov_matrix <- historical_cov
diag(cov_matrix) <- predicted_variance


#-----------------------------------------------------------
# using LaBarr's Code
#-----------------------------------------------------------

f <- function(x) x[1]*cov_matrix[1,1]*x[1] + x[1]*cov_matrix[1,2]*x[2] + x[1]*cov_matrix[1,3]*x[3] + x[1]*cov_matrix[1,4]*x[4] + x[1]*cov_matrix[1,5]*x[5] + 
  x[2]*cov_matrix[2,1]*x[1] + x[2]*cov_matrix[2,2]*x[2] + x[2]*cov_matrix[2,3]*x[3] + x[2]*cov_matrix[2,4]*x[4] + x[2]*cov_matrix[2,5]*x[5] + 
  x[3]*cov_matrix[3,1]*x[1] + x[3]*cov_matrix[3,2]*x[2] + x[3]*cov_matrix[3,3]*x[3] + x[3]*cov_matrix[3,4]*x[4] + x[3]*cov_matrix[3,5]*x[5] +
  x[4]*cov_matrix[4,1]*x[1] + x[4]*cov_matrix[4,2]*x[2] + x[4]*cov_matrix[4,3]*x[3] + x[4]*cov_matrix[4,4]*x[4] + x[4]*cov_matrix[4,5]*x[5] +
  x[5]*cov_matrix[5,1]*x[1] + x[5]*cov_matrix[5,2]*x[2] + x[5]*cov_matrix[5,3]*x[3] + x[5]*cov_matrix[5,4]*x[4] + x[5]*cov_matrix[5,5]*x[5]


theta <- c(0.96,0.01,0.01,0.01,0.005)

ui <- rbind(c(1,0,0,0,0),
            c(0,1,0,0,0),
            c(0,0,1,0,0),
            c(0,0,0,1,0),
            c(0,0,0,0,1),
            c(-1,-1,-1,-1,-1),
            c(1,1,1,1,1),
            c(historical_median_returns))
ci <- c(0,
        0,
        0,
        0,
        0,
        -1,
        0.99,
        0.0005) # 5.04% Annual Return Spread to Daily #


port_opt <- constrOptim(theta = theta, f = f, ui = ui, ci = ci, grad = NULL)

port_weights_h <- port_opt$par
port_var_h <- port_opt$value
names(port_weights_h) <- names(historical_median_returns)
final_h <- round(port_weights_h*100,2)

#-----------------------------------------------------------
# using Simmon's Code
#-----------------------------------------------------------

mean.vec <- historical_median_returns
#cov.vec <- cov_matrix
cov.vec <- cov(top_5_returns1)
Dmat <- 2*cov.vec
dvec <- rep(0,5)
Amat <- t(matrix(c(1,1,1,1,1,mean.vec),nrow=2,byrow=T))
bvec <- c(1,0.0005)
meq <- 1
ln.model <- solve.QP(Dmat,dvec,Amat,bvec,meq)
ln.names <- names(historical_median_returns)

names(ln.model$solution)=ln.names
ln.model$solution
ln.model$value


ln.model$solution
ln.model$value


################################ 
#Efficient Frontier
################################


param=seq(0.0001,0.0007, by=0.000001)
eff.front.weight=matrix(nrow=length(param),ncol=length(mean.vec))
eff.front.return=vector(length=length(param))
eff.front.risk=param
for (i in 1:length(param)){
  bvec=c(1,param[i])
  ln.model=solve.QP(Dmat,dvec,Amat,bvec,meq)
  eff.front.return[i]=sum(ln.model$solution*mean.vec)
  eff.front.risk[i]=sqrt(ln.model$value)
  eff.front.weight[i,]=ln.model$solution
}
plot(eff.front.risk,eff.front.return,type='l')






