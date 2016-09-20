# MOEX Investors Challenge statistics
# http://investor.moex.com

library(blotter)
library(rusquant)

  #Portfolio and dAcconut names
  userPortf<-"user_port"
  userAcc<-"user_acc"
  #Remove account and portfolio if run previously
  try(rm(list=c(paste("portfolio",userPortf,sep="."),
                paste("account",userAcc,sep=".")),
         pos=.blotter), silent =FALSE)
  
  
  #download user data and trades data
  userId<-"49083"
  yearId<-2015
  dateId<-"all" # all - all trades, 20141208 - day trades
  marketId<-2 # 1 - spot, 2 - deriv
  ProfileLink<-"http://investor.moex.com/ru/statistics/2015/portfolio.aspx?traderId="
  ProfileLink<-paste(ProfileLink, userId, sep="")
  TradesLink<-"ftp://ftp.moex.com/pub/info/stats_contest"
  TradesLink<-paste(TradesLink, yearId,dateId,
                    paste(marketId,"_", userId,".zip", sep=""),
                    sep="/")
  download.file(TradesLink, paste(marketId,"_", userId,".zip", sep=""))
  unzip(paste(marketId,"_", userId,".zip", sep=""))
  
  #Read trades data
  userData<-read.csv(paste(marketId,"_", userId,".csv", sep=""),sep=";", header=FALSE)
  
  #Removing temporary files
  file.remove(paste(marketId,"_", userId,".zip", sep=""))
  file.remove(paste(marketId,"_", userId,".csv", sep=""))
  
  
  #Processing data and declare symbol
  #userSymbol<-"SiZ4"
  userSymbols<-levels(factor(userData$V2))
  
  #Load historical data for the symbol
  #symbol<-"SiZ4 (12.2014)"
  #tickers <- loadStockListMfd()
  data("tickers")
  symbols<-unlist(sapply(paste(userSymbols, " ", sep=""), 
                         searchSymbol, USE.NAMES=FALSE))
  
  from<-as.Date(userData[1,1])
  to<-Sys.Date()
  period="5min"
  i<-1
  for(s in symbols){
    assign(userSymbols[i],
           getSymbols(s, 
                      from=from, 
                      to=to, 
                      period=period, 
                      src='mfd',
                      adjust=TRUE, 
                      auto.assign=FALSE),
           envir=.GlobalEnv)
    i<-i+1
    
  }
  
  #Initialize stocks
  currency("RUB")
  #symbol<-toupper(symbol)
  symbols<-toupper(symbols)
  symbols.df<-data.frame(symbols, userSymbols)
  symbol<-userSymbols[1]
  
  #stock(symbol,currency="RUB",multiplier=1)
  stock(userSymbols,currency="RUB",multiplier=1)
  
  
  # Initialize the Portfolio
  initDate<-"2010-01-14"
  initEq<-100000
  #initPortf(userPortf,symbols=symbol,initDate=initDate)
  initPortf(userPortf,symbols=userSymbols,initDate=initDate)
  
  initAcct(userAcc,portfolios=userPortf,initDate=initDate, initEq=initEq)
  
  # look at the transactions data
  #symbol.trades
  
  # Add the transactions to the portfolio
  for(s in symbols){
    us<-as.character(symbols.df[symbols.df[,1]==s,2])
    symbol.trades<-userData[userData$V2==us,]
    symbol.trades<-xts(cbind(symbol.trades$V4,symbol.trades$V3),
                       order.by=as.POSIXct(symbol.trades[,1]))
    colnames(symbol.trades)<-c("TxnPrice","TxnQty")
    blotter:::addTxns(userPortf,us,
                      TxnData=symbol.trades,verbose=FALSE)
    
  }
  
  # update the portfolio stats
  updatePortf(userPortf)
  
  # update the account P&L
  updateAcct(userAcc)
  
  # and look at it
  portfolio = getPortfolio(userPortf)
  account = getAccount(userAcc)
  
  
  # FORMAT THEME
  theme<-chart_theme()
  theme$col$up.col<-'#81F7BE'
  theme$col$up.border<-'#81F7BE'
  theme$col$dn.col<-'#FAAC58'
  theme$col$dn.border<-'#FAAC58'
  
  #Plots
  chart.Posn(userPortf, userSymbols[2], theme=theme)


chart.ME(
    Portfolio=userPortf,
    Symbol=symbol,
    type='MAE',
    scale='percent'
)

chart.ME(
    Portfolio=userPortf,
    Symbol=symbol,
    type='MFE',
    scale='percent'
)


#trade statistics
tStats <- tradeStats(Portfolios = userPortf, use="trades", inclZeroDays=FALSE)
tStats[,4:ncol(tStats)] <- round(tStats[,4:ncol(tStats)], 2)
print(data.frame(t(tStats[,-c(1,2)])))

#daily statistics
dStats<-dailyStats(Portfolios = userPortf, use="trades")
print(data.frame(t(dStats)))

# Returns (check initial equity first)
charts.PerformanceSummary(PortfReturns(userAcc))
portRet<-PortfReturns(Account=userAcc, period="daily")
print(data.frame(t(portRet)))
