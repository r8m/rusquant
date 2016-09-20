# MIX 0 miniMIX arb test
library(rusquant)
library(ggplot2)
library(data.table)

#' 1min OHLC test
symbs<-c("MXZ5 (12.2015)","MMZ5 (12.2015)")
from<-as.Date("2015-10-15")
to<-Sys.Date()
period="1min"
for (symb in symbs)
  getSymbols(symb, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=TRUE)

symbSpread<-get(symbs[1])$Close-get(symbs[2])$Close*100
symbSpread<-data.frame(DateTimeSymb=as.POSIXct(index(symbSpread)),as.data.frame(symbSpread, stringsAsFactors=FALSE),stringsAsFactors=FALSE)
symbSpread<-data.table(symbSpread)

qplot(format(DateTimeSymb, "%m%d"),y=Close, data=symbSpread,
      alpha=0.1,
       geom = c("violin", "jitter"))
qplot(DateTimeSymb,Close,data=symbSpread)
summary(symbSpread)
