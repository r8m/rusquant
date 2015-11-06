#Si~BR linear regression test

library(rusquant)
library(ggplot2)
require(scales)
symbols<-c("SiZ5 (12.2015)",
           "BRX5 (11.2015)")

from<-"2015-10-01"
to<-Sys.Date()
period<-"1min"
for(s in symbols)
    getSymbols(s, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=TRUE)

symbols<-toupper(symbols)

df<-merge.xts(Cl(get(symbols[1])),Cl(get(symbols[2])),Vo(get(symbols[2])))
colnames(df)<-c("si", "br", "volSi")
weekNum<-as.numeric(format(index(df), "%U"))
startDate<-as.Date("2014-01-01")
weekNum<-paste("w",weekNum,": ",weekNum*7+startDate-2," ~",sep="")
df$br<-df$br*1000
qplot(br, si, 
      alpha=0.5,size=volSi,color=factor(weekNum),data=df,
      #facets= weekNum~.,
      geom=c("point", "smooth"),method="lm", formula=y~x,
      main="Si~BR linear regression", 
      xlab=symbols[2], ylab=symbols[1]) + scale_x_continuous(breaks=pretty_breaks(n=20)) + scale_y_continuous(breaks=pretty_breaks(n=20))
summary(lm(si~factor(weekNum)+(br), df))
