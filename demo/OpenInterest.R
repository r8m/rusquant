# Load Open Interest daily data for Si
library(ggplot2)
library(grid)


urlOI<-"http://moex.com/ru/derivatives/open-positions-csv.aspx?d="
#"&t=1"
dateStart<-as.Date("2015-10-01", "%Y-%m-%d")
dateEnd<-as.Date("2015-11-06", "%Y-%m-%d")
OpIn.df<-read.csv(paste(urlOI,format(dateStart, "%Y%m%d"),"&t=1", sep=""), nrows=1)
i<-dateEnd
for(i in 0:(dateEnd-dateStart)){
    id<-format(dateStart+i, "%Y%m%d")
    iurl<-paste(urlOI,id,"&t=1", sep="")
    
    print(iurl)
    OpIn.i<-read.csv(iurl)
    OpIn.df<-rbind(OpIn.df, OpIn.i)
}

SiData<-OpIn.df[OpIn.df$isin=="Si" & OpIn.df$contract_type=="F",]
SiData$date<-SiData[,1]
qShort<-qplot(y=short_position,x=date, data=SiData, size=clients_in_lshort, color=factor(iz_fiz))
qLong<-qplot(y=long_position,x=date, data=SiData, size=clients_in_long, color=factor(iz_fiz))
qLS<-qplot(y=long_position-short_position,x=date, data=SiData, size=clients_in_long-clients_in_lshort, color=factor(iz_fiz))

pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(qLS, vp = vplayout(1, 1:2))  # key is to define vplayout
print(qShort, vp = vplayout(2, 1))
print(qLong, vp = vplayout(2, 2))