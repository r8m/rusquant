"getSymbols.rogov" <-
function(Symbols,env,return.class='xts',index.class='Date',
         from='2014-01-01',
         to=Sys.Date(),
         adjust=FALSE,
         period='day',
         ...)
{
   importDefaults("getSymbols.rogov")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }
	 options(warn = -1)
     default.return.class <- return.class
     default.from <- from
     default.to <- to

     if(missing(verbose)) verbose <- TRUE
     if(missing(auto.assign)) auto.assign <- FALSE

     p <- 0
     if ("hour" == period) 
		 {
		 p <- 'Hourly' 
		 limit<-16000
		 }
	if ("day" == period) 
		 {
		 p <- 'Daily' 
		 limit<-16000
		 }
    if ("week" == period) 
		{
		p <- 'Weekly' 
		limit<-10000
		}
    if ("month" == period)
		{
		p <- 'Monthly' 
		limit<-1000
		}
	if ("year" == period)
		{
		p <- 'Annually'
		limit<-100
		}
    if (p==0) 
		 {
			message(paste("Unkown period ", period))
		 }
  
	format(as.Date(from),"%m.%d.%Y")->rogov.from
	format(as.Date(to),"%m.%d.%Y")->rogov.to
for(i in 1:length(Symbols)) {	
	rogov.URL<-"http://www.rogovindex.com/Quote/searchentries?rogovindex.period="	
	stock.URL <- paste(rogov.URL,p,"&limit=",limit,"&RegionFromDate=",rogov.from,"&regiontodate=",rogov.to,sep="")
	tmp <- tempfile()
    download.file(stock.URL, destfile=tmp,quiet=TRUE)
    fr <- read.table(tmp, sep="{",header=FALSE)	 
	as.character(unlist(fr[3:length(fr)]))->fr
	gsub("ValueDateString:", "", fr)->fr
	gsub("BaseValue:", "", fr)->fr
	gsub("FValue:", "", fr)->fr
	gsub("BValue:", "", fr)->fr
	gsub("RValue:", "", fr)->fr
	gsub("YValue:", "", fr)->fr
	gsub("},", "", fr)->fr
	gsub("}]}", "", fr)->fr		
	t(as.data.frame(strsplit(fr,",")))->fr
 
    unlink(tmp)	 	 
    
	if(p=='Hourly')       
		{
		fr <- xts(apply(as.matrix(fr[,2:6]),2, as.numeric), as.POSIXct(strptime(fr[,1], "%m/%d/%Y %I:%M:%S %p")),src='rogov',updated=Sys.time())
		colnames(fr) <- c('Base Value','F Value','B Value','R Value','Y Value')
		}
	if(p=='Monthly')       
		{
		fr <- xts(apply(as.matrix(fr[,2:6]),2, as.numeric), as.Date(strptime(paste("01/",fr[,1],sep=""), "%d/%m/%Y")),src='rogov',updated=Sys.time())
		colnames(fr) <- c('Base Value','F Value','B Value','R Value','Y Value')
		} 
	if(p=='Annually')       
		{
		fr <- xts(apply(as.matrix(fr[,2:6]),2, as.numeric), as.Date(strptime(paste("01/01/",fr[,1],sep=""), "%d/%m/%Y")),src='rogov',updated=Sys.time())
		colnames(fr) <- c('Base Value','F Value','B Value','R Value','Y Value')
		} 	 
	 
    if(p=='Daily' | p=='Weekly')       
		{
		fr <- xts(apply(as.matrix(fr[,2:6]),2, as.numeric), as.Date(strptime(fr[,1], "%m/%d/%Y")),src='rogov',updated=Sys.time())
		colnames(fr) <- c('Base Value','F Value','B Value','R Value','Y Value')
		}

       fr <- convert.time.series(fr=fr,return.class=return.class)
     #  if(is.xts(fr))
     #    indexClass(fr) <- index.class

       Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]]))
       if(auto.assign)
         assign(Symbols[[i]],fr,env)
       if(i >= 5 && length(Symbols) > 5) {
         message("pausing 1 second between requests for more than 5 symbols")
         Sys.sleep(1)
       }
     }
     if(auto.assign)
       return(Symbols)
	
	
    return(fr)
}

"getSymbols.Finam" <-
function(Symbols,env,return.class='xts',index.class='Date',
         from='2014-01-01',
         to=Sys.Date(),
         adjust=FALSE,
         period='day',
         ...)
{
     importDefaults("getSymbols.Finam")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }

     default.return.class <- return.class
     default.from <- from
     default.to <- to

     if(missing(verbose)) verbose <- FALSE
     if(missing(auto.assign)) auto.assign <- FALSE

     p <- 0

     if ("tick" == period) p <- 1
     if ("1min" == period) p <- 2
     if ("5min" == period) p <- 3
     if ("10min" == period) p <- 4
     if ("15min" == period) p <- 5
     if ("30min" == period) p <- 6
     if ("hour" == period) p <- 7
     if ("day" == period) p <- 8
     if ("week" == period) p <- 9
     if ("month" == period) p <- 10

     if (p==0) {
        message(paste("Unkown period ", period))
     }
     finam.HOST <- '195.128.78.52'
     finam.URL <- "/table.csv?d=d&market=1&f=table&e=.csv&dtf=1&tmf=1&MSOR=0&sep=1&sep2=1&at=1&"

     if (!exists("finam.stock.list")){
        finam.stock.list <- loadStockListFinam()
        assign('finam.stock.list', finam.stock.list, env)
     }
     fr <- NaN
     for(i in 1:length(Symbols)) {

       return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
       return.class <- ifelse(is.null(return.class),default.return.class,
                              return.class)
       from <- getSymbolLookup()[[Symbols[[i]]]]$from
       from <- if(is.null(from)) default.from else from
       to <- getSymbolLookup()[[Symbols[[i]]]]$to
       to <- if(is.null(to)) default.to else to

       from.y <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][1])
       from.m <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][2])-1
       from.d <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][3])
       to.y <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][1])
       to.m <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][2])-1
       to.d <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][3])

       Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
       Symbols.name <- ifelse(is.null(Symbols.name),Symbols[[i]],Symbols.name)
       if(verbose) cat("downloading ",Symbols.name,".....\n\n")
       Symbols.id <- finam.stock.list[Symbols.name]

       if (is.na(Symbols.id)){
           if (verbose)
                cat("Don't know about",Symbols[[i]],"\n\n")
           next
       }

       stock.URL <- paste(finam.URL,
                           "p=", p,
                           "&em=",Symbols.id,
                           "&df=",from.d,
                           "&mf=",from.m,
                           "&yf=",from.y,
                           "&dt=",to.d,
                           "&mt=",to.m,
                           "&yt=",to.y,
                           "&cn=",Symbols.name,
                           sep='')
       if (verbose) cat(stock.URL);
       tmp <- tempfile()
       if (p==1){
           lts <-  http.get(finam.HOST, paste(stock.URL, '&datf=6', sep=''),  referer='http://www.finam.ru/analysis/export/default.asp', verbose=verbose)
           write(lts, file=tmp)
       }else {
           stock.URL <- paste('http://', finam.HOST, stock.URL, '&datf=1' , sep='')
           download.file(stock.URL, destfile=tmp, quiet=!verbose)
       }
       fr <- read.csv(tmp, as.is=TRUE, colClasses="character")
       unlink(tmp)

       if(verbose) cat("done.\n")
       if (p==1){
            fr <- xts(apply(as.matrix(fr[,(5:6)]),2, as.numeric), as.POSIXct(strptime(paste(fr[,3],fr[,4]), "%Y%m%d %H%M%S")),
                    src='finam',updated=Sys.time())
            colnames(fr) <- paste(toupper(gsub('\\^','',Symbols.name)),
                             c('Close','Volume'),
                             sep='.')
       }else if (p>7) {
            fr <- xts(apply(as.matrix(fr[,(5:9)]),2, as.numeric), as.Date(strptime(fr[,3], "%Y%m%d")),
                 src='finam',updated=Sys.time())
            colnames(fr) <- paste(toupper(gsub('\\^','',Symbols.name)),
                             c('Open','High','Low','Close','Volume'),
                             sep='.')
       }else {
            fr <- xts(apply(as.matrix(fr[,(5:9)]),2,as.numeric), as.POSIXct(strptime(paste(fr[,3],fr[,4]), "%Y%m%d %H%M%S")),
                    src='finam',updated=Sys.time())
            colnames(fr) <- paste(toupper(gsub('\\^','',Symbols.name)),
                             c('Open','High','Low','Close','Volume'),
                             sep='.')
       }

   
       fr <- convert.time.series(fr=fr,return.class=return.class)
       if(is.xts(fr) && p>7)
         indexClass(fr) <- index.class

       Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]]))
       if(auto.assign)
         assign(Symbols[[i]],fr,env)
       if(i >= 5 && length(Symbols) > 5) {
         message("pausing 1 second between requests for more than 5 symbols")
         Sys.sleep(1)
       }
     }
     if(auto.assign)
       return(Symbols)

     return(fr)

}

"getSymbols.mfd" <-
function(Symbols,env,return.class='xts',index.class='Date',
         from='2014-01-01',
         to=Sys.Date(),
         adjust=FALSE,
         period='day',
         updatetickers=FALSE,
         ...)
{
     importDefaults("getSymbols.mfd")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }

     default.return.class <- return.class
     default.from <- from
     default.to <- to

     if(missing(verbose)) verbose <- FALSE
     if(missing(auto.assign)) auto.assign <- FALSE

    p <- 0

    if ("1min" == period) p <- 1
    if ("5min" == period) p <- 2
    if ("10min" == period) p <- 3
    if ("15min" == period) p <- 4
    if ("30min" == period) p <- 5
    if ("hour" == period) p <- 6
    if ("day" == period) p <- 7
    if ("week" == period) p <- 8
    if ("month" == period) p <- 9
    if ("tick" == period) p <- 0

     #if (p==0) {
     #   message(paste("Unkown period ", period))
     #}
	 for (i in 1:length(Symbols)) {
        mfd.from <- format(as.Date(from), "%d.%m.%Y")
        mfd.to <- format(as.Date(to), "%d.%m.%Y")
        Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
        Symbols.name <- ifelse(is.null(Symbols.name), Symbols[[i]], 
                               Symbols.name)
        
        if(updatetickers){
            tickers <- loadStockListMfd()
            assign('tickers', tickers, env)
        }
            
        #data("tickers")
        if (!exists("tickers")){
            data("tickers")
        }
        
        
        SYMBOL.GROUP <- tickers[which(tickers[, 4] == Symbols), 1]
        SYMBOL.ID <- tickers[which(tickers[, 4] == Symbols), 3]
        if (length(SYMBOL.ID) == 0) {
            if (verbose) 
                cat("Don't know about", Symbols[[i]], "\n\n")
            next
        }
        mfd.URL <- "http://mfd.ru/export/handler.ashx/Data.txt?"
        stock.URL <- paste(mfd.URL, "TickerGroup=", SYMBOL.GROUP, 
                           "&Tickers=", SYMBOL.ID, "&Alias=false&Period=", p, 
                           "&timeframeValue=1&timeframeDatePart=day&StartDate=", 
                           mfd.from, "&EndDate=", mfd.to, "&SaveFormat=0&SaveMode=0&FileName=Date18112013_23112013.txt&FieldSeparator=%253b&DecimalSeparator=.&DateFormat=yyyyMMdd&TimeFormat=HHmmss&DateFormatCustom=&TimeFormatCustom=&AddHeader=true&RecordFormat=0&Fill=false", 
                           sep = "")
        tmp <- tempfile()
        download.file(stock.URL, destfile = tmp, quiet = TRUE)
        fr <- read.table(tmp, sep = ";", header = TRUE)
        unlink(tmp)
        if (p %in% 1:6) {
            fr[fr[, 4] < 1e+05 & fr[, 4] >= 10000, 4] <- paste("0", 
                                                               as.character(fr[fr[, 4] < 1e+05 & fr[, 4] >= 
                                                                                   10000, 4]), sep = "")
            fr[as.double(fr[, 4]) < 10000 & as.double(fr[, 4]) > 
                   0, 4] <- paste("00", (fr[as.double(fr[, 4]) < 
                                                10000 & as.double(fr[, 4]) > 0, 4]), sep = "")
            fr[fr[, 4] == "0", 4] <- paste("00000", (fr[fr[, 
                                                           4] == "0", 4]), sep = "")
            fr <- xts(apply(as.matrix(fr[, (5:10)]), 2, as.numeric), 
                      as.POSIXct(strptime(paste(fr[, 3], fr[, 4]), 
                                          "%Y%m%d %H%M%S")), src = "mfd", updated = Sys.time())
            if(!is.null(colnames(fr)))
                colnames(fr) <- c("Open", "High", "Low", "Close", "Volume", "OPEN_INTEREST")
        }
        if (p %in% 7:9) {
            fr <- xts(apply(as.matrix(fr[, (5:10)]), 2, as.numeric), 
                      as.Date(strptime(fr[, 3], "%Y%m%d")), src = "mfd", updated = Sys.time())
            
            if(!is.null(colnames(fr)))
                colnames(fr) <- c("Open", "High", "Low", "Close", "Volume", "OPEN_INTEREST")
        }
        
        
        #<TICKER>;<PER>;<DATE>;<TIME>;<CLOSE>;<VOL>
        if (p == 0) {
            fr[fr[, 4] < 1e+05 & fr[, 4] >= 10000, 4] <- paste("0", 
                                                               as.character(fr[fr[, 4] < 1e+05 & fr[, 4] >= 
                                                                                   10000, 4]), sep = "")
            fr[as.double(fr[, 4]) < 10000 & as.double(fr[, 4]) > 
                   0, 4] <- paste("00", (fr[as.double(fr[, 4]) < 
                                                10000 & as.double(fr[, 4]) > 0, 4]), sep = "")
            fr[fr[, 4] == "0", 4] <- paste("00000", (fr[fr[, 
                                                           4] == "0", 4]), sep = "")
            fr <- xts(apply(as.matrix(fr[, (5:6)]), 2, as.numeric), 
                      as.POSIXct(strptime(paste(fr[, 3], fr[, 4]), 
                                          "%Y%m%d %H%M%S")), src = "mfd", updated = Sys.time())
            colnames(fr) <-  c("Close", "Volume")
        }
        
        fr <- convert.time.series(fr = fr, return.class = return.class)
        fr<-fr[,-6]
        if (is.xts(fr) && p > 7) 
            indexClass(fr) <- index.class
        Symbols[[i]] <- toupper(gsub("\\^", "", Symbols[[i]]))
        if (auto.assign) 
            assign(Symbols[[i]], fr, env)
        if (i >= 5 && length(Symbols) > 5) {
            message("pausing 1 second between requests for more than 5 symbols")
            Sys.sleep(1)
        }
    }
    if (auto.assign) 
        return(Symbols)
    if (exists("fr")) 
        return(fr)
    
}


#This one is taken from quanmod package since it's not available through the API
"convert.time.series" <-
function (fr, return.class)
{
    if ("quantmod.OHLC" %in% return.class) {
        class(fr) <- c("quantmod.OHLC", "zoo")
        return(fr)
    }
    else if ("xts" %in% return.class) {
        return(fr)
    }
    if ("zoo" %in% return.class) {
        return(as.zoo(fr))
    }
    else if ("ts" %in% return.class) {
        fr <- as.ts(fr)
        return(fr)
    }
    else if ("data.frame" %in% return.class) {
        fr <- as.data.frame(fr)
        return(fr)
    }
    else if ("matrix" %in% return.class) {
        fr <- as.data.frame(fr)
        return(fr)
    }
    else if ("its" %in% return.class) {
        if ("package:its" %in% search() || suppressMessages(require("its",
            quietly = TRUE))) {
            fr.dates <- as.POSIXct(as.character(index(fr)))
            fr <- its::its(coredata(fr), fr.dates)
            return(fr)
        }
        else {
            warning(paste("'its' from package 'its' could not be loaded:",
                " 'xts' class returned"))
        }
    }
    else if ("timeSeries" %in% return.class) {
        if ("package:fSeries" %in% search() || suppressMessages(require("fSeries",  quietly = TRUE))) {
            fr <- timeSeries(coredata(fr), charvec = as.character(index(fr)))
            return(fr)
        }
        else {
            warning(paste("'timeSeries' from package 'fSeries' could not be loaded:", " 'xts' class returned"))
        }
    }
}

"getSymbols.Forts" <-
function(Symbols,env,return.class='xts',index.class='Date',
         from='2007-01-01',
         to=Sys.Date(),
         adjust=FALSE,
         period='day',
         ...)
{
     importDefaults("getSymbols.Forts")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }

     default.return.class <- return.class
     default.from <- from
     default.to <- to

     if(missing(verbose)) verbose <- FALSE
     if(missing(auto.assign)) auto.assign <- FALSE

     forts.URL <- "http://www.rts.ru/ru/forts/contractresults-exp.html?"

     for(i in 1:length(Symbols)) {

       return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
       return.class <- ifelse(is.null(return.class),default.return.class,
                              return.class)
       from <- getSymbolLookup()[[Symbols[[i]]]]$from
       from <- if(is.null(from)) default.from else from
       to <- getSymbolLookup()[[Symbols[[i]]]]$to
       to <- if(is.null(to)) default.to else to

       from.f <- format(as.Date(from,origin='1970-01-01'), '%Y%m%d')
       to.f <- format(as.Date(to,origin='1970-01-01'), '%Y%m%d')

       Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
       Symbols.name <- ifelse(is.null(Symbols.name),Symbols[[i]],Symbols.name)
       if(verbose) cat("downloading ",Symbols.name,".....\n\n")

       tmp <- tempfile()
       stock.URL <- paste(forts.URL,
                           "day1=", from.f,
                           "&day2=", to.f,
                           "&isin=",gsub(' ', '%20', Symbols.name),
                           sep='')

       download.file(stock.URL, destfile=tmp, quiet=!verbose)

       fr <- read.csv(tmp, as.is=TRUE, skip=1)
       unlink(tmp)

       if(verbose) cat("done.\n")

      fr <- xts(as.matrix(cbind(fr[,(4:7)], fr[,12], fr[,14]) ), as.Date(strptime(fr[,1], "%d.%m.%Y")),
                src='forts',updated=Sys.time())

       colnames(fr) <- paste(toupper(gsub('[ -.]','',Symbols.name)),
                             c('Open','High','Low','Close', 'Volume', 'Positions'),
                             sep='.')

       fr <- convert.time.series(fr=fr,return.class=return.class)
       if(is.xts(fr))
         indexClass(fr) <- index.class

       Symbols[[i]] <-toupper(gsub('[ -.]','',Symbols[[i]]))
       if(auto.assign)
         assign(Symbols[[i]],fr,env)
       if(i >= 5 && length(Symbols) > 5) {
         message("pausing 1 second between requests for more than 5 symbols")
         Sys.sleep(1)
       }
     }
     if(auto.assign)
       return(Symbols)

     return(fr)

}


"select.hours" <-
function(data, hour){
    return(data[format(index(data), format="%H")==hour])
}


http.get <- function(host, path, port=80, referer="", verbose=FALSE) {

  if(missing(path))
    path <- "/"
  if(missing(host))
    stop("No host URL provided")

  header <- NULL
  header <- c(header,paste("GET ", path, " HTTP/1.0\r\n", sep=""))
  header <- c(header,"User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:6.0.2) Gecko/20100101 Firefox/6.0.2\r\n")
  header <- c(header,"Accept: */*\r\n")
  header <- c(header,"Accept-Encoding: deflate\r\n")
  header <- c(header,paste("Referer: ", referer, "\r\n", sep=""))

  request <- paste(header, sep="", collapse="")

  if (verbose) {
    cat("Sending HTTP GET request to ", host, ":", port, "\n")
    cat(request, "\n")
  }

  con <- socketConnection(host=host, port=port, open="w+", blocking=TRUE, encoding="UTF-8")

  on.exit(close(con))

  writeLines(request, con)

  response <- list()
  response$status <- readLines(con, n=1)
  if (verbose) {
    write(response$status, stderr())
    flush(stderr())
  }
  response$headers <- character(0)
  repeat{
    ss <- readLines(con, n=1)
    if (verbose) {
      write(ss, stderr())
      flush(stderr())
    }
    if (ss == "") break
    key.value <- strsplit(ss, ":\\s*")
    response$headers[key.value[[1]][1]] <- key.value[[1]][2]
  }
  response$body = readLines(con)

  return(response$body)
}

"searchSymbol"<-function(Symbol, source="mfd"){
	if (!exists("tickers")){
            tickers <- loadStockListMfd()
            assign('tickers', tickers, parent.frame())
    }
    tickers[grepl(Symbol,tickers[,4]),4]
}