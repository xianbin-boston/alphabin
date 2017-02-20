#######################################################################
########         Check if a package is installed or not        ######## 
######## if yes then require it; if not install and require it ########
####################################################################### 
usePackage <- function(p) {
    installed <- installed.packages()[,1]
    for (i in 1:length(p)){
        if (is.element(p[i], installed)==F){
            install.packages(p[i])
            require(p[i],quiet=T, character.only=T)
        } else {
            flag <- tryCatch({require(p[i],quiet=T,character.only=T)},
                             warning = function(war){war},
                             error = function(err){err}
                             )
            if ((inherits(flag,"error"))) {
                install.packages(p[i])
                require(p[i],quiet=T, character.only=T)
            } else {
                require(p[i],quiet=T,character.only=T)
            }
        }
    }
}

pkglist <- c("jsonlite","RCurl","XML","lubridate","zoo","quantmod","dplyr","tidyr","httr","xml2")

# comment out "XLConnect"

usePackage(pkglist)



#########################################
######## Account Initial Setting ########
#########################################
host <- "https://api-fxtrade.oanda.com"
urlbase0 <- "https://api-fxtrade.oanda.com/v1"
token <- "xxx"
id <- xxx

# Practice Account Setting
# urlbase0 <- "https://api-fxpractice.oanda.com/v3"
# token <- "xxx"
# id <- "xxx"


########################################################################################
######## Note: in case i need datetime stamp in the furure                      ########
######## DateTime_GMT <- as.POSIXct(format(Sys.time(), tz = "GMT"), tz = "GMT") ########
######## DateTime_Loc <- Sys.time()                                             ########
######################################################################################## 


#################################
########  Base Function  ########
########  conv.num       ########
########  conv.datetime  ########
########  cleanPair      ########
########  conv.pair      ########
########  conv.urlbase   ########
########  check.na.df    ########
########  conv.response  ########
########  check.nchar.df ########
########  cross.flg      ########
########  check.nchar.df ########
########  append.csv     ########
########  cleanDollar    ########
########  quote.convention ########
########  exposureUSD    ########
#################################

################################################################
######## Convert possible numeric value in a Data Frame ########
################################################################
conv.num <- function(data){
    output <- data
    for (i in 1:ncol(data)) {
        flag <- tryCatch({cleanDollar(data[,i])},
                         warning = function(war){war},
                         error = function(err){err}
        )
        if (!(inherits(flag,"error") | inherits(flag,"warning"))) {
            output[,i]=cleanDollar(data[,i])}
    }
    return(output)
}


#####################################################################################
######## Convert datetime YYYYMMDD-HHMMSS to OANDA API ready datetime format ########
#####################################################################################
conv.datetime <- function(datetime) {
    
    if (is.POSIXt(datetime)) {temp <- datetime} else 
        if (class(datetime)=="character") {
            temp <- with_tz(ymd_hms(datetime, tz="America/New_York"),tz="UTC")
            if (is.na(temp)) {stop("Please Specify A Valid Datetime in YYMMDD-HHMMSS format")}
        }
    date <- date(temp)
    
    x<-function(a){
        if (nchar(a)==1) 
        {out=paste0("0",a)} 
        else {out=paste0(a)} 
        return(out)
    }
    
    time <- paste(x(hour(temp)), x(minute(temp)), x(round(second(temp),0)), sep = "%3A")
    output <- paste0(date, "T", time, "Z")
    return(output)
}


###############################################################
######## Paste currency pair to OANDA API ready format ########
###############################################################
cleanPair <- function(pair){
    toupper(paste(gsub("[[:punct:]]","_",pair)))
}

conv.pair <- function(pair){
    output <- paste(cleanPair(pair),collapse="%2C")
    return(output)
}


####################################################################
######## Pick the appropriate urlbase based on API endpoint ########
####################################################################
conv.urlbase <- function(endpoint) {
    if (endpoint=="rate") {out<-urlbase0} else
      if (endpoint %in% c("account","transaction","order","trade","position")) {out<-paste0(urlbase0,"/accounts/",id)} else
        if (endpoint=="lab") {out<-paste0(host,"/labs/v1")}
  return(out)
}


################################################
######## Check if all column/row are NA ########
################################################
check.na.df <- function (df,margin) {
  temp=unlist(apply(df,margin,function(x)all(is.na(x))))
  if (margin==1) {out=which(temp==F)}
  else {out=names(temp[temp==F])}
  return(out)
}


##########################################################
######## Split HTTP response into HEADER and BODY ########
##########################################################
conv.response <- function (rsp) {
    output <- unlist(strsplit(rsp,"\r\n\r\n"))
    return(output)
}


#############################################################
########  Create Base/Quote currency reference flag  ########
######## Useful to convert triangular arbitrage rate ######## 
#############################################################
cross.flg <- function(instrument=NULL){
    if (length(instrument)>0) {
        indsn <- as.data.frame(list(sapply(instrument,conv.pair,USE.NAMES=F)),col.names="instrument")
    } else {
        indsn <- list.pair()
    }
    flg <- function(a) {paste0(ifelse(unlist(strsplit(toupper(conv.pair(a)),"_"))=="USD",1,0),collapse = "")}
    output <- select(indsn,instrument)
    output$flg <- sapply(output$instrument,flg) 
    return(output)
}


### beta ###
######################################################################
######## Check values length in each column from raw csv file ########
######################################################################
check.nchar.df <- function(df){
  for (i in 1:length(df)) {
    a <- read.csv(file=df[i],sep=",")
    print(apply(apply(a, 2, nchar),2,max))
    print(length(which(duplicated(a)==T)))
  }
}

###################################################################
######## Append rows and remove deuplicate in raw csv file ########
###################################################################
append.csv <- function(old, new) {
  old2 <- read.table(old, sep = ",", header = T, stringsAsFactors = F, quote = "\"")
  if (all(names(old2)==names(new))==T && class(old2)==class(new)) {
    out <- rbind.data.frame(old2,new)
    out2 <- out[order(out$timestamp,out$currency),]
    out <- out[!duplicated(out),]
    return(out)
  } else {stop("two df need to have the same column.name")}
}


#########################################
######## Clean accounting number ######## 
########      strip $ and ,      ########
######################################### 
cleanDollar <- function(inchar) {
    step1 <- sub("$","",as.character(inchar),fixed = T)
    output <- as.numeric(gsub(",","",as.character(step1),fixed = T))
    return(output)
}


#############################################
### Create flag for Base/Quote convention ###
###      01 for quote currency = USD      ###
###      10 for base currency = USD       ###
###      00 for cross currency pair       ###
#############################################
quote.convention <- function(instrument=NULL){
    if (length(instrument)>0) {
        indsn <- as.data.frame(list(sapply(instrument,conv.pair,USE.NAMES=F)),col.names="instrument")
    } else {
        indsn <- list.pair()["instrument"]
    }
    flg <- function(a) {paste0(ifelse(unlist(strsplit(toupper(conv.pair(a)),"_"))=="USD",1,0),collapse = "")}
    output <- select(indsn,instrument)
    output$flg <- sapply(output$instrument,flg) 
    return(output)
}

####################################
### Calculate trade value in USD ###
####################################
exposureUSD <- function(instrument,
                 unit,
                 side) {
    
    sign <- ifelse(toupper(side)=="BUY","ask","bid")
    support.pair.list<-c(list.pair()$instrument)
    
    base <- matrix(unlist(strsplit(cleanPair(instrument),"_")),ncol=2, byrow=T)[,1]
    quote <- matrix(unlist(strsplit(cleanPair(instrument),"_")),ncol=2, byrow=T)[,2]
    flg <- quote.convention(instrument)$flg
    
    idx <- ifelse(flg=="00" & (base %in% c("AUD","EUR","GBP","NZD")),1,2)
    idx <- ifelse(flg=="01",3,idx)
    idx <- ifelse(flg=="10",4,idx)
    
    mid_instrument <- NULL
    mid_instrument[idx==1]=paste0(base,"_USD")[idx==1]
    mid_instrument[idx==2]=paste0("USD_",base)[idx==2]
    mid_instrument[idx==3]=instrument[idx==3]
    mid_instrument[idx==4]=idx[idx==4]
    
    price <- NULL
    for (i in 1:length(instrument)) {
        if(idx[i]!=4) {
            price[i] <- unlist(list.price(mid_instrument[i])[sign[i]])
        } else {price[i]=1}
    }

    tradeValue <- as.numeric(unit*price)
    return(tradeValue)
}






####################################
######## Oanda API Function ########
####################################

##################################################
########  List all trading currency pair  ########
######## Endpoint: Rate; Path: instrument ########
##################################################
list.pair <- function(instrument = NULL, 
                      detail = NULL) {
    
    ######## Initial setup
    path <- "/instruments?"
    endpoint <- "rate"
    urlbase <- conv.urlbase(endpoint)
    
    ######## Account ID (required)
    if (length(id)>0) {
        par.accountId <- paste0("accountId=", id)
    } else {stop("Account ID is required")}
    
    ######## Currency Instrument (Optional)
    if (length(instrument)>0) {
        instrument <- conv.pair(instrument)
        par.instrument <- paste0("instruments=", instrument)
    } else {par.instrument <- NULL}
    
    ######## Fields (Optional)
    if (length(detail) > 0) {
        fields <- "instrument,pip,marginRate,precision,maxTradeUnits,maxTrailingStop,minTrailingStop,halted,interestRate"
        fields <- gsub(",", "%2C", fields)
        par.fields <- paste0("fields=", fields)
    } else {
        fields <- "instrument,pip,marginRate"
        fields <- gsub(",", "%2C", fields)
        par.fields <- paste0("fields=", fields)
    }
    
    ######## Query to retrive HTTP response and flag error
    query <- paste(c(par.accountId, par.instrument, par.fields), collapse = "&")
    info <- getURL(paste0(urlbase, path, query), 
                   httpheader = c(Authorization = token), 
                   ssl.verifypeer = FALSE,
                   header=T)
    header <- parseHTTPHeader(conv.response(info)[1])
    if (header["status"] == "201" | header["status"] == "200") {
        body <- fromJSON(conv.response(info)[2], flatten = T)
    } else {stop(paste("Error in HTTP request:",header["status"], header["statusMessage"]),sep=" ")}
    
    ######## Output result
    date <- data.frame(Date=paste(Sys.Date()),stringsAsFactors=F)
    data <- as.data.frame(body[[1]], stringsAsFactors=F)
    output <- cbind.data.frame(date, data)
    output <- conv.num(output)
    return(output)
}


##############################################################
######## List all currency pair bid/ask interest rate ########
########       Endpoint: Rate; Path: instrument       ########
##############################################################
list.interest <- function(instrument = NULL) {
    
    ######## Initial setup
    path <- "/instruments?"
    endpoint <- "rate"
    urlbase <- conv.urlbase(endpoint)
    
    ######## Account ID (required)
    if (length(id)>0) {
        par.accountId <- paste0("accountId=", id)
    } else {stop("Account ID is required")}
    
    ######## Currency Instrument (Optional)
    if (length(instrument)>0) {
        instrument <- conv.pair(instrument)
        par.instrument <- paste0("instruments=", instrument)
    } else {par.instrument <- NULL}
    
    ######## Fields (Default)
        fields <- "instrument,marginRate,interestRate"
        fields <- gsub(",", "%2C", fields)
        par.fields <- paste0("fields=", fields)
    
    ######## Query to retrive HTTP response and flag error
    query <- paste(c(par.accountId, 
                     par.instrument, 
                     par.fields
                     ),collapse = "&")
    info <- getURL(paste0(urlbase, path, query), 
                   httpheader = c(Authorization = token), 
                   ssl.verifypeer = FALSE,
                   header=T)
    header <- parseHTTPHeader(conv.response(info)[1])
    if (header["status"] == "201" | header["status"] == "200") {
        body <- fromJSON(conv.response(info)[2], simplifyVector = F, flatten = F)
    } else {stop(paste("Error in HTTP request:",header["status"], header["statusMessage"]),sep=" ")}
    
    ######## Output result
    output <- data.frame(NULL)
    body=body[[1]]
    len <- length(body)
    for (i in 1:len) {
        for (j in 1:length(body[[i]][[3]])) {
            out <- paste(body[[i]][[3]][[j]])
            pair <- names(body[[i]][[3]][j])
            out <- data.frame(cbind(paste0(Sys.Date()),pair, rbind(out)), stringsAsFactors=F)
            output <- rbind(output,out)
        }
    }
    output <- unique(output[order(output[,2]),])
    colnames(output) <- c("Date","Currency","Bid","Ask")
    rownames(output) <- NULL
    output <- conv.num(output)
    return(output)
}


###############################################################
########  Fetch live prices for specified instruments  ######## 
########        Endpoint: Rate; Path: prices           ########
###############################################################
list.price <- function(instrument) {
    
    ######## Initial setup
    path="/prices?"
    endpoint="rate"
    urlbase <- conv.urlbase(endpoint)
    
    ######## Currency Pairs (Required)
    if (length(instrument) > 0) {
        instrument <- conv.pair(instrument)
        par.instrument <- paste0("instruments=", instrument)
    } else {
        stop("Please Specify At Least One Currency Instrument")
    }
    
    ######## Query to retrive HTTP response and flag error
    query <- paste(c(par.instrument),collapse = "&")
    
    h <- basicHeaderGatherer()
    info <- (getURL(paste0(urlbase,path,query), 
                    httpheader = c(Authorization = token,Connection="Keep-Alive"), 
                    ssl.verifypeer = FALSE,
                    headerfunction = h$update
    )
    )
    header <- h$value()
    
    if (header["status"] == "201" | header["status"] == "200") {
        body <- fromJSON(conv.response(info))
    } else {
        stop(paste("Error in HTTP request:",header["status"], header["statusMessage"]),sep=" ")
    }
    
    ######## output result
    output <- body[["prices"]]
    output <- conv.num(output)
    return(output)
}


###########################################################
######## Download currency history OHLC price data ########
########        Endpoint: Rate; Path: price        ########
###########################################################
hist.price <- function(instrument = NULL, 
                      granularity = "D", 
                      start = NULL, 
                      end = NULL, 
                      count = 500, 
                      candleFormat = "bidask", 
                      includeFirst = NULL, 
                      dailyAlignment = NULL, 
                      alignmentTimezone = NULL, 
                      weeklyAlignment = "Sunday") {
  
    ######## Initial setup
    path <- "/candles?"
    endpoint <- "rate"
    urlbase <- conv.urlbase(endpoint)
    
    ######## Currency Pairs (Required)
    if (length(instrument) > 0) {
        instrument <- conv.pair(instrument)
        par.instrument <- paste0("instrument=", instrument)
    } else {
        stop("Please Specify At Least One Currency Instrument")
    }
    
    ######## Granularity (Optional, default = 'D')
    if (length(granularity) > 1) {
        stop("Only One Granularity Can Be Retrived")
    } else {
        if (toupper(granularity) %in% c("S5", "S10", "S15", "S30", "M1", "M2", "M3", "M4", 
                                        "M5", "M10", "M15", "M30", "H1", "H2", "H3", "H4", 
                                        "H6", "H8", "H12", "D", "W", "M")) {
            par.granularity <- paste0("granularity=", toupper(granularity))
        } else {
            stop("Invalid Granularity Value. Please select one of the following: 
                                        S5, S10, S15, S30, M1, M2, M3, M4, 
                                        M5, M10, M15, M30, H1, H2, H3, H4, 
                                        H6, H8, H12, D, W, M")
        }
    }
    
    ######## Start/End (Datetime value in the format of YYYYMMDD-HHMMSS, default=NULL)
    if (length(start) > 0) {
        par.start <- paste0("start=", conv.datetime(start))
    } else {
        par.start <- NULL
    }
    
    if (length(end) > 0) {
        par.end <- paste0("end=", conv.datetime(end))
    } else {
        par.end <- NULL
    }
    
    ######## Count (Optional, should not be specified if both the start and end parameters are also specified. default = 500, max=5000)
    if (length(par.start) > 0 & length(par.end) > 0) {
        par.count <- NULL
    } else {
        par.count <- paste0("count=", min(count, 5000))
    }
    
    ######## candleFormat ('midpoint'/'bidask', default='bidask')
    if (toupper(candleFormat) %in% c("MIDPOINT", "BIDASK")) {
        par.candleFormat <- paste0("candleFormat=", candleFormat)
    } else {
        stop("Invalid candleFormat. Please select either midpoint or bidask")
    }
    
    ######## includeFirst (only can used when "start" is specified)
    if (is.null(par.start)) {
        par.includeFirst <- NULL
    } else {
        par.includeFirst <- paste0("includeFirst=", includeFirst)
    }
    
    ######## dailyAlignment (The default for dailyAlignment is 21 when Eastern Daylight Time is in effect and 22 when Eastern Standard Time
    ######## is in effect.  This corresponds to 17:00 local time in New York)
    if (is.null(dailyAlignment)) {
        par.dailyAlignment <- NULL
    } else {
        par.dailyAlignment <- paste0("dailyAlignment=", dailyAlignment)
    }
    
    ######## alignmentTimezone (default='America/New_York')
    if (is.null(alignmentTimezone)) {
        par.alignmentTimezone <- NULL
    } else {
        par.alignmentTimezone <- paste0("alignmentTimezone=", alignmentTimezone)
    }
    
    ######## weeklyAlignment (default='Friday', other option including 'Monday','Tuesday'...)
    if (is.null(weeklyAlignment)) {
        par.weeklyAlignment <- NULL
    } else {
        par.weeklyAlignment <- paste0("weeklyAlignment=", weeklyAlignment)
    }
    
    ######## Query to retrive HTTP response and flag error
    query <- paste(c(par.instrument, 
                     par.granularity, 
                     par.start, 
                     par.end, 
                     par.count, 
                     par.candleFormat, 
                     par.dailyAlignment, 
                     par.alignmentTimezone, 
                     par.weeklyAlignment
                     ), collapse = "&")
    info <- getURL(paste0(urlbase, path, query), 
                   httpheader = c(Authorization = token), 
                   ssl.verifypeer = FALSE,
                   header=T)
    header <- parseHTTPHeader(conv.response(info)[1])
    if (header["status"] == "201" | header["status"] == "200") {
        body <- fromJSON(conv.response(info)[2])
    } else {stop(paste("Error in HTTP request:",header["status"], header["statusMessage"]),sep=" ")}
    
    ######## output result
    pair <- body[[1]]
    interval <- body[[2]]
    candles <- body[[3]]
    output <- cbind.data.frame(pair, interval, candles, stringsAsFactors=F)
    output <- conv.num(output)
    return(output)
}


##############################################################
########        Download all transaction history      ########
######## Endpoint: Transaction; Path: alltransactions ########
##############################################################
track.history <- function (write=F,
                            outdir="./"){
    
    ######## Initial setup
    path <- "/alltransactions"
    endpoint <- "transaction"
    urlbase <- conv.urlbase(endpoint)
    
    ######## download full history zip file
    query=""
    
    if(inherits(tryCatch(parseHTTPHeader(getURL(paste0(urlbase, path, query), 
                                                httpheader = c(Authorization = token), 
                                                ssl.verifypeer = FALSE, 
                                                header=T))["Location"], error = function(e) e),"error")){
      Sys.sleep(1)} 
    else {
        zipfile <- parseHTTPHeader(getURL(paste0(urlbase, path, query), 
                                   httpheader = c(Authorization = token), 
                                   ssl.verifypeer = FALSE, 
                                   header=T))["Location"]
    }
    
    info <- getURL(paste0(urlbase, path, query), 
                   httpheader = c(Authorization = token), 
                   ssl.verifypeer = FALSE,
                   header=T)
    header <- parseHTTPHeader(conv.response(info)[1])
    if (header["status"] == "202") {
        zipfile <- header["Location"]
        filename <- gsub(".zip","",rev(unlist(strsplit(zipfile,split="/")))[1])
        } else if (header["status" == "400"]) {
            Sys.sleep(2)
            zipfile <- header["Location"]
            filename <- gsub(".zip","",rev(unlist(strsplit(zipfile,split="/")))[1])
        } else {stop(paste("Error in HTTP request:",header["status"], header["statusMessage"]),sep=" ")}
    
    ######## download history zip file
    temp=tempfile()
    download.file(zipfile,temp, quiet = T)
    con=unz(temp,filename)
    info=fromJSON(readLines(con), flatten = T)
    info=conv.num(info)
    close(con)
    unlink(temp)
    
    ######## Create history by transaction type
    type <- names(table(info$type))
    output=list()
    for (i in (1:length(type))) {
        df=info[which(info$type==type[i]),]
        keep=check.na.df(df,2)
        output[[i]]=df[,c(keep)]
        names(output)[i]=type[i]
    }
    
    if (write==T) {
        for (i in (1:length(type))) {
            filename <- paste0(outdir,id,"_history_",type[i],".csv")
            if(file.exists(filename)) {append=T} else {append=F}
            write.table(output[[i]], file=filename, append = append, quote=F, sep=",", row.names = F, col.names = !append)
        }
    }
    return(output)
}


###########################################################
########      Download balance and NAV snapshot    ########
########          Endpoint: Account; Path:         ########
###########################################################
track.balance <- function (write=F,
                           outfile="temp_nav.csv"){
    ######## Initial setup
    path <- ""
    endpoint <- "account"
    urlbase <- conv.urlbase(endpoint)
    
    ######## Query to retrive HTTP response and flag error
    query <- ""
    info <- getURL(paste0(urlbase, path, query), 
                   httpheader = c(Authorization = token), 
                   ssl.verifypeer = FALSE,
                   header=T)
    header <- parseHTTPHeader(conv.response(info)[1])
    if (header["status"] == "201" | header["status"] == "200") {
        body <- fromJSON(conv.response(info)[2])
    } else {stop(paste("Error in HTTP request:",header["status"], header["statusMessage"]),sep=" ")}
    
    ######## download and output balance snapshot
    date <- data.frame(Date=paste(Sys.Date()),stringsAsFactors=F)
    data <- as.data.frame(body,stringsAsFactors=F)
    output <- cbind.data.frame(date, data)
    output$NAV <- output$balance + output$unrealizedPl
    output <- conv.num(output)
    output$rsp_time <- Sys.time()
    
    if (write==T) {
        if (is.null(outfile)) {stop("Please Specigy Output File Name")} else {
            if(file.exists(outfile)) {
                write.table(output, file=outfile, append = T, quote=F, sep=",", row.names = F, col.names = F)
            } else {write.table(output, file=outfile, quote=F, sep=",", row.names = F)}
        }
    }
    return(output)
}


##########################################################
########     Get a list of all open positions     ######## 
########    Endpoint: Position; Path: positions   ########
##########################################################
list.position <- function() {
    
    ######## Initial setup
    path="/positions"
    endpoint="position"
    urlbase <- conv.urlbase(endpoint)
    
    ######## Query to retrive HTTP response and flag error
    query <- ""
    
    h <- basicHeaderGatherer()
    info <- getURL(paste0(urlbase,path,query), 
                   httpheader = c(Authorization = token,Connection="Keep-Alive"), 
                   ssl.verifypeer = FALSE,
                   headerfunction = h$update
    )
    header <- h$value()
    
    if (header["status"] == "201" | header["status"] == "200") {
        body <- fromJSON(conv.response(info))
    } else {
        stop(paste("Error in HTTP request:",header["status"], header["statusMessage"]),sep=" ")
    }
    
    ######## Output result
    data <- body[["positions"]]
    data$exposure <- exposureUSD(data[,1],data[,2],data[,3])
    price <- list.price(instrument = paste(unique(data[,1]),sep=","))
    output <- data %>%
        left_join(price, by=c("instrument"="instrument")) %>%
        rename(price_dt=time) %>%
        mutate(PL=(bid-avgPrice)*(side=="buy")+(-ask+avgPrice)*(side=="sell")) %>%
        mutate(PL_pct=PL/avgPrice*100) %>%
        arrange(exposure)
    
    return(output)
}


####################################################
########    Get a list of all open trades   ######## 
########    Endpoint: Trade; Path: trades   ########
####################################################
list.trade <- function() {
    
    ######## Initial setup
    path="/trades"
    endpoint="trade"
    urlbase <- conv.urlbase(endpoint)
    
    ######## Query to retrive HTTP response and flag error
    query <- ""
    
    h <- basicHeaderGatherer()
    info <- getURL(paste0(urlbase,path,query), 
                   httpheader = c(Authorization = token,Connection="Keep-Alive"), 
                   ssl.verifypeer = FALSE,
                   headerfunction = h$update
    )
    header <- h$value()
    
    if (header["status"] == "201" | header["status"] == "200") {
        body <- fromJSON(conv.response(info))
    } else {
        stop(paste("Error in HTTP request:",header["status"], header["statusMessage"]),sep=" ")
    }
    
    ######## Output result
    data <- body[["trades"]]
    data$exposure <- exposureUSD(data[,4],data[,2],data[,3])
    price <- list.price(instrument = paste(unique(data[,"instrument"]),sep=","))
    output <- data %>%
        rename(trade_dt=time) %>%
        left_join(price, by=c("instrument"="instrument")) %>%
        rename(price_dt=time) %>%
        mutate(PL=(bid-price)*(side=="buy")+(-ask+price)*(side=="sell")) %>%
        mutate(PL_pct=PL/price*100) %>%
        arrange(instrument,id)
    
    return(output)
}


#################################################################
######## Returns up to 1 year worth of economic calendar ########
########        information relevant to an instrument    ########
########          Endpoint: Lab; Path: calender          ########
#################################################################
track.calendar <- function(instrument = NULL, 
                          period = 31536000,
                          write = F,
                          outfile = "temp_calendar.csv") {
    ######## Initial setup
    path <- "/calendar?"
    endpoint <- "lab"
    urlbase <- conv.urlbase(endpoint)
    
    ######## Currency Instrument (Optional)
    if (length(instrument)>0) {
        instrument <- conv.pair(instrument)
        par.instrument <- paste0("instrument=", instrument)
    } else {par.instrument <- NULL}
    
    ######## Period (Required)
    if (is.null(period)) {stop("Time Period is required")} else {
        if (length(period) > 1) {stop("Only One Time Period Can Be Retrived")} else {
            if (period %in% c(3600,43200,86400,604800,2592000,7776000,15552000,31536000)) {par.period <- paste0("period=", period)} else {
                stop("Invalid Granularity Value. Please select one of the following: 
                     3600 - 1 hour
                     43200 - 12 hours
                     86400 - 1 day
                     604800 - 1 week
                     2592000 - 1 month
                     7776000 - 3 months
                     15552000 - 6 months
                     31536000 - 1 year")
            }
        }
    }
    
    ######## Query to retrive HTTP response and flag error
    query <- paste(c(par.instrument,par.period),collapse = "&")
    
    h <- basicHeaderGatherer()
    info <- (getURL(paste0(urlbase,path,query), 
                    httpheader = c(Authorization = token), 
                    ssl.verifypeer = FALSE,
                    headerfunction = h$update
                    )
             )
    header <- h$value()
    
    if (header["status"] == "201" | header["status"] == "200") {
        body <- fromJSON(conv.response(info))
    } else {stop(paste("Error in HTTP request:",header["status"], header["statusMessage"]),sep=" ")}
    
    ######## Output result
    
    date <- data.frame(news_date=paste(as.POSIXct(body$timestamp, origin="1970-01-01")),stringsAsFactors=F)
    data <- as.data.frame(body, stringsAsFactors=F)[,c("currency",
                                                       "title",
                                                       "region",
                                                       "unit",
                                                       "previous",
                                                       "forecast",
                                                       "actual",
                                                       "market",
                                                       "impact",
                                                       "timestamp")]
    
    output <- cbind.data.frame(date, data)
    output <- conv.num(output)
    
    if (write==T) {
        if (is.null(outfile)) {stop("Please Specigy Output File Name")} else {
            if(file.exists(outfile)) {
                output <- append.csv(outfile,output)
                write.table(output, file=outfile, append = F, quote=F, sep=",", row.names = F, col.names = T)
            } else {write.table(output, file=outfile, quote=F, sep=",", row.names = F)}
        }
    }
    return(output)
}


##########################################################################
######## Returns up to 1 year worth of historical position ratios ######## 
########                for a supported instrument                ########
########     Endpoint: Lab; Path: historical_position_ratios      ########
##########################################################################
track.long.ratio <- function(instrument = NULL, 
                            period = 86400) {
  
  ######## Initial setup
  path <- "/historical_position_ratios?"
  endpoint <- "lab"
  urlbase <- conv.urlbase(endpoint)
  
  support.pair.list<-c("AUD_JPY", "AUD_USD", 
                       "EUR_AUD", "EUR_CHF", "EUR_GBP", "EUR_JPY", "EUR_USD", 
                       "GBP_CHF", "GBP_JPY", "GBP_USD", 
                       "NZD_USD", "USD_CAD", "USD_CHF", "USD_JPY", "XAU_USD", "XAG_USD")
  
  ######## Currency Instrument (Required)
  if (is.null(instrument)) {stop("Instrument is required")} else {
    if (length(instrument)>1) {stop("Only one Currency Pair Are Allowed")} else {
      instrument <- conv.pair(instrument)
      if (instrument %in% support.pair.list) {
        par.instrument <- paste0("instrument=", instrument)
      } else { 
        stop("Invalid Currency Pair. Please select one of the following:
             AUD_JPY, AUD_USD,
             EUR_AUD, EUR_CHF, EUR_GBP, EUR_JPY, EUR_USD,
             GBP_CHF, GBP_JPY, GBP_USD,
             NZD_USD, USD_CAD, USD_CHF, USD_JPY, XAU_USD, XAG_USD")
      }
    }
  }
  
  ######## Period (Required)
  if (is.null(period)) {stop("Time Period is required")} else {
    if (length(period) > 1) {stop("Only One Time Period Can Be Retrived")} else {
      if (period %in% c(86400,172800,604800,2592000,7776000,15552000,31536000)) {par.period <- paste0("period=", period)} else {
        stop("Invalid Granularity Value. Please select one of the following:",support.pair.list)
      }
    }
  }
  
  ######## Query to retrive HTTP response and flag error
  query <- paste(c(par.instrument,par.period),collapse = "&")
  
  h <- basicHeaderGatherer()
  info <- (getURL(paste0(urlbase,path,query), 
                  httpheader = c(Authorization = token,Connection="Keep-Alive"), 
                  ssl.verifypeer = FALSE,
                  headerfunction = h$update
                  )
           )
  header <- h$value()
  
  if (header["status"] == "201" | header["status"] == "200") {
    body <- fromJSON(conv.response(info))
  } else {
    stop(paste("Error in HTTP request:",header["status"], header["statusMessage"]),sep=" ")
  }
  
  ######## Output result
  data <- as.data.frame(body, stringsAsFactors=F)
  colnames(data) <- c("currency","timestamp","long_position_ratio","price")
  data <- data[,c("currency","long_position_ratio","price","timestamp")]  
  date <- data.frame(time=paste(as.POSIXct(data$timestamp, origin="1970-01-01")),stringsAsFactors=F)
  output <- cbind.data.frame(date, data)
  output <- conv.num(output)
  return(output)
}


###################################################################
######## Returns up to 1 year worth of spread information  ######## 
########             for a supported instrument            ########
########            Endpoint: Lab; Path: spreads           ########
###################################################################
track.spread <- function(instrument = NULL, 
                         period = 31536000,
                         unique = 0) {
  
  ######## Initial setup
  path <- "/spreads?"
  endpoint <- "lab"
  urlbase <- conv.urlbase(endpoint)
  
  support.pair.list<-c(list.pair()$instrument,"XAU_USD", "XAG_USD")
  
  ######## Currency Instrument (Required)
  if (is.null(instrument)) {stop("Instrument is required")} else {
    if (length(instrument)>1) {stop("Only one Currency Pair Are Allowed")} else {
      instrument <- conv.pair(instrument)
      if (instrument %in% support.pair.list) {
        par.instrument <- paste0("instrument=", instrument)
      } else { 
        stop("Invalid Currency Pair. Please select one of the following:",support.pair.list)
      }
    }
  }
  
  ######## Period (Required)
  if (is.null(period)) {stop("Time Period is required")} else {
    if (length(period) > 1) {stop("Only One Time Period Can Be Retrived")} else {
      if (period %in% c(3600,43200,86400,604800,2592000,7776000,15552000,31536000)) {par.period <- paste0("period=", period)} else {
        stop("Invalid Granularity Value. Please select one of the following: 
             3600 - 1 hour
             43200 - 12 hour
             86400 - 1 day
             604800 - 1 week
             2592000 - 1 month
             7776000 - 3 months
             15552000 - 6 months
             31536000 - 1 year")
      }
    }
  }
  
  ######## Currency Instrument (Required)
  par.unique <- paste0("unique=", unique)
        
  ######## Query to retrive HTTP response and flag error
  query <- paste(c(par.instrument,par.period,par.unique),collapse = "&")
  
  h <- basicHeaderGatherer()
  info <- (getURL(paste0(urlbase,path,query), 
                  httpheader = c(Authorization = token,Connection="Keep-Alive"), 
                  ssl.verifypeer = FALSE,
                  headerfunction = h$update
                  )
           )
  header <- h$value()
  
  if (header["status"] == "201" | header["status"] == "200") {
    body <- fromJSON(conv.response(info))
  } else {
    stop(paste("Error in HTTP request:",header["status"], header["statusMessage"]),sep=" ")
  }
  
  ######## Output result
  avg <- as.data.frame(body$avg, stringsAsFactors=F)
  min <- as.data.frame(body$min, stringsAsFactors=F)
  max <- as.data.frame(body$max, stringsAsFactors=F)
  
  
  data <- mutate(avg,currency=paste0(instrument)) %>%
          select(currency,V1,V2) %>%
          left_join(min, by=c("V1"="V1")) %>%
          left_join(max, by=c("V1"="V1"))
  
  colnames(data) <- c("currency","timestamp","avg","min","max")
  date <- data.frame(time=paste(as.POSIXct(data$timestamp, origin="1970-01-01")),stringsAsFactors=F)
  output <- cbind.data.frame(date, data)
  output <- conv.num(output)
  return(output)
}


###############################################
########       Create a new order      ########
######## Endpoint: order; Path: orders ########
###############################################
post.order <- function(instrument = NULL, 
                       unit = NULL,
                       side = NULL,
                       type = NULL,
                       expiry = NULL,
                       price = NULL,
                       lowerBound = NULL,
                       upperBound = NULL, 
                       stopLoss = NULL,
                       takeProfit = NULL, 
                       trailingStop = NULL) {
  
  ######## Initial setup
  path <- "/orders"
  endpoint <- "order"
  urlbase <- conv.urlbase(endpoint)
  
  support.pair.list<-c(list.pair()$instrument)
  
  ######## Currency Instrument (Required)
  if (is.null(instrument)) {stop("Instrument is required")} else {
    if (length(instrument)>1) {stop("Only One Currency Pair Are Allowed")} else {
      instrument <- conv.pair(instrument)
      if (instrument %in% support.pair.list) {
        par.instrument <- paste0("instrument=", instrument)
      } else {
        stop("Invalid Currency Pair. Please select one of the following:",support.pair.list)
      }
    }
  }
  
  ######## Units (Required)
  if (is.null(unit)) {stop("Order Unit is required")} else {
    if (length(unit)>1) {stop("Only One Order Units Input Are Allowed")} else {
      par.unit <- paste0("units=", unit)
    }
  }
  
  ######## Side (Required)
  if (is.null(side)) {stop("Oder Side is required")} else {
    if (length(side)>1) {stop("Only One Order Side Input Are Allowed")} else {
      if (toupper(side) %in% c("BUY","SELL")) {
        par.side <- paste0("side=",side)
      } else {
        stop("Invalid order side. Please select either buy or sell")
      }
    }
  }
  
  ######## Order Type (Required)
  if (is.null(type)) {stop("Order Type is required")} else {
    if (length(type) > 1) {stop("Only One Order Type Are Allowed")} else {
      if (toupper(type) %in% c("LIMIT", "STOP", "MARKETIFTOUCHED", "MARKET")) {
        par.type <- paste0("type=", type)
      } else {
        stop("Invalid Order Type. Please select one of the following: LIMIT, STOP, MARKETIFTOUCHED, MARKET")
      }
    }
  }
  
  ######## Expiry (Required, datetime value in the format of YYYYMMDD-HHMMSS, default=NULL)
  if (is.null(expiry)) {stop("Order Expiry is required")} else {
    if (length(expiry) > 1) {stop("Only One Order Expiry Are Allowed")} else {
      par.expiry <- paste0("expiry=", conv.datetime(expiry))
    }
  }
  
  ######## Price (Required, NULL if Order Type = Market)
  if (toupper(type == "MARKET")) {
    par.price <- NULL
  } else {
    if (is.null(price)) {stop("Order Price is required")
    } else {
      if (length(price) > 1) {stop("Only One Order Price Are Allowed")
      } else {
        par.price <- paste0("price=", price)
      }
    }
  }
  
  ######## LowerBound (Optional, in price)
  if (length(lowerBound)>1) {stop("Only One Order lowerBound Input Are Allowed")} else {
    if (length(lowerBound)==1) {
      par.lowerBound <- paste0("lowerBound=", lowerBound)
    } else {par.lowerBound <- NULL}
  }
  
  ######## UpperBound (Optional, in price)
  if (length(upperBound)>1) {stop("Only One Order upperBound Input Are Allowed")} else {
    if (length(upperBound)==1) {
      par.upperBound <- paste0("upperBound=", upperBound)
    } else {par.upperBound <- NULL}
  }
  
  ######## StopLoss (Optional, in price)
  if (length(stopLoss)>1) {stop("Only One Order stopLoss Input Are Allowed")} else {
    if (length(stopLoss)==1) {
      par.stopLoss <- paste0("stopLoss=", stopLoss)
    } else {par.stopLoss <- NULL}
  }
  
  ######## TakeProfit (Optional, in price)
  if (length(takeProfit)>1) {stop("Only One Order takeProfit Input Are Allowed")} else {
    if (length(takeProfit)==1) {
      par.takeProfit <- paste0("takeProfit=", takeProfit)
    } else {par.takeProfit <- NULL}
  }
  
  ######## TrailingStop (Optional, in pips, up to 1 decimal)
  if (length(trailingStop)>1) {stop("Only One Order trailingStop Input Are Allowed")} else {
    if (length(trailingStop)==1) {
      par.trailingStop <- paste0("trailingStop=", round(trailingStop,1))
    } else {par.trailingStop <- NULL}
  }
  
  ######## Query to POST HTTP response and flag error
  query <- paste(c(par.instrument, par.unit, par.side, par.type, par.expiry, par.price, 
                   par.lowerBound, par.upperBound, par.stopLoss, par.takeProfit, par.trailingStop),
                 collapse = "&")
  
  info <- POST(url=paste0(urlbase,path),
               body=query,
               add_headers(c(Authorization = token,
                             "Content-Type"="application/x-www-form-urlencoded"))
  )
  
  if (info["status_code"] == "201" | info["status_code"] == "200") {
    body <- content(info)
  } else {stop(paste("Error in HTTP request:",info["status_code"], message_for_status(info)),sep=" ")}
  
  ######## Output result
  data <- as.data.frame(body, stringsAsFactors=F)
  output <- conv.num(data)
  return(output)
}



