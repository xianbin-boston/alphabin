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
            require(p[i],quiet=T,character.only=T)
        }
    }
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


################################################################
######## Convert possible numeric value in a Data Frame ########
################################################################
conv.num <- function(data){
    output <- data
    for (i in 1:ncol(data)) {
        flag <- tryCatch({as.numeric(data[,i])},
                         warning = function(war){war},
                         error = function(err){err}
        )
        if (!(inherits(flag,"error") | inherits(flag,"warning"))) {
            output[,i]=as.numeric(data[,i])}
    }
    return(output)
}


##########################################################
######## Check Resaling Price History for a Book  ########
##########################################################
resale.price.history <- function(isbn13) {
    url=paste0("https://bookscouter.com/historic/",isbn13)
    a<- GET(url)
    doc=content(a,as="text")
    
    price <- readHTMLTable(doc,stringsAsFactors = FALSE)
    
    position <- regexpr("<h2>[[:print:]]*<\\/h2>", doc)
    start <- position[1]-1+4
    end <- position[1]+attr(position, "match.length")-1-5
    title <- substr(doc, start + 1, end)
    
    position <- regexpr("Author: [[:print:]]*<br", doc)
    start <- position[1]-1+8
    end <- position[1]+attr(position, "match.length")-1-3
    author <- substr(doc, start + 1, end)
    
    output <-  conv.num(price[[1]])
    output[,1] <- ymd(output[,1])
    colnames(output) <- c("Date","Max_Price","Avg_Price","N_Prices")
    return(output)
}


################################################
######## Frequency Table of a variable  ########
################################################
freq_tb <- function(in_df) {
    output <- cbind(Level=unlist(attr(table(univ$lrnval),"dimnames")),
                    Freq=table(in_df), 
                    CumFreq=cumsum(table(in_df)), 
                    Pct=prop.table(table(in_df)), 
                    CumPct=cumsum(prop.table(table(in_df)))
    )
    output <- conv.num(data.frame(output))
    return(output)
}