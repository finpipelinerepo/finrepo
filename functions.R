#
#7/31/2019 - JAK
## Get All Data Functions
# These functions call the Cloud DB Functions
#

################################################################

library(httr)

########Get Quotes ###############
getAllData <- function(symbols,term) {
  
  symbols <- toupper(symbols)
  
  # d <- as.data.frame( strsplit(x = symbols,split = "," ) ) 
  symbols <- data.frame(symbols)
  colnames(symbols) <- c("index")
  
  startnum <- 1
  endnum  <- dim(symbols)[1]
  
  s <- NULL
  for (i in startnum:endnum) {
    index <- trimws( symbols$index[i])
    s <- paste(s,index,sep="")
    if(i < endnum) {
      s <- paste(s,",",sep="")
    }
  }
  
  ##########Get Quotes from Functions.R#############
  quoterows <- getYTDQuotes()
  return(quoterows)
} ##Get All Data


#########Get All Information ###############
getAllInfo <- function(symbols,term) {
  
  symbols <- toupper(symbols)
  print(symbols)
  # d <- as.data.frame( strsplit(x = symbols,split = "," ) ) 
  symbols <- data.frame(symbols)
  colnames(symbols) <- c("index")
  
  startnum <- 1
  endnum  <- dim(symbols)[1]
  
  s <- NULL
  for (i in startnum:endnum) {
    index <- trimws( symbols$index[i])
    s <- paste(s,index,sep="")
    if(i < endnum) {
      s <- paste(s,",",sep="")
    } 
  }
  ###Get Information#########
  quoterowsglobal <- quoterows
  # rows2 <- quoterowsglobal[, c("Symbol","Date","CompanyName","Close","Volume","Change","ChangePercent","ChangeOverTime","PERatio", "MarketCap")]
  # //rows2$MarketCap <- as.numeric(rows2$MarketCap)/1000000
  inforows <- rows2
  inforowsglobal <<- inforows
  return(inforows)
}


##################Get All News ######################
getAllNews <- function(symbols,term) {
  
  symbols <- toupper(symbols)
  
  # d <- as.data.frame( strsplit(x = symbols,split = "," ) ) 
  symbols <- data.frame(symbols)
  colnames(symbols) <- c("index")
  
  startnum <- 1
  endnum  <- dim(symbols)[1]
  
  s <- NULL
  for (i in startnum:endnum) {
    index <- trimws( symbols$index[i])
    s <- paste(s,index,sep="")
    if(i < endnum) {
      s <- paste(s,",",sep="")
    }
  }
  
  ###############Get News from Functions.R##################
  
  newsrows <- getAllNewsFromDB()
  
  colnames(newsrows) <- c("Date","Headline","Summary")  
  newsrowsglobal <<- newsrows
  
  return(newsrows)
  
} ##End of AllNews


#####Get Earnings #############
getAllErngs <- function(symbols) {
  colnames(symbols) <- c("index")
  
  startnum <- 1
  endnum  <- dim(symbols)[1]
  
  s <- NULL
  for (i in startnum:endnum) {
    index <- trimws( symbols$index[i])
    s <- paste(s,index,sep="")
    if(i < endnum) {
      s <- paste(s,",",sep="")
    }
  }
  
  #######Get Earnings################
  erngsrows <- getAllErngsFromDB()
  erngsrows <- erngsrows[(Symbol %in% symbols$index) , ]

  return(erngsrows)
} ##Get All Earnings



###########Get Financials#####################
getAllFins <- function(symbols) {
  
  symbols <- toupper(symbols)
  
  # d <- as.data.frame( strsplit(x = symbols,split = "," ) ) 
  symbols <- data.frame(symbols)
  colnames(symbols) <- c("index")
  
  startnum <- 1
  endnum  <- dim(symbols)[1]
  
  s <- NULL
  for (i in startnum:endnum) {
    index <- trimws( symbols$index[i])
    s <- paste(s,index,sep="")
    if(i < endnum) {
      s <- paste(s,",",sep="")
    }
  }
  
  ###Get Financials
  # baseURL is in config.R
  getFinURL <- paste(baseURL,s,"&types=financials", sep="" )
  
  finrows <- data.table::data.table()
  try( finrows <- jsonlite::fromJSON(getFinURL, flatten = TRUE), silent=TRUE)
  
  fin <- data.table::data.table()
  for(i in 1:length(finrows)) {
    try( expr= {
      ticker <- finrows[[i]][[1]][[1]]
      
      r1 <- cbind(ticker, data.frame(finrows[[i]][[1]][["financials"]] ) )
      r1$reportDate <- as.Date(r1$reportDate)
      r1$grossProfit = round( r1$grossProfit/1000000,2)
      r1$costOfRevenue = round( r1$costOfRevenue/1000000,2)
      r1$operatingRevenue = round( r1$operatingRevenue/1000000,2)
      r1$totalRevenue = round( r1$totalRevenue/1000000,2)
      r1$operatingIncome = round( r1$operatingIncome/1000000,2)
      r1$netIncome = round( r1$netIncome/1000000,2)
      r1$researchAndDevelopment = round( r1$researchAndDevelopment/1000000,2)
      r1$operatingExpense = round( r1$operatingExpense/1000000,2)
      r1$currentAssets = round( r1$currentAssets/1000000,2)
      r1$totalAssets = round( r1$totalAssets/1000000,2)
      r1$totalLiabilities = round( r1$totalLiabilities/1000000,2)
      r1$currentCash = round( r1$currentCash/1000000,2)
      r1$currentDebt = round( r1$currentDebt/1000000,2)
      r1$totalCash = round( r1$totalCash/1000000,2)
      r1$totalDebt = round( r1$totalDebt/1000000,2)
      r1$shareholderEquity = round( r1$shareholderEquity/1000000,2)
      r1$cashChange = round( r1$cashChange/1000000,2)
      r1$cashFlow = round( r1$cashFlow/1000000,2)
      r1$operatingGainsLosses = round( r1$operatingGainsLosses/1000000,2)
      
      fin <- rbind(fin,r1) }, silent = TRUE)
    
  }
  
  colnames(fin) <- stringi::stri_trans_totitle(colnames(fin))
  
  finrowsglobal <<- fin
  
  return(fin)
} ##Get All Financials


###########Get All Stats #####################
getAllStats <- function() {

  statsTbl <- getAllStatsFromDB()
  return(statsTbl)
  
}
