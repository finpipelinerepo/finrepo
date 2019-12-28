#
# July 2019 - JAK
#       These functions call the Cloud Database API's
# Oct 2019 - JAK
#       removed hardcoding url's within the functions
# 
# 
################################################################


library(httr)

source("config.R")


##################Get YTD Quotes from DB#########################
getYTDQuotes <- function() {
  
  token_request <- httr::GET(url = getytdquotes_url, 
        add_headers(.headers= c("X-IBM-Client-Id"=getytdquotes_api_key, "Accept"="application/json","Content-Type"="application/json") ) )
  
  token_body <- httr::content(token_request, as = "parsed")
  token_body <- jsonlite::parse_json(token_request,simplifyVector = TRUE)
  df<- data.frame(token_body)
  df <- data.table::data.table(df)
  quoterows <- df[, c(8,7,6,3,4,5,9,1,2,10)]
  
  colnames(quoterows) <- c("Symbol","Date","Open","Close","High","Low","Volume","Change","ChangePercent","YTDChangePct")

  quoterows$Open <- as.numeric(quoterows$Open)
  quoterows$Close <- as.numeric(quoterows$Close)
  quoterows$High <- as.numeric(quoterows$High)
  quoterows$Low <- as.numeric(quoterows$Low)
  quoterows$Volume <- as.numeric(quoterows$Volume)
  quoterows$Date <- as.Date(quoterows$Date)
  quoterows$Change <- as.numeric(quoterows$Change)
  quoterows$ChangePercent <- as.numeric(quoterows$ChangePercent)
  quoterows$YTDChangePct <- as.numeric(quoterows$YTDChangePct)

  return(quoterows)
}  ## getYTDQuotes
##############Get YTD Quotes from DB########################


############Get All News from DB##############################
getAllNewsFromDB <- function() {
  
  token_request <- httr::GET(url = newsurl, 
                 add_headers(.headers= c("X-IBM-Client-Id"=news_api_key, "Accept"="application/json","Content-Type"="application/json") ) )
  
  token_body <- httr::content(token_request, as = "parsed")
  token_body <- jsonlite::parse_json(token_request,simplifyVector = TRUE)
  
  newsrows <- data.table::data.table()
  df<- data.frame(token_body)
  newsrows <- data.table::data.table(df)
  rows3 <- data.table::data.table()
  
  rows3 <- data.table::data.table()
  for(i in 1:length(newsrows)) {
    try( expr = {
      news1 <- data.frame(newsrows[[1]][[1]][, c("datetime","headline","summary")] )
      # format.Date( newsrows$AMGN$news$datetime, format="%b%d")
      news1$datetime <- format.Date( news1$datetime, format="%b%d")
      rows3 <- rbind(rows3, news1) 
    }, silent = TRUE)
  }
  newsrows <- rows3
  return(newsrows)
}
############Get All News from DB##############################


##################Get Earnings##########################
getAllErngsFromDB <- function() {

  token_request <- httr::GET(url = erngsurl,
               add_headers(.headers= c("X-IBM-Client-Id"=erngs_api_key, "Accept"="application/json","Content-Type"="application/json") ) )
  
  token_body <- httr::content(token_request, as = "parsed")
  token_body <- jsonlite::parse_json(token_request,simplifyVector = TRUE)
 
  erngsrows <- data.table::data.table()
  erngsrows<- data.frame(token_body)
  erngsrows <- data.table::data.table(erngsrows)
  colnames(erngsrows) <- c("actualEPS","consensusEPS","epsReportDate","epsSurpriseDollar","fiscalEndDate","fiscalPeriod","numberOfEstimates","Symbol", "yearAgo", "yearAgoChangePercent")
  
  erngsrows$epsReportDate <- as.Date(erngsrows$epsReportDate)
  erngsrows$fiscalEndDate <- as.Date(erngsrows$fiscalEndDate)
  erngsrows$yearAgoChangePercent <- round(as.numeric(erngsrows$yearAgoChangePercent) * 100, 2)
  return(erngsrows)
}

################Get Earnings##########################


##################Get All Stats ##########################
getAllStatsFromDB <- function() {

  token_request <- httr::GET(url = statsurl, 
            add_headers(.headers= c("X-IBM-Client-Id"=stats_api_key, "Accept"="application/json","Content-Type"="application/json") ) )
  
  token_body <- httr::content(token_request, as = "parsed")
  token_body <- jsonlite::parse_json(token_request,simplifyVector = TRUE)
  
  statsrows <- data.table::data.table()
  statsrows<- data.frame(token_body)
  statsrows <- data.table::data.table(statsrows)
  colnames(statsrows) <- c("AVG10VOLUME", "AVG30VOLUME","BETA",
  "COMPANYNAME","DAY200MOVINGAVG","DAY30CHANGEPERCENT", "DAY50MOVINGAVG", "DAY5CHANGEPERCENT","DIVIDENDYIELD",
  "MARKETCAP","MAXCHANGEPERCENT", "MONTH1CHANGEPERCENT","MONTH3CHANGEPERCENT","MONTH6CHANGEPERCENT",
  "NEXTDIVIDENDDATE", "NEXTEARNINGSDATE", "PERATIO","SHARESFLOAT", "SHARESOUTSTANDING",
  "SYMBOL", "TTMDIVIDENDRATE",  "TTMEPS",  "WEEK52CHANGE",  "WEEK52HIGH",  "WEEK52LOW",  "YEAR1CHANGEPERCENT",
  "YEAR2CHANGEPERCENT",  "YEAR5CHANGEPERCENT",  "YTDCHANGEPERCENT")
  
  statsrows$AVG10VOLUME <- as.numeric(statsrows$AVG10VOLUME)
  statsrows$AVG30VOLUME <- as.numeric(statsrows$AVG30VOLUME)
  statsrows$BETA <- as.numeric(statsrows$BETA)
  statsrows$DAY200MOVINGAVG <- as.numeric(statsrows$DAY200MOVINGAVG)
  statsrows$DAY30CHANGEPERCENT <- as.numeric(statsrows$DAY30CHANGEPERCENT)
  statsrows$DAY50MOVINGAVG <- as.numeric(statsrows$DAY50MOVINGAVG)
  statsrows$DAY5CHANGEPERCENT <- as.numeric(statsrows$DAY5CHANGEPERCENT)
  statsrows$DIVIDENDYIELD <- as.numeric(statsrows$DIVIDENDYIELD)
  statsrows$MARKETCAP <- as.numeric(statsrows$MARKETCAP)
  statsrows$MAXCHANGEPERCENT <- as.numeric(statsrows$MAXCHANGEPERCENT)
  statsrows$MONTH1CHANGEPERCENT <- as.numeric(statsrows$MONTH1CHANGEPERCENT)
  statsrows$MONTH3CHANGEPERCENT <- as.numeric(statsrows$MONTH3CHANGEPERCENT)
  statsrows$MONTH6CHANGEPERCENT <- as.numeric(statsrows$MONTH6CHANGEPERCENT)
  statsrows$NEXTDIVIDENDDATE <- as.Date(statsrows$NEXTDIVIDENDDATE)
  statsrows$NEXTEARNINGSDATE <- as.Date(statsrows$NEXTEARNINGSDATE)
  statsrows$PERATIO <- as.numeric(statsrows$PERATIO)
  statsrows$SHARESFLOAT <- as.numeric(statsrows$SHARESFLOAT)
  statsrows$SHARESOUTSTANDING <- as.numeric(statsrows$SHARESOUTSTANDING)
  statsrows$TTMDIVIDENDRATE <- as.numeric(statsrows$TTMDIVIDENDRATE)
  statsrows$TTMEPS <- as.numeric(statsrows$TTMEPS)
  statsrows$WEEK52CHANGE <- as.numeric(statsrows$WEEK52CHANGE)
  statsrows$WEEK52HIGH <- as.numeric(statsrows$WEEK52HIGH)
  statsrows$WEEK52LOW <- as.numeric(statsrows$WEEK52LOW)
  statsrows$YEAR1CHANGEPERCENT <- as.numeric(statsrows$YEAR1CHANGEPERCENT)
  statsrows$YEAR2CHANGEPERCENT <- as.numeric(statsrows$YEAR2CHANGEPERCENT)
  statsrows$YEAR5CHANGEPERCENT <- as.numeric(statsrows$YEAR5CHANGEPERCENT)
  statsrows$YTDCHANGEPERCENT <- as.numeric(statsrows$YTDCHANGEPERCENT)
  
  return(statsrows)
}

################Get Stats##########################



