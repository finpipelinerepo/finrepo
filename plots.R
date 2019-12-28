#
# July 2019 - JAK
#  Plots
# Oct 2019 - JAK
#  
#
################################################

library(httr)
library(ggplot2)


####### Plot Price data set  ######################
plotChart <- function(plotData, percent) {
  # plotData <- quoterowsglobal
  # percent <- 8
  
  # mindate1  <- head( plotData [, (Date)] ,1)
  maxdate1  <- tail( plotData [, (Date)] ,1)
  if( is.na(percent) == TRUE) {
    percent <- 0
  }
  
  if ( is.null(percent) == FALSE && trimws(percent) != "" ) {
    symbs <- plotData[ (YTDChangePct >= percent) & ( Date >= maxdate1), (Symbol) ]
    plotData <- plotData[(Symbol %in% c(symbs)), ]
  } 
  
  tplot <- ggplot(plotData, aes(x=Date, y=YTDChangePct, color=Symbol, group=Symbol )  ) + geom_line(size=0.7)
  tplot <- tplot + geom_smooth( method = "glm", se=TRUE)
  tplot <- tplot + xlab("Date") + ylab("Percent Change")
  tplot <- tplot + facet_wrap(~Symbol, scales="free")
      # tplot <- plotly::ggplotly(tplot)

  return (tplot)
  
}

#####Plot Earnings and Financials#######################
plotErngs <- function(plotData) {

  erngs1 <- plotData[order(plotData$Symbol, plotData$fiscalEndDate),]
  tplot <- ggplot(data=erngs1, aes(x=erngs1$fiscalEndDate, y = erngs1$actualEPS, fill=erngs1$Symbol))
  tplot <- tplot + geom_bar(stat="identity")
  tplot <- tplot + xlab("Fiscal Period") + ylab("Actual EPS")
  tplot <- tplot + facet_wrap(~erngs1$Symbol, scales="free", ncol=4)
  # tplot <- plotly::ggplotly(tplot)
  return(tplot)
}


##############Plot Financials######################
plotFins <- function(fin) {
  
  tplot <- ggplot()
  fin1 <- fin[order(fin$Ticker, fin$Reportdate),]
  tplot <- tplot + geom_point( data=fin1, aes(x= fin1$Reportdate, y=fin1$Totalrevenue, color=fin1$Ticker, size=fin1$Cashflow) )
  tplot <- tplot + xlab("Fiscal Period") + ylab("Total Revenue")
  tplot <- tplot + facet_wrap(~ fin1$Ticker, scales="free", ncol = 4)
  # tplot <- plotly::ggplotly(tplot)
  return(tplot)
}

#####Plot Stats#######################
plotStats <- function(plotData) {
  
  tplot <- ggplot(data=plotData, aes(x=SYMBOL, y = erngs1$actualEPS, fill=erngs1$Symbol))

  plotData <- statsrows
  
  tplot <- ggplot(data=plotDat, aes(x=SYMBOL, y = erngs1$actualEPS, fill=erngs1$Symbol))
  tplot <- tplot + geom_bar(stat="identity")
  tplot <- tplot + xlab("Fiscal Period") + ylab("Actual EPS")
  tplot <- tplot + facet_wrap(~erngs1$Symbol, scales="free", ncol=4)
  # tplot <- plotly::ggplotly(tplot)
  return(tplot)
}
