#===================================================================
#                             Server Code
#9/9/2019 - JAK
#11/9/2019 - JAK - Moved all URL's and connectivity details to config.R
#
#####################################################################


library(shiny)
library(plotly)
library(shinydashboard)
library(httr)
library(DT)
library(reshape2)
library(shinyjs)
library(ggplot2)

source("config.R")
source("CloudDBFunctions.R")
source("functions.R")
source("plots.R")



#Define server logic required to draw a histogram

shinyServer(function(input, output, session) {
  
  watchlistdata <- data.table::data.table()
  alldata <- data.table::data.table()
  quoterowsglobal <- data.table::data.table()
  inforowsglobal <- data.table::data.table()
  newsrowsglobal <- data.table::data.table()
  erngsrowsglobal <- data.table::data.table()
  finrowsglobal <- data.table::data.table()
  watchlistsymbols <- data.table::data.table()
  sectorinfo <- data.table::data.table()
  statsrowsglobal <- data.table::data.table()
  
  ###New Function to get all data into R##########
  quotes <- getYTDQuotes()
  quoterowsglobal <<- quotes
  watchlistdata <<- quoterowsglobal
  statsrowsglobal <- getAllStats()
  print( nrow(statsrowsglobal))
  

  ##########Get Watchlists From Cloudant###################
  # Removed this code
  #########################################
  
  ###########Watchlist ChangeEvent###################
  # Removed Code
  ####################################
  
  
  #######Open Settings Tab #############
  observeEvent(eventExpr = input$settings, {
    updateTabsetPanel(session, inputId = "tabs", selected = "5" )
    
  }) ###Clicked on Settings
  
  
  ##################Set Default Symbols in Settings #######################
  observeEvent(eventExpr = input$sectorsettings, {
    selectedsector <- trimws(input$sectorsettings)
    print(selectedsector)
    
    if ( is.null(selectedsector) || selectedsector == "" )   {
      return()
    } else {
      sectorinfo <<- getSymbols(selectedsector)
      updateTextInput(session, inputId = "sectorname", value = selectedsector  )
      updateTextInput(session, inputId = "defaultsymbols", value = sectorinfo$symbols  )
    }
  } )
  

  
  
  ######CreateWatchList########
  # Section to create watchlists
  # Fill in the code here
  #
  ########Create Watchlist######
  


  ###########Get Plot#################
  getPlot <- function(flag) {  
    
    # progress <- Progress$new(session, min=1, max=60) 
    #       on.exit(progress$close())
    # 
    # progress$set(message = 'Creating Stock Plots',
    #              detail = 'This may take upto 1 min...')
    
    percent <- as.numeric(input$percent)
    term <- input$month
    
    if(flag == "Sector") {
      sectors <- trimws(input$sector)
      if ( sectors == "" || is.null(sectors) ) {
        return()
      } else {
        # sectorinfo <<- getSymbols("Finance-Tech")
        sectorinfo <<- getSymbols(sectors)
        symbs <- sectorinfo
      }
    }
    if(flag == "Watchlist") {
      sectors <- trimws(input$watchlists)
      symbs <- watchlistsymbols
    }

    # sectorinfo <<- getSymbols("Healthcare")
    # percent <- 5
    # term <- "3m"
    symbs <- data.frame(sort(symbs$symbols))
    if( trimws(symbs) == "" || length(symbs) == 0 ) {
      return()
    }
    
    # symbs <- data.frame(list(stringr::str_split(symbs, ",")))
    colnames(symbs) <- c("symbol")
    
    
    { ###Get Earnings, News, Financials
      # newsrowsglobal <<- getAllNews(symbs, term)
      erngsrowsglobal <<- getAllErngs(symbs)
      # finrowsglobal <<- getAllFins(symbs)
      # inforowsglobal <<- getAllInfo(symbs, term)
    }
    
    

    {  ######Filter Data     
      quotes <- quotes[(Symbol %in% symbs$symbol) ]
      maxdate1  <- tail( quotes [, (Date)] ,1)
      if(term == "1m") {
        mindate1 <- maxdate1 - 30
      }
      if(term == "3m") { 
        mindate1 <- maxdate1 - 90
      }
      if(term == "6m") { 
        mindate1 <- maxdate1 - 180
      }
      
      # percent=""
      if (is.na(percent) || percent == "") {
        data <- unique( quotes[ (Date >= maxdate1 ), Symbol])
      } else {
        data <- unique( quotes[ (Date >= maxdate1 ) & (YTDChangePct > percent ), Symbol])
      }
      quotes <- quotes[ ( Symbol %in% data) & (Date>= c(mindate1)) ]
    } ## Filter Data
    
    
    output$plots <- renderPlot({
      withProgress( min = 1, max=10, message = "Creating Charts", {
        n<-10
        incProgress(1/n, detail = paste("Doing part", ++n ))
        Sys.sleep(0.1)
      }) ##end of withProgress
      
      withProgress( min = 1, max=10, message = "Creating Charts", {
        incProgress(1/n, detail = paste("Doing part", n))
        ###########################
        plotChart(quotes, percent)
        
      })
      
    })  ##renderplot
    
    
    ##########Setting Output Elements #####
    output$tickers <- shiny::renderUI( {
      DT::dataTableOutput(outputId = "tickertbl")
    })
    
    output$txtwatchlistname <- shiny::renderUI( {
      shiny::textInput(inputId = "watchlistname", label = "Watchlist Name", value = "", width = "200px")
    })
    
    output$btnwatchlist <- shiny::renderUI( {
      shiny::actionButton(inputId = "createwatchlist", label = "Create Watchlist")
    })
    
    
    # ##########Output TickerTable################
    # output$tickertbl <- DT::renderDataTable({
    #   ### watchlistdata <- unique( data.frame(quotes$index, quotes$name, quotes$Industry, quotes$Sector))
    #   # watchlistdata <- unique( data.frame(watchlistdata$index, watchlistdata$name, watchlistdata$Industry, watchlistdata$Sector))
    #   # colnames(watchlistdata) <- c("Index","Name","Industry","Sector")
    # 
    #   rownames(watchlistdata) <- NULL
    #   watchlistdata <<- watchlistdata[ order(watchlistdata$Index), ]
    #   watchlistdata <- watchlistdata[ order(watchlistdata$Index), ]
    #   
    #   # colnames(rows2) <- c("Index","Name","Sector","MarketCap_Bil", "YtdChange(%)")
    #   
    #   tbl <- DT::datatable(watchlistdata, filter = list("position"="none"), width="200px", height = "200px", selection = list("mode"="multiple", target="row"), rownames = FALSE, options = list(orderClasses = TRUE))
    #   
    #   DT::formatCurrency(table = tbl, columns = "MarketCap_Bil", currency = "$ ", interval = 3, mark = ",", digits = 2, before = TRUE  ) %>%
    #     DT::formatRound( columns = "YtdChange(%)", digits=2 )
    # 
    # }) 
    # 
    #####################Output News##############
    output$newsoutput <- renderDataTable( {
      
      newstbl <- DT::datatable(newsrowsglobal, filter = list("position"="none"), height = "200px", selection = list("mode"="none"), rownames = FALSE)
      # options = list(
      #   autoWidth = TRUE, columnDefs = list(list(width="20%", targets=4) )))
      #   # columnDefs = list(list(width = '10%', targets = 1),====
      #                   list(width = '20%', targets = 2),
      #                   list(width = '50%', targets = 3), 
      #                   list(width = '20%', targets = 4)) ))
      
      
      # DT::formatStyle(table = newstbl, columns = "Date", width= "10px")%>%
      # DT::formatStyle(columns = "Headline", width= "50px")%>%
      # DT::formatStyle(columns = "Summary", width="150px")%>%
      # DT::formatStyle(columns = "Related", width="50px") 
    })
    
    output$erngsdataoutput <- renderDataTable({
      erngstbl <- DT::datatable(erngsrowsglobal, filter = list("position"="none"), height = "200px", selection = list("mode"="none"), rownames = FALSE)
    })  ##Earnings Output
    
    output$erngsplotoutput <- renderPlot ({
      plotErngs(erngsrowsglobal)
    } )  ##Earnings Plots
    
    output$findataoutput <- renderDataTable({
      erngstbl <- DT::datatable(finrowsglobal, filter = list("position"="none"), height = "200px", selection = list("mode"="none"), rownames = FALSE)
    })  ##Financials Output
    
    ##8/30/2019
    ##Changed this to a Stats Plot
    print( nrow(statsrowsglobal))
    output$statsoutput <- renderDataTable({
      erngstbl <- DT::datatable(statsrowsglobal, filter = list("position"="none"), height = "200px", selection = list("mode"="none"), rownames = FALSE)
    })  ##Stats Output
    
    
  }  ###GetPlot Function
  #################
  
  observeEvent(eventExpr = input$sector, {
    shinyjs::hideElement(id = "sidebarCollapsed", anim = TRUE, animType = "slide",time = 1 )
    getPlot("Sector")
  })
  
  
  ##############Save Changes to Sectors###############
  observeEvent(eventExpr = input$savesymbols, {
    
    sectorname <- input$sectorsettings
    defaultsymbols <- input$defaultsymbols
    id <- sectorinfo$id
    rev <- sectorinfo$rev
    sectorsavequery <- paste( " { \"doc\" : {  \n \"_id\":\"",id,"\", \n  \"_rev\":\"",rev,"\", \n \"sector\":\"",sectorname,"\", \n \"symbols\":\"",defaultsymbols,"\" } \n  }  ", sep="" )
    
    token_request <- POST (url = sectorurlpost, use_proxy(NULL), body=sectorsavequery,
                           add_headers(.headers= c("X-IBM-Client-Id"= savesector_apikey,"Accept"="application/json", "Content-Type"="application/json") ) ) 
    
    token_body <- content(token_request, as = 'parsed')
    print(token_body)
    
    ##Retrieve values after saving to get the latest _rev value
    sectorinfo <<- getSymbols(sectorname)
    
  } ) ##Save Sector changes
  
  
})##########Server Function
