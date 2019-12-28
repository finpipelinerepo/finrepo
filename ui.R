
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# 7/22/2019 - JAK
#   Separated functions to another "functions" script
#
########################################################################


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


useShinyjs()


header <- dashboardHeader(title = "Stockszen", titleWidth = "15%")

sidebar <- dashboardSidebar( collapsed = TRUE, 
           shiny::uiOutput(outputId = "watchlist"),
           shiny::selectInput(inputId = "sector", choices = c(" ","Indexes","Healthcare","Tech-IT","Tech-Non-IT","Finance-Tech","Industrial-Defense-Cyber","Restaurants-Retail","Other"), selected = " ", label="Sector"),
           shiny::selectInput(inputId = "month", choices = c("1m","3m","6m"), selected = "3m", label="Number of Months"),
           shiny::textInput(inputId = "percent",value = "", label="Filter by percent growth"),
           shiny::textInput(inputId = "ticker",label = "Type your own tickers (max 20)",width = "100%"),
           shiny::actionButton(inputId = "Go", label = "Get Chart", width = "50%", ),
           shiny::actionButton(inputId = "settings", label = "Settings", width = "50%"))
 

body <- dashboardBody( 
  
  box( width = "100%",
       tabsetPanel(type= "pills", id = "tabs", 
                   
                   tabPanel(title = "Plots", value = "1", 
                            # plotlyOutput("plots" ),
                            plotOutput("plots"),
                            
                            shiny::uiOutput(outputId = "tickers"),
                            shiny::uiOutput(outputId = "txtwatchlistname"),
                            shiny::uiOutput(outputId = "btnwatchlist")
                            
                   ),
                   tabPanel(title = "Stats", value = "2", 
                            DT::dataTableOutput(outputId = "statsoutput" )
                            # plotOutput(outputId = "statsplot")
                            
                   ),
                   
                   tabPanel(title = "Earnings", value = "3",
                            # plotlyOutput(outputId = "erngsplotoutput"),
                            plotOutput(outputId = "erngsplotoutput", height = "600px" ),
                            DT::dataTableOutput(outputId = "erngsdataoutput" )
                   ),
                   
                   tabPanel(title = "Revenue & CashFlow", value = "4",
                            # plotlyOutput(outputId = "statsplotoutput"),
                            plotOutput(outputId = "statsplotoutput", height = "600px" ),
                            DT::dataTableOutput(outputId = "findataoutput" )
                   ),
                   
                   tabPanel(title = "Settings", value = "5", 
                            shiny::selectInput(inputId = "sectorsettings", choices = c(" ","Indexes","Healthcare","Tech-IT","Tech-Non-IT","Finance-Tech","Industrial-Defense-Cyber","Restaurants-Retail","Other"), selected = " ", label="Sector", width = 200), 
                            shiny::textInput(inputId = "sectorname", label = "Sector", value = "", width = "50%" , placeholder = "Healthcare" ),
                            shiny::textInput(inputId = "defaultsymbols", label = "Symbols", value = "", width = "50%" , placeholder = "AMGN,BIIB,AAPL,NFLX" ),
                            shiny::actionButton(inputId = "savesymbols", label = "Save", width = 50)
                            
                            # shiny::textInput(inputId = "id", label="", value = "", width = "50%" ),
                            # shiny::textInput(inputId = "rev", label="", value = "", width = "50%" )
                            
                   )
                   
       ) ##TabPanel
       
  ) ##Box
  
  
)  ##dashboardBody


# Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black

ui <- dashboardPage(header,  sidebar, body, title = "Stock Charts and Analysis")
