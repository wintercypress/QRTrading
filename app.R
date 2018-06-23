# Load packages ----
library(shiny)
library(DT)
library(dygraphs)

# Source file ----
source("functions.R")


# User interface ----
ui <- fluidPage(
        
  titlePanel("S&P500 Index Members Query"),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("symb",
                  label = 'Select Stock Ticker',
                  choices = sp500,
                  selected = 'AAPL'
                  ),
      
      dateRangeInput("dates", 
                     "Date range",
                     start = as.character(Sys.Date()-7), 
                     end = as.character(Sys.Date())),
      
      br()
 
    ),
    
    mainPanel(
            
        h2('Candlestick Chart'),
        
        br(),
            
        dygraphOutput("plot"),
              
        br(),
        
        h2('Pairwise Correlation betwwen Query Stock and Its Peers'),
        p('- Define the five stocks with the highest return correlation with the query stock as its peers'),
        
        br(),
        
        dataTableOutput("table")
              )
 
  )
)

# Server logic
server <- function(input, output) {
  
  query_stock <- reactive({
         
          prc_tibble <- get_prc(input$symb, start = input$dates[1], end = input$dates[2])
          prc_df <- prc_tibble %>% as.data.frame()
          prc_xts <- xts(prc_df[,-1], order.by = prc_df[,1])
          
  })
  
  output$plot <- renderDygraph({
          dygraph(query_stock()[,1:4]) %>% dyCandlestick() %>% dyLegend(width = 400)
          

  })

  sp500_corr <- reactive({

          corr_table <- all_corr(start = input$dates[1], end = input$dates[2])


  })
  
  #If date range has been changed, rerun SP500 correlation again, o/w filter data from corr_table by symbol
  output$table <- renderDataTable({
         peers_corr <- corr_func(input$symb, sp500_corr())
         datatable(peers_corr, 
                   rownames = FALSE,
                   colnames = capitalize(colnames(peers_corr)),
                   options = list(dom = 't', 
                                  columnDefs = list(list(className = 'dt-center', targets = 0:6))
                                  )
                  ) %>% formatStyle('symbol', fontWeight = 'bold')
          
  })
}

# Run the app
shinyApp(ui, server)
