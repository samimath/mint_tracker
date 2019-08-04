library(dplyr)
library(ggplot2)

ui <- pageWithSidebar(
  headerPanel("CSV Data explorer"),
  sidebarPanel(
    
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    
    htmlOutput("varselect", inline=TRUE),
    
    selectInput("vars", "Select a Category:", choices=htmlOutput("varselect"),
                multiple = TRUE),
    
    selectInput("credit","Select credit or debit", choices = htmlOutput("creditselect"),
                multiple = FALSE),
    
    dateRangeInput('dateRange',"Input Date Range")
  ),
  
  mainPanel(
    plotOutput("viz"),
    dataTableOutput("table")

  )
)


# server function:
server <- function(session,input, output) {
  
  Dataset <- reactive({
    
    infile <- input$datafile
    
    if (is.null(infile)) {
      
      return(NULL)
      
    }
    read.csv(infile$datapath,check.names = FALSE,stringsAsFactors = F )
  })
  
  output$creditselect <- renderUI({
    
  })
  
  observe({
    if (identical(Dataset(), '') || identical(Dataset(), data.frame()))
      return(NULL)
    
    updateSelectInput(session, inputId="credit", label="Credit or Debit",
                      
                      choices=unique(Dataset()$`Transaction Type`), selected="debit")
  })
  
  output$varselect <- renderUI({
    
  })
  
  observe({
    if (identical(Dataset(), '') || identical(Dataset(), data.frame()))
      return(NULL)
    
    updateSelectInput(session, inputId="vars", label="Variables to use:",
                      
                      choices=unique(Dataset()$Category), selected="Food & Dining")
  })
  
  output$dateRange <- renderUI({
    
    
    
  })
  
  observe({
    if (identical(Dataset(), '') || identical(Dataset(), data.frame()))
      return(NULL)
    
    updateDateRangeInput(session, 'dateRange',
                         label = 'Date range input: yyyy-mm-dd',
                         start = Sys.Date() - 7, end = Sys.Date())
  })
  
  # get dataframe based on selection:
  
  get_output <- function(){
    
    if (is.null(input$vars) || length(input$vars)==0)
      return(NULL)
    
    return(Dataset() %>% filter(Category %in% input$vars, `Transaction Type` %in% input$credit))
    
    
  }
  
  get_output_time <- function(){
    
    df <- get_output()
    
    if (identical(df, '') || identical(df, data.frame()))
      return(NULL)
    else{
      
      df[,'Date']<-format(strptime(as.character(df$Date),"%m/%d/%Y"),"%Y-%m-%d")
      df[,'Year']<-strftime(df$Date, "%Y" )
      df[,'Month']<-strftime(df$Date, "%m" )
      df[,'Day']<-strftime(df$Date, "%d" )
      df[,'MonthDay']<-as.numeric(paste0(df[,'Month'],df[,'Day']))
      
      return(df)
    }
    
  }

  output$table <- renderDataTable({
    if (is.null(input$vars) || length(input$vars)==0)
      return(NULL)
    
    df <- get_output_time()#test <-format(strptime(as.character(tran$Date),"%d/%m/%Y"),"%d/%m/%Y")
    
    return(df%>%select(Date,Description,Category,Amount,`Account Name`)%>%filter(Date >= input$dateRange[1], Date <= input$dateRange[2]))
    
  })
  
  output$table_historic <-renderDataTable({
    
    df <- get_output_time()#test <-format(strptime(as.character(tran$Date),"%d/%m/%Y"),"%d/%m/%Y")
    
    #df2 <-get_output_time()
    min_date <- as.numeric(strftime(input$dateRange[1],'%m%d'))
    max_date <- as.numeric(strftime(input$dateRange[2],'%m%d'))
    
    return(df%>%filter(MonthDay >= min_date, MonthDay <= max_date))
    
  })
  
  
  
  
  
  output$summary <- renderText({
    if (is.null(input$vars) || length(input$vars)==0)
      return(paste('Please upload transaction data'))
    
    
      paste('Average spending on ', paste(input$vars,collapse = '+'),  ':', 
            get_output_time()%>%summarise(Amount = median(Amount)))

      
    
  }
    
    
  )
  
  
  
  output$viz <-renderPlot({

    
    min_date <- as.numeric(strftime(input$dateRange[1],'%m%d'))
    max_date <- as.numeric(strftime(input$dateRange[2],'%m%d'))
    
    df<-get_output_time()%>%filter(MonthDay >= min_date, MonthDay <= max_date)
    
    ggplot(df) + geom_bar(aes(x = Month, y = Amount,fill = Month),stat = 'identity') + facet_grid(.~Year)

  })
}

shinyApp(ui, server)