library(shiny)
library(dplyr)
library(ggplot2)

# load the data (retrieve and clean raw data if this is the first time)
bcl <- read.csv("dataset.csv", stringsAsFactors = FALSE)


ui <- fluidPage(
  titlePanel("Insurance for you."),
  sidebarLayout(
    sidebarPanel(
      h4(
        "This app will help you find the right Insurance Plan! Just use the filters below..."
      ),
      br(),
      sliderInput("ageInput", "AGE", 1, 90, c(25, 40)),
      uiOutput("typeSelectOutput"),
      checkboxInput("filterState", "Filter by state", FALSE),
      conditionalPanel(
        condition = "input.filterState",
        uiOutput("stateSelectorOutput")
      ),
      
      hr(),
      span("Data source:", 
           tags$a("HealthInsuranceMarketplace",
                  href = "https://www.kaggle.com/hhsgov/health-insurance-marketplace")),
      
      br(), br(),
      em(
          ("Created by, Pankhuri Roy, Sree Lakshmi Kurra, Swetha Iyer")
      )
      
      
    ),
    mainPanel(
      h3(textOutput("summaryText")),
      downloadButton("download", "Download results"),
      br(),
      plotOutput("plot"),
      br(), br(),
      #tableOutput("age")
      DT::dataTableOutput("age")
    )
  )
)

server <- function(input, output, session) {
  output$stateSelectorOutput <- renderUI({
    selectInput("stateInput", "State",
                sort(unique(bcl$StateCode)),
                selected = "LA")
  })
  
  output$typeSelectOutput <- renderUI({
    selectInput("typeInput", "Product type",
                sort(unique(bcl$Type)),
                multiple = TRUE,
                selected = c("DENTAL", "HEALTH"))
  })
  
 
  output$summaryText <- renderText({
    numOptions <- nrow(age())
    if (is.null(numOptions)) {
      numOptions <- 0
    }
    paste0("We found ", numOptions, " options for you")
  })
  
  age <- reactive({
    age <- bcl
    
    if (is.null(input$stateInput)) {
      return(NULL)
    }
    
    age <- dplyr::filter(age, Type %in% input$typeInput)
    if (input$filterState) {
      age <- dplyr::filter(age, StateCode == input$stateInput)
    }
    age <- dplyr::filter(age, AGE >= input$ageInput[1],
                         AGE <= input$ageInput[2])
    
    if(nrow(age) == 0) {
      return(NULL)
    }
    age
  })
  
  output$plot <- renderPlot({
    if (is.null(age())) {
      return(NULL)
    }
    
    ggplot(age(), aes(Plans, fill = Type)) +
      stat_count(colour = "black") +
      theme_classic(20)
  })
  
  output$age <- DT::renderDataTable({
    age()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      "results.csv"
    },
    content = function(con) {
      write.csv(prices(), con)
    }
  )
}

shinyApp(ui = ui, server = server)