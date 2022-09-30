library(RLab5)
library(shiny)
library(shinythemes)
library(ggplot2)


ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("GET DATA"),
  textInput(inputId="kpi","KPI",'N00945'),
  textInput(inputId="municipality","Municipality",''),
  textInput(inputId="period","Year",'2009'),
  mainPanel(
    tableOutput(outputId="df_data")
  )
)

# Define server function
server <- function(input, output) {
  pkpi<-reactive({input$kpi})
  pmunicipality<-reactive({input$municipality})
  pperiod<-reactive({input$period})

  rmm<-get_data(link="http://api.kolada.se/v2/data/",kpi=pkpi,period=pperiod,municipality=pmunicipality)
  dfdata<-return_df(rmm)
  jsontext<-return_json(rmm)
  output$df_data<-renderTable(dfdata)
}


# Create Shiny object
shinyApp(ui = ui, server = server)
