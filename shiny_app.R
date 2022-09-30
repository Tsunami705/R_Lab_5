library(RLab5)
library(shiny)
library(shinythemes)
library(ggplot2)

#rmm<-get_data(link="http://api.kolada.se/v2/data/",kpi='N00945',period='2009')
#dfdata<-return_df(rmm)
#jsontext<-return_json(rmm)

ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("GET DATA(You must fullfill 2 in 3 below)"),
  textInput(inputId="kpi","KPI",'N00945'),
  textInput(inputId="municipality","Municipality",''),
  textInput(inputId="period","Year",'2009'),
  mainPanel(
    tableOutput(outputId="df_data")
  )
)

# Define server function
server <- function(input, output) {
  #pkpi<-input$kpi
  #pmunicipality<-input$municipality
  #pperiod<-input$period

  #rmm<-get_data(link="http://api.kolada.se/v2/data/",kpi=pkpi,period=pperiod,municipality=pmunicipality)
  #dfdata<-return_df(rmm)
  #jsontext<-return_json(rmm)
  output$df_data<-renderTable({
    pkpi<-input$kpi
    pmunicipality<-input$municipality
    pperiod<-input$period
    rmm<-get_data(link="http://api.kolada.se/v2/data/",kpi=pkpi,period=pperiod,municipality=pmunicipality)
    dfdata<-return_df(rmm)
    jsontext<-return_json(rmm)
    print(dfdata)}
                              )
}


# Create Shiny object
shinyApp(ui = ui, server = server)
