library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(scales) 
library(shiny)
library(plotly)

#load the excel file
india_export <- read_excel("india_export.xlsx")


#creating shiny app for total export value
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="group",
                  label="Select the Product Group:",
                  choices=unique(india_export$group),
                  selected="Raw materials",
                  multiple=TRUE),
      sliderInput(inputId="time",
                  label = "Select the range of years",
                  min = 1989, max= 2016,
                  value = c(1989, 2016),
                  step = 1, sep = "")
    ),
    #Main panel
    mainPanel(
      plotlyOutput(outputId = "linechart", width = "100%", height = "550px")
    )
  )
)


#Define server function
server <- function(input,output) {
  plotlydata <- reactive({
    india_export %>% 
      filter(group %in% input$group) %>% 
      filter(Year %in% seq(input$time[1], input$time[2], 1)) %>% 
      filter(Indicator=="Export-Share")
    
  })
  #create line chart using reactivity
  output$linechart <- renderPlotly({
    ggplotly(ggplot(data = plotlydata(), mapping = aes(x=Year, y=trade_value))+
             geom_line(aes(group=group, colour=group), size=1)+geom_point(aes(color=group))+
               scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.8))+ theme_classic()+
               theme(axis.text.x = element_text(angle = 55, hjust=1), axis.text.y = element_text(angle=55))+
               labs(x="Years", y="Export Share (in percentage)", color="Product Group"))
  })
}

#Create the shiny app object
shinyApp(ui=ui, server=server)
