#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries

library(shiny)
library(ggplot2)
library(rsconnect)
library(ggthemes)
library(dplyr)

# Connecting with shinyapps.io

rsconnect::setAccountInfo(name='hkhan10',
                          token='98AE94E42147E194412E59A9C277E4DE',
                          secret='CIhbO9Z5mofaaxRPq+HMDLvyvUE8H0OfQmHaukFP')

# Read and filter data for 2010
mortality_rate <- read.csv("mortality rate.csv", stringsAsFactors = FALSE)


# Define UI for application 

ui2 <- fluidPage(
  titlePanel("Crude Mortality Rate in state Vs National Average"), 
  sidebarPanel(
    
    selectInput("option1", label = strong("State"), 
                choices = levels(as.factor(mortality_rate$State)), 
                selected = 1),
    
    selectInput("option2", label = strong("Death Reason"), 
                choices = levels(as.factor(mortality_rate$ICD.Chapter)), 
                selected = 1),
    
    width = "auto"
    
  ),
  mainPanel(
    plotOutput("distPlot")
    
  )
  
)

# Define server logic 
server2 <- function(input, output) {
  
  output$distPlot <- renderPlot({
    mortality_rate %>% 
      group_by(Year, ICD.Chapter) %>% 
      mutate(crude.rate.new = 10^5*(sum(Deaths))/sum(Population)) %>% 
      group_by(Year, State, ICD.Chapter) %>% 
      mutate(crude.rate.new2=10^5*(sum(Deaths)/Population)) %>% 
      select(ICD.Chapter, State, Year, crude.rate.new, crude.rate.new2) %>% 
      filter(ICD.Chapter== input$option2, State== input$option1) %>% 
      ggplot()+ geom_bar(aes(x=Year, weight= crude.rate.new2))+
      geom_line(aes(x=Year, y=crude.rate.new))+labs(x= "Years", y="Crude Rates")
    
  }
  )
}


# Run the application 
shinyApp(ui = ui2, server = server2)

