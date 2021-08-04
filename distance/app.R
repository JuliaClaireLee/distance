#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinythemes)
library(shiny)
library(readr)
uscities <- read_csv("uscities.csv")
k<-function(city1,state1, city2,state2){
    lo1<-uscities %>%
        filter(city == city1 & state_id == state1)
    lo2<-uscities %>%
        filter(city == city2 & state_id == state2)
    p<-distm(c(lo1$lng,lo1$lat),c(lo2$lng,lo2$lat), fun=distHaversine)
    r<-p[1]/1609
    return(r)
}
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
              # Application title
              titlePanel("Distance between two U.S. cities"),
              sidebarPanel(
                  textInput("obs1", "city:", value = "New York", width = NULL, placeholder = NULL ),
                  textInput("obs2", "State:", value = "NY", width = NULL, placeholder = NULL )
                  
              ),
              sidebarPanel(
                  textInput("obs3", "city:", value = "New York", width = NULL, placeholder = NULL ),
                  textInput("obs4", "State:", value = "NY", width = NULL, placeholder = NULL )
                  
              ),
        # Show a plot of the generated distribution
        mainPanel(
            h5("The distance between the 2 cities:", align = "left"),
            verbatimTextOutput("val2")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$val12 <- renderText({
        k(input$obs1,input$obs2, input$obs3, input$obs4 )
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
