#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
enrollment = readRDS("enrollment.rds")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project Title",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Model Title"),
                 sidebarLayout(
                     sidebarPanel(
                         textInput(
                             "name",
                             "What is your name?"),
                         textOutput("greeting"),
                         sliderInput(
                             "year",
                             "What year were you born?",
                             1900,
                             2020,
                             2000
                         ),
                         textOutput("year"),
                         sliderInput(
                             "enrollmentYear",
                             "What class year do you want?",
                             2015,
                             2019,
                             2019
                         )),
                     mainPanel(plotOutput("newPlot")))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$greeting <- renderText({
        if(input$name != "") (
            paste("Hello", input$name)
        )
    })
    
    output$year <- renderText({
        paste("So you're ", as.character(2020 - as.numeric(input$year)), 
              " years old")
    })
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$newPlot <- renderPlot({
        enrollment %>%
            filter(year == input$enrollmentYear) %>%
            ggplot(aes(x = u_grad)) + geom_density()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
