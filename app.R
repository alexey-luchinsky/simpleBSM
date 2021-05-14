#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)


# Loading data
all_df = read.csv("all_df.csv")
days = unique(all_df$dayN)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simple Building Management System"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("current_day",  "Current day:", 
                min = min(days), max = max(days),  value = max(days), step=1),
            selectInput("metric", label = h3("Select"), 
                        choices = c("Temperature", "Pressure", "Humidity"),
                        selected = "Temperature")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           fluidRow(
               column(4, verbatimTextOutput("tempGauge")),
               column(4, verbatimTextOutput("presGauge")),
               column(4, verbatimTextOutput("humGauge"))
           ),
           plotOutput("historyPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$tempGauge <- renderText({
        current_day = input$current_day
        all_df %>% select(dayN, temperature) %>% filter(dayN == current_day) %>% 
            summarise(y = mean(temperature)) %>% .$y %>% round %>% paste("Temperature:",.)
    })
    output$presGauge <- renderText({
        current_day = input$current_day
        all_df %>% select(dayN, pressure) %>% filter(dayN == current_day) %>% 
            summarise(y = mean(pressure)) %>% .$y %>% round %>% paste("Pressure:",.)
    })
    output$humGauge <- renderText({
        current_day = input$current_day
        all_df %>% select(dayN, humidity) %>% filter(dayN == current_day) %>% 
            summarise(y = mean(humidity)) %>% .$y %>% round %>% paste("Humidity:",.)
    })
    output$historyPlot <- renderPlot({
        current_day = input$current_day
        metric = input$metric
        all_df %>% select(dayN, y=tolower(metric)) %>% group_by(dayN) %>% summarize(my = mean(y)) %>% 
            ggplot(aes(x=dayN, y=my)) + geom_point() + geom_line() +
            geom_vline(xintercept = current_day, linetype = "dashed") +
            labs(x = "Day", y = metric, title = paste("Mean", tolower(metric)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
