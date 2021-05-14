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


# Loading data
all_df = read.csv("all_df.csv")
days = unique(all_df$day)
last_day <- max(days)
last_df <- group_by(all_df, day) %>% summarise(temperature = mean(temperature), pressure = mean(pressure), humidity = mean(humidity)) %>% 
    filter(day == last_day)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simple Building Management System"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("day",  "Current day:", 
                min = min(days), max = max(days),  value = max(days), step=1),
            selectInput("metric", label = h3("Select"), 
                        choices = c("Temperature", "Pressure", "Humidity"),
                        selected = "Temperature")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("historyPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$historyPlot <- renderPlot({
        day = input$day
        metric = input$metric
        all_df %>% select(day, y=tolower(metric)) %>% group_by(day) %>% 
            ggplot(aes(x=day, y=y)) + geom_point() + geom_line() +
            geom_vline(xintercept = day, linetype = "dashed") +
            labs(x = "Day", y = metric, title = paste("Mean", tolower(metric)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
