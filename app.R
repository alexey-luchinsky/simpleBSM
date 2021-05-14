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
                min = min(days), max = max(days),  value = max(days), step=1)
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
        all_df %>% group_by(day) %>% summarise(temp = mean(temperature)) %>% 
            ggplot(aes(x=day, y=temp)) + geom_point() + geom_line() +
            geom_vline(xintercept = day, linetype = "dashed") +
            labs(x = "Day", y = "Temperature", title = "Mean temperature")
    })
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
