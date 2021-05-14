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

######### Summary Panel #############
summary_panel <- function() {
    fluidPage(
        
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
}

#### Details Panel
details_panel <- function() {
    fluidPage(
        titlePanel("Simple Building Management System"),
        sliderInput("current_day2",  "Current day:", min = min(days), max = max(days),  value = max(days), step=1),
        h2("Temperature"),
        dataTableOutput('tempTable'),
        h2("Pressure"),
        dataTableOutput('presTable'),
        h2("Humidity"),
        dataTableOutput('humTable'),
        
    )
}

# Define UI for application that draws a histogram
ui <- tabsetPanel(position = "below",
                  tabPanel("Summary", summary_panel()),
                  tabPanel("Details", details_panel())
                  )

gen_gauge <- function(input, output, metric) {
    renderText({
        current_day = input$current_day
        all_df %>% select(dayN, y = tolower(metric)) %>% filter(dayN == current_day) %>% 
            summarise(my = mean(y)) %>% .$my %>% round %>% paste(metric,.)
    })
}

gen_table <- function(input, output, metric) {
    renderDataTable({
        current_day = input$current_day2
        df <- all_df %>% filter(dayN == current_day) %>% select(room, tolower(metric)) %>% round
        df
    })
}

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$tempGauge <- gen_gauge(input, output, "Temperature")
    output$presGauge <- gen_gauge(input, output, "Pressure")
    output$humGauge <- gen_gauge(input, output, "Humidity")
    
    output$historyPlot <- renderPlot({
        current_day = input$current_day
        metric = input$metric
        all_df %>% select(dayN, y=tolower(metric)) %>% group_by(dayN) %>% summarize(my = mean(y)) %>% 
            ggplot(aes(x=dayN, y=my)) + geom_point() + geom_line() +
            geom_vline(xintercept = current_day, linetype = "dashed") +
            labs(x = "Day", y = metric, title = paste("Mean", tolower(metric)))
    })
    

    output$tempTable <- gen_table(input, output, "Temperature")
    output$presTable <- gen_table(input, output, "Pressure")
    output$humTable <- gen_table(input, output, "Humidity")
    
}

# Run the application 
shinyApp(ui = ui, server = server)
