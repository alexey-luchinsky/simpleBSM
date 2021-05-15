#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)
library(flexdashboard)


reload_data <- function(file_name = "all_df.csv") {
    print(paste("Reloading file", file_name))
    all_df <<- read.csv(file_name)
    days <<- unique(all_df$dayN)
}

reload_data()

######### Summary Panel #############
summary_panel <- function() {
    fluidPage(
        h2("Averaged information"),
        # Show gauges of metrics for current day
        fluidRow(
            column(4, gaugeOutput("tempGauge")),
            column(4, gaugeOutput("presGauge")),
            column(4, gaugeOutput("humGauge"))
        ),
        # Sidebar with a selector for metric
        sidebarLayout(
            sidebarPanel(
                    selectInput("metric", label = h3("Select"), 
                            choices = c("Temperature", "Pressure", "Humidity"),
                            selected = "Temperature")
            ),
            mainPanel(
                # Show history plot for selected metric
                plotOutput("historyPlot")
            )
        )
    )    
}

#### Details Panel
details_panel <- function() {
    fluidPage(
        h2("Detailed info"),
        h3("Temperature"),
        dataTableOutput('tempTable'),
        h3("Pressure"),
        dataTableOutput('presTable'),
        h3("Humidity"),
        dataTableOutput('humTable'),
        
    )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(tags$script(src = "message-handler.js")),
    # Application title
    titlePanel("Simple Building Management System"),
    actionButton("reload", "Reload"),
    # Sidebar with a slider input for current day 
    sidebarLayout(
        sidebarPanel(
            noUiSliderInput("current_day",  "Current day:", 
                        min = min(days), max = max(days),  value = max(days), step=1,
                        orientation = "vertical", direction = "rtl", width = "50px", height = "300px"),
            width = 2
        ),
        mainPanel(
            tabsetPanel(position = "below",
                tabPanel("Summary", summary_panel()),
                tabPanel("Details", details_panel())
            ),
            width = 10
        )
    )
)



gen_gauge <- function(input, output, metric, min = 0, max = 100) {
    renderGauge({
        current_day = input$current_day
        x <- all_df %>% select(dayN, y = tolower(metric)) %>% filter(dayN == current_day) %>% 
            summarise(my = mean(y)) %>% .$my %>% round
        gauge(x, min = min, max = max, label = metric, href = "http://www.bgsu.edu")
    })
}


gen_table <- function(input, output, metric) {
    renderDataTable({
        current_day = input$current_day
        df <- all_df %>% filter(dayN == current_day) %>% select(room, tolower(metric)) %>% round
        datatable(df, rownames = F, selection = "none", options = list(dom="t"))
        # datatable(df, filter="top", selection="multiple", escape=FALSE,
        #           options = list(dom = 't'))
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
    
    new_data <- eventReactive(input$reload, {
        print("RELOAD")
        reload_data()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
