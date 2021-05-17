#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# need to install: libssl-dev

# info about shiny on aws: https://towardsdatascience.com/how-to-host-a-r-shiny-app-on-aws-cloud-in-7-simple-steps-5595e7885722
#   https://www.linode.com/docs/guides/how-to-deploy-rshiny-server-on-ubuntu-and-debian/


library(shiny)
library(shinyWidgets)
library(tidyr)
library(dplyr)
#library(tidyverse)
#library(ggplot2)
library(plotly)       # libssl-dev is required, libcurl4-openssl-dev 
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

## Original Data Panel
original_data_panel <- function() {
    fluidPage(
        h2("Original Data"),
        dataTableOutput('originalTable')
    )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(tags$script(src = "message-handler.js")),
    # Application title
    titlePanel("Simple Building Management System"),
    fluidRow(
        column(2, actionButton("reload", "Reload"), offset = 1), 
        column(2, actionButton("about", "About"), offset = 5)
    ),
    fluidRow(column(width = 6, offset = 0, style='padding-left:0px; padding-right:1px; padding-top:5px; padding-bottom:5px')),
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
                tabPanel("1: Summary", summary_panel()),
                tabPanel("2: Details", details_panel()),
                tabPanel("3: Original Data", original_data_panel())
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
    
    output$originalTable <- renderDataTable(
        datatable(
            all_df %>% select(dayN, room, temperature, pressure, humidity) %>% round(2)
            , rownames = F, selection = "none")
    )
    
    new_data <- observeEvent(input$reload, {
        print("RELOAD")
        reload_data()
    })
    
    observeEvent(input$about, {
        print("ABOUT")
        showModal(modalDialog(
            p("In this application I am using tha R shiny toolkit to build a simple Building Management System dashboard"),
            p("In our toy bulding there are 10 rooms with temperature, pressure and humidity detectors in each room. Data from all these rooms
              was collected 20 days"),
            tags$ul(
                tags$li("In tab \"1: Summary\" you can find some averaged by rooms inforamtion for the selected day"),
                tags$li("In tab \"2: Details\" detaied information for each room is presented"),
                tags$li("In tah \"3: Original Data\" the original table is shown")
            ),
            p("Click anywhere outside this dialog box to exit it"),
            title = "Simple Building Management System",
            easyClose = TRUE,
            footer = NULL
        ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
