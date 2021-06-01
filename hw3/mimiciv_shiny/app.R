#B203 hw3 icu cohort explorer
#Alvin Wang 605524509

library(shiny)
library(plotly)
library(dplyr)

icu_cohort <- readRDS("icu_cohort.rds")

demo_data <- c("gender", "anchor_age", "insurance", "language", "marital_status",
               "ethnicity")
lab_data <- c("bicarbonate", "calcium", "chloride", "creatinine", "glucose",
              "magnesium", "potassium", "sodium", "hematocrit", "wbc", "lactate")
vital_data <- c("heart_rate", "non_invasive_blood_pressure_systolic",
                "non_invasive_blood_pressure_mean", "respiratory_rate",
                "temperature_fahrenheit","arterial_blood_pressure_systolic",
                "arterial_blood_pressure_mean" )
# Define UI for application that draws a histogram
ui <- navbarPage(
    "ICU Cohort Explorer",
    tabPanel("Demographics",
    # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
                selectInput("demo",
                            "Select demographics to plot:",
                            choices = demo_data,
                            selected = "Gender"),
                helpText("MIMIC-IV Demographics")
            ),
            
            mainPanel(
                plotlyOutput("demoChart")
            )
        )
    ),
    tabPanel("Lab Measurements", 
             sidebarLayout(
                 sidebarPanel(
                     selectInput("lab",
                                 "Select lab measurements to plot:",
                                 choices = lab_data,
                                 selected = "bicarbonate"),
                     helpText("MIMIC-IV Lab Measurements"),
                     verbatimTextOutput("labsum")
                     
                 ),
                 
                 mainPanel(
                     textOutput("miss_value"),
                     plotlyOutput("labChart"),
                     plotlyOutput("labDensity")
                 )
             )),
    tabPanel("Vitals", 
             sidebarLayout(
                 sidebarPanel(
                     selectInput("vital",
                                 "Select a vital variable to plot:",
                                 choices = vital_data,
                                 selected = "heart_rate"),
                     helpText("MIMIC-IV Vitals Data"),
                     verbatimTextOutput("vitalsum")
                 ),
                 
                 mainPanel(
                     textOutput("miss_value_vt"),
                     plotlyOutput("vitalChart"),
                     plotlyOutput("vitalDensity")
                 )
             ))
)

# Define server logic required to plot
server <- function(input, output) {
    #Tab1: Demographics
    demochart_dt <- reactive({
        out <- icu_cohort[, input$demo]
        names(out)  <- c("col")
        if(input$demo == "anchor_age"){
            out$col[out$col < 30] <- "<30"
            out$col[which(out$col >= 30 & out$col <= 50)] <- "30-50"
            out$col[which(out$col > 50 & out$col <= 70)] <- "50-70"
            out$col[out$col > 70] <- ">70"
        }
        return(out)
    })
    # Fill in the plot for demographics
    output$demoChart <- renderPlotly({
        # Render a pie chart
        demochart_dt() %>% group_by(col) %>% summarise(count = n()) %>%
        plot_ly(labels = ~col, values = ~count, type = 'pie', 
                textinfo='label+percent',
                insidetextorientation='radial',
                hole = 0.4)
    })
    #Tab2: Lab measurements
    output$labsum <- renderPrint(summary(icu_cohort[, input$lab]))
    
    output$miss_value <- renderText({ 
        paste("Number of missing values:", sum(is.na(lab_miss())))
    })
    
    lab_miss <- reactive({
        lab_miss <- icu_cohort[, input$lab]
        return(lab_miss)
    })
    
    lab_dt <- reactive({
        lab_out <- icu_cohort[, input$lab] %>% na.omit() %>% 
            rename(col=input$lab)
        lab_out <- lab_out %>% filter(col <= quantile(lab_out$col, 0.99, na.rm=T), 
                   col >= quantile(lab_out$col, 0.01, na.rm=T)) 
        return(lab_out)
    })
    # Fill in the plot for lab measurements
    output$labChart <- renderPlotly({
        lab_dt() %>% group_by(col) %>% summarise(count = n()) %>%
        plot_ly(y = ~col, type = "box") %>% 
            layout(xaxis = list(title=''), yaxis = list(title=input$lab))
    })
    lab_den <- reactive({
        lab_out <- icu_cohort[, input$lab] %>% na.omit() %>% 
            rename(col=input$lab)
        lab_out <- lab_out %>% filter(col <= quantile(lab_out$col, 0.99, na.rm=T), 
                                    col >= quantile(lab_out$col, 0.01, na.rm=T)) 
        lab_density <- density(lab_out$col)
        return(lab_density)
    })
    #Density plot for lab measurements
    output$labDensity <- renderPlotly({
        den1 <- lab_den()
        plot_ly(x = ~den1$x, y = ~den1$y, type = 'scatter',
                              mode = 'lines', fill = 'tozeroy') %>% 
            layout(xaxis = list(title=''), yaxis = list(title='density'))
    })
    #Tab3: Vitals
    output$vitalsum <- renderPrint(summary(icu_cohort[, input$vital]))
    
    output$miss_value_vt <- renderText({ 
        paste("Number of missing values:", sum(is.na(vital_miss())))
    })
    vital_miss <- reactive({
        vital_miss <- icu_cohort[, input$vital]
        return(vital_miss)
    })
    
    vital_dt <- reactive({
        vital_out <- icu_cohort[, input$vital] %>% na.omit() %>% 
            rename(col=input$vital)
        vital_out <- vital_out %>% filter(col <= quantile(vital_out$col, 0.99, na.rm=T), 
                                      col >= quantile(vital_out$col, 0.01, na.rm=T)) 
        names(vital_out)  <- c("col")
        return(vital_out)
    })
    # Fill in the plot for vitals
    output$vitalChart <- renderPlotly({
        vital_dt() %>% group_by(col) %>% summarise(count = n()) %>%
            plot_ly(y = ~col, type = "box") %>% 
            layout(xaxis = list(title=''), yaxis = list(title=input$vital))
    })
    
    vital_den <- reactive({
        vital_out <- icu_cohort[, input$vital] %>% na.omit() %>% 
            rename(col=input$vital)
        vital_out <- vital_out %>% filter(col <= quantile(vital_out$col, 0.99, na.rm=T), 
                                          col >= quantile(vital_out$col, 0.01, na.rm=T)) 
        vital_density <- density(vital_out$col)
        return(vital_density)
    })
    #Density plot for vitals
    output$vitalDensity <- renderPlotly({
        den2 <- vital_den()
        plot_ly(x = ~den2$x, y = ~den2$y, type = 'scatter',
                mode = 'lines', fill = 'tozeroy') %>% 
            layout(xaxis = list(title=''), yaxis = list(title='density'))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
