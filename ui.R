# load libraries
library(DT)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(prophet)
library(ggplot2)
library(rships)
library(shinycssloaders)
library(magrittr)
library(lubridate)
library(readr)

ui <- dashboardPage(
  dashboardHeader(title = "SHA-Forecasting Tool"),
  
  ## Sidebar----
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "About"),
      menuItem("Forecasting Tool", tabName = "Forecast")
    )
  ),
  ## Body----
  dashboardBody(
    ### include css file----
    tags$head(tags$style(includeCSS("./www/style.css"))),
    ### include script with function openTab----
    tags$script(HTML("var openTab = function(tabName){$('a', $('.sidebar')).each(function() {
                     if(this.getAttribute('data-value') == tabName) {
                     this.click()
                     };
                     });
                     }
                     ")),
    ### use shinyjs----
    useShinyjs(),
    
    ## Tab Items----
    tabItems(
      ### About----
      tabItem(tabName = "About",
              fluidRow(
                box(width = 12,
                    infoBox(width = 12,
                            title = "",
                            value = includeHTML("./www/about.html"),
                            icon = icon("info")),
                    
                    column(width = 3,
                           a(actionButton(inputId = "start",
                                          label = "Get Started",
                                          style = "font-size: 150%'"),
                             onclick = "openTab('Forecast')",
                             style = "cursor: pointer; font-size: 300%;")))
                
              )
      ),
      ### Prophet----
      tabItem(tabName = "Forecast",
              fluidRow(
                box(width = 12,
                    tabsetPanel(id = "inTabset",
                                ## TAB 1: Upload Data----
                                tabPanel(title = "Upload Data", value = "panel1",
                                         
                                         fluidRow(
                                           ## upload main dataset----
                                           column(width = 6,
                                                  tags$h4("Main Dataset"),
                                                  helpText("One column should contain the date information"),
                                                  fileInput("ts_file","Upload CSV File",
                                                            accept = c(
                                                              "text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")),
                                                  
                                                  conditionalPanel(condition = 'output.panelStatus',
                                                                   helpText("First 6 rows of the uploaded data")),
                                                  
                                                  tableOutput("uploaded_data"),
                                                  ### error msg if main dataset is not valid 
                                                  uiOutput("msg_main_data")
                                                  
                                           ),
                                           ## predict parameters----
                                           column(width = 6,
                                                  tags$h3("Select Columns:"),
                                                  fluidRow(
                                                    column(width = 6, 
                                                           uiOutput("dateColumn")),
                                                    column(width = 6,
                                                           uiOutput("actualColumn"))
                                                  ),
                                                  hr(),
                                                  tags$h3("Forecasting Parameters"),
                                                  ### paramater: periods
                                                  numericInput("periods",
                                                               "# Forecasted Periods",
                                                               value = 90),
                                                  ### parameter: freq
                                                  selectInput("freq", "Forecast Frequency",
                                                              choices = c('Daily' = 'day', 
                                                                          'Weekly' = 'week', 
                                                                          'Monthly' = 'month', 
                                                                          'Quarterly' = 'quarter',
                                                                          'Yearly' = 'year')),
                                                  ### Add holiday information
                                                  checkboxInput("holiday",
                                                                "Include holiday information?",
                                                                value = TRUE)
                                           )
                                         ),
                                         ## Next 1----
                                         fluidRow(
                                           column(width = 2, offset = 10,
                                                  shinyjs::disabled(actionButton("next1", "Next",
                                                                                 style = "width:100%; font-size:200%"))))
                                ),
                                ## TAB 2: Set Filters----
                                tabPanel(title = "Select Filters", value = "panel2",
                                         fluidRow(id = "placeholder")
                                         ,
                                         ## Back/Next 2----
                                         fluidRow(
                                           column(width = 2, 
                                                  actionButton("back2", "Back",
                                                               style = "width:100%; font-size:200%")),
                                           column(width = 2, offset = 8,
                                                  actionButton("next2", "Next",
                                                               style = "width:100%; font-size:200%"))
                                         )
                                ),
                                ## TAB 3: Fit Prophet Model----
                                tabPanel(title = "Fit Forecasting Model", value = "panel3", 
                                         fluidRow(
                                           column(width = 12,
                                                  shinyjs::disabled(actionButton("plot_btn2", 
                                                                                 "Fit Forecasting Model",
                                                                                 style = "width:30%; margin-top: 25px; margin-bottom: 50px; font-size:150%; ")
                                                  )
                                           )
                                         ),
                                         ## Plots Box: collapsible----
                                         fluidRow( 
                                           conditionalPanel("input.plot_btn2",
                                                            box(width = 12, collapsible = TRUE, 
                                                                title = "Plots",
                                                                tabsetPanel(
                                                                  tabPanel("Forecast Plot",
                                                                           
                                                                           div(id = "output-container",
                                                                               withSpinner(plotOutput("ts_plot"))
                                                                           )
                                                                           
                                                                  ),
                                                                  tabPanel("Prophet Plot Components",
                                                                           conditionalPanel("input.plot_btn2",
                                                                                            div(id = "output-container",
                                                                                                plotOutput("prophet_comp_plot"))
                                                                           )
                                                                  )
                                                                )))),
                                         ## Results Box: collapsible----
                                         fluidRow(
                                           conditionalPanel("input.plot_btn2",
                                                            box(width = 12, collapsible = TRUE, 
                                                                title = "Results",
                                                                
                                                                div(id = "output-container3",
                                                                    DT::dataTableOutput("data")),
                                                                conditionalPanel("output.data",
                                                                                 uiOutput("dw_button")
                                                                )
                                                            )
                                           )),
                                         ## back 3----
                                         fluidRow(
                                           column(width = 2, 
                                                  actionButton("back3", "Back",
                                                               style = "width:100%; font-size:200%"))
                                         )
                                )
                    )
                )
                )))
  )
)