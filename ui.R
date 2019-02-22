# load libraries
library(DT)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(prophet)
library(ggplot2)
library(rships)
library(shinycssloaders)
library(magrittr)
library(lubridate)
library(readr)

# Set ggplot theme
theme_set(theme_minimal())
# Increase input file limit to 10MB
options(shiny.maxRequestSize = 10*1024^2)

ui <- dashboardPage(
  dashboardHeader(title = "SHA-Forecasting Tool"),
  
  ## Sidebar ------------------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "About"),
      menuItem("Forecasting Tool", tabName = "Forecast")
    )
  ),
  
  ## Body ------------------------------------
  dashboardBody(
    ### include css file --------------------
    tags$head(tags$style(includeCSS("./www/mycss.css"))),
    ### include script with function openTab----------------------
    tags$script(HTML("var openTab = function(tabName){$('a', $('.sidebar')).each(function() {
                     if(this.getAttribute('data-value') == tabName) {
                     this.click()
                     };
                     });
                     }
                     ")),
    ### use shinyjs -----------------------
    useShinyjs(),
    
    ## Tab Items ---------------------------
    tabItems(
      ### ABout ----------------------------
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
      ### Prophet ----------------------------
      tabItem(tabName = "Forecast",
              fluidRow(
                box(width = 12,
                    tabsetPanel(id = "inTabset",
                                ## TAB 1 : Upload Data --------------------------
                                tabPanel(title = "Upload Data", value = "panel1",
                                         
                                         fluidRow(
                                           ## upload main dataset -----------------
                                           column(width = 6,
                                                  # column(width = 12,
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
                                           ## predict parameters --------------------
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
                                         ## Next 1 ---------------
                                         fluidRow(
                                           column(width = 2, offset = 10,
                                                  shinyjs::disabled(actionButton("next1", "Next",
                                                                                 style = "width:100%; font-size:200%"))))
                                ),
                                ## TAB 2 : Set Parameters -----------------------------------
                                tabPanel(title = "Set Parameters", value = "panel2",
                                         fluidRow(
                                           column(width = 12,
                                                  column(width = 9, 
                                                         offset = 6,
                                                         tags$h3("Prophet Parameters")),
                                                  column(width = 6,
                                                         radioButtons("growth",
                                                                      "growth",
                                                                      c('linear',
                                                                        'logistic'), 
                                                                      inline = TRUE),
                                                         
                                                         ### parameter: yearly.seasonality
                                                         checkboxInput("yearly",
                                                                       "yearly.seasonality", 
                                                                       value = TRUE),
                                                         
                                                         ### parameter: weekly.seasonality 
                                                         checkboxInput("monthly",
                                                                       "weekly.seasonality", 
                                                                       value = TRUE),
                                                         ### parameter: n.changepoints
                                                         numericInput("n.changepoints",
                                                                      "n.changepoints", 
                                                                      value = 25),
                                                         
                                                         ### parameter: seasonality.prior.scale
                                                         numericInput("seasonality_scale",
                                                                      "seasonality.prior.scale", 
                                                                      value = 10),
                                                         
                                                         ### parameter: changepoint.prior.scale
                                                         numericInput("changepoint_scale",
                                                                      "changepoint.prior.scale", 
                                                                      value = 0.05, 
                                                                      step = 0.01)),
                                                  column(width = 6,
                                                         
                                                         ### parameter: holidays.prior.scale
                                                         numericInput("holidays_scale",
                                                                      "holidays.prior.scale", 
                                                                      value = 10),
                                                         
                                                         ### parameter: mcmc.samples
                                                         numericInput("mcmc.samples", 
                                                                      "mcmc.samples", 
                                                                      value = 0),
                                                         
                                                         ### parameter: interval.width
                                                         numericInput("interval.width", 
                                                                      "interval.width", 
                                                                      value = 0.8, 
                                                                      step = 0.1),
                                                         ### parameter: uncertainty.samples
                                                         numericInput("uncertainty.samples",
                                                                      "uncertainty.samples", 
                                                                      value = 1000))
                                                  
                                           ) #,
                                           # ## predict parameters --------------------
                                           # column(width = 4,
                                           #        tags$h3("Predict Parameters"),
                                           #        ### paramater: periods
                                           #        numericInput("periods",
                                           #                     "periods",
                                           #                     value = 365),
                                           #        ### parameter: freq
                                           #        selectInput("freq","freq",
                                           #                    choices = c('day', 'week', 'month', 
                                           #                                'quarter','year')),
                                           #        ### parameter: include_history
                                           #        checkboxInput("include_history",
                                           #                      "include_history", 
                                           #                      value = TRUE)
                                           # )
                                         )
                                         ,
                                         ## Back/Next 2 --------------------------
                                         fluidRow(
                                           column(width = 2, 
                                                  actionButton("back2", "Back",
                                                               style = "width:100%; font-size:200%")),
                                           column(width = 2, offset = 8,
                                                  actionButton("next2", "Next",
                                                               style = "width:100%; font-size:200%"))
                                         )
                                ),
                                ## TAB 3 : Fit Prophet Model ----------------------
                                tabPanel(title = "Fit Forecasting Model", value = "panel3", 
                                         fluidRow(
                                           # box(width = 12, 
                                           column(width = 12,
                                                  shinyjs::disabled(actionButton("plot_btn2", 
                                                                                 "Fit Forecasting Model",
                                                                                 style = "width:30%; margin-top: 25px; margin-bottom: 50px; font-size:150%; ")
                                                  )
                                           )
                                         ),
                                         ## Plots Box : collapsible ------------------
                                         fluidRow( 
                                           conditionalPanel("input.plot_btn2",
                                                            box(width = 12, collapsible = TRUE, 
                                                                title = "Plots",
                                                                tabsetPanel(
                                                                  tabPanel("Forecast Plot",
                                                                           
                                                                           div(id = "output-container",
                                                                               withSpinner(plotOutput("ts_plot"))
                                                                           )
                                                                           # )
                                                                           
                                                                  ),
                                                                  tabPanel("Prophet Plot Components",
                                                                           # output.logistic_check=='no_error'
                                                                           conditionalPanel("input.plot_btn2",
                                                                                            div(id = "output-container",
                                                                                                plotOutput("prophet_comp_plot"))
                                                                           )
                                                                  )
                                                                )))),
                                         ## Results Box : collapsible ------------------
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
                                         ## back 3 ------------
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