# load libraries
library(DT)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(prophet)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "Prophet Explorer"),
  
  ## Sidebar ------------------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "About"),
      menuItem("Prophet Explorer", tabName = "Prophet")
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
                box(width=12,
                    infoBox(width = 12,
                            title = "",
                            value = includeHTML("./www/about.html"),
                            icon = icon("info")),
                    
                    column(width = 3,
                           a(actionButton(inputId = "start",
                                          label = "Get Started",
                                          style = "font-size: 150%'"),
                             onclick = "openTab('Prophet')",
                             style="cursor: pointer; font-size: 300%;")))
                
              )
      ),
      ### Prophet ----------------------------
      tabItem(tabName = "Prophet",
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
                                                  helpText("A valid dataframe contains at least 2 colums (ds, y)"),
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
                                           ## upload holidays -----------------
                                           column(width = 6,
                                                  tags$h4("Holidays (Optional)"),
                                                  helpText("A valid dataframe contains at least 2 colums (ds, holiday)"),
                                                  fileInput("holidays_file","Upload CSV File",
                                                            accept = c(
                                                              "text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")),
                                                  conditionalPanel(condition = 'output.panelStatus_holidays',
                                                                   helpText("First 6 rows of the uploaded holidays ")),
                                                  tableOutput("uploaded_holidays")
                                                  
                                                  ### error msg if holidays is not valid 
                                                  # uiOutput("msg_holidays")
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
                                           column(width = 8,
                                                  column(width = 8, offset = 2,
                                                         tags$h3("Prophet Parameters")),
                                                  column(width = 6,
                                                         
                                                         radioButtons("growth","growth",
                                                                      c('linear','logistic'), inline = TRUE),
                                                         
                                                         ### parameter: yearly.seasonality
                                                         checkboxInput("yearly","yearly.seasonality", value = TRUE),
                                                         
                                                         ### parameter: weekly.seasonality 
                                                         checkboxInput("monthly","weekly.seasonality", value = TRUE),
                                                         ### parameter: n.changepoints
                                                         numericInput("n.changepoints","n.changepoints", value = 25),
                                                         
                                                         ### parameter: seasonality.prior.scale
                                                         numericInput("seasonality_scale","seasonality.prior.scale", value = 10),
                                                         
                                                         ### parameter: changepoint.prior.scale
                                                         numericInput("changepoint_scale","changepoint.prior.scale", value = 0.05, step = 0.01)),
                                                  column(width = 6,
                                                         
                                                         ### parameter: holidays.prior.scale
                                                         numericInput("holidays_scale","holidays.prior.scale", value = 10),
                                                         
                                                         ### parameter: mcmc.samples
                                                         numericInput("mcmc.samples", "mcmc.samples", value = 0),
                                                         
                                                         ### parameter: interval.width
                                                         numericInput("interval.width", "interval.width", value= 0.8, step = 0.1),
                                                         ### parameter: uncertainty.samples
                                                         numericInput("uncertainty.samples","uncertainty.samples", value = 1000))
                                                  
                                           ),
                                           ## predict parameters --------------------
                                           column(width = 4,
                                                  tags$h3("Predict Parameters"),
                                                  ### paramater: periods
                                                  numericInput("periods","periods",value=365),
                                                  ### parameter: freq
                                                  selectInput("freq","freq",
                                                              choices = c('day', 'week', 'month', 'quarter','year')),
                                                  ### parameter: include_history
                                                  checkboxInput("include_history","include_history", value = TRUE)
                                           )
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
                                ## TAB 3 : Fit Propher Model ----------------------
                                tabPanel(title = "Fit Model", value = "panel3", 
                                         fluidRow(
                                           # box(width = 12, 
                                           column(width = 12,
                                                  shinyjs::disabled(actionButton("plot_btn2", "Fit Prophet Model",
                                                                                 style = "width:30%; margin-top: 25px; margin-bottom: 50px; font-size:150%; ")
                                                  )
                                           )
                                         ),
                                         
                                         ## Results Box : collapsible ------------------
                                         fluidRow(
                                           conditionalPanel("input.plot_btn2",
                                                            box(width = 12, collapsible = TRUE, 
                                                                title = "Results",
                                                                
                                                                div(id = "output-container3",
                                                                    tags$img(src = "spinner.gif",
                                                                             id = "loading-spinner"),
                                                                    DT::dataTableOutput("data")),
                                                                conditionalPanel("output.data",
                                                                                 uiOutput("dw_button")
                                                                )
                                                            )
                                           )),
                                         ## Plots Box : collapsible ------------------
                                         fluidRow( 
                                           conditionalPanel("input.plot_btn2",
                                                            box(width = 12, collapsible = TRUE, 
                                                                title = "Plots",
                                                                tabsetPanel(
                                                                  tabPanel("Forecast Plot",
                                                                           
                                                                           div(id = "output-container",
                                                                               # tags$img(src = "spinner.gif",
                                                                               #          id = "loading-spinner"),
                                                                               plotOutput("ts_plot")
                                                                           )
                                                                           # )
                                                                           
                                                                  ),
                                                                  tabPanel("Prophet Plot Components",
                                                                           # output.logistic_check=='no_error'
                                                                           conditionalPanel("input.plot_btn2",
                                                                                            div(id = "output-container",
                                                                                                # tags$img(src = "spinner.gif",
                                                                                                #          id = "loading-spinner"),
                                                                                                plotOutput("prophet_comp_plot"))
                                                                           )
                                                                  )
                                                                )))),
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