server <- function(input, output, session) {
  addClass(selector = "body", class = "sidebar-collapse")
  
  ## Next/Back Buttons actions (to be turned into modules)---------------------------
  observeEvent(input$next1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  observeEvent(input$next2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  
  observeEvent(input$back2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  
  observeEvent(input$back3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  observeEvent(input$back4, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  
  ## function: duplicatedReactive values -----------------------------
  duplicatedReactive <- function(signal){
    values <- reactiveValues(val = "")
    observe({
      values$val <- signal()
    })
    reactive(values$val)
  }
  
  ## read csv file main data------------------------------------------------
  dat <- reactive({
    req(input$ts_file)
    file_in <- input$ts_file
    read_csv(file_in$datapath)     # read csv
  })
  
  ## Toggle submit button state according to main data -----------------------
  observe({
    if (!(c("ds","y") %in% names(dat()) %>% mean == 1))
      shinyjs::disable("next1")
    else if (c("ds","y") %in% names(dat()) %>% mean == 1)
      shinyjs::enable("next1")
  })
  
  ## output: table of 1st 6 rows of uploaded main data ------------------
  output$uploaded_data <- renderTable({
    req(dat)
    head(dat())
  })
  
  ## panel status depending on main data ------------------------
  output$panelStatus <- reactive({
    nrow(dat()) > 0
  })
  
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
  
  ## Toggle submit button state according to data ---------------
  observe({
    if (!(c("ds","y") %in% names(dat()) %>% mean == 1))
      shinyjs::disable("plot_btn2")
    else if (c("ds","y") %in% names(dat()) %>% mean == 1)
      shinyjs::enable("plot_btn2")
  })
  
  ## generate holiday dataframe ---------------------------------
  holidays_upload <- reactive({
    if (input$holiday) {
      h <- dat() %$% 
        rships::create_df_holidays(
          begin = min(ds),
          end = max(ds) + days(input$periods)
      )
    } else h <- NULL
    return(h)
  })
  
  # ## output: table of 1st 6 rows of uploaded holidays ------------------
  # output$uploaded_holidays <- renderTable({
  #   req(holidays_upload)
  #   head(holidays_upload())
  # })
  # 
  # ## panel status depending on holidays ------------------------
  # output$panelStatus_holidays <- reactive({
  #   !(is.null(holidays_upload()))
  # })
  # 
  # outputOptions(output, "panelStatus_holidays", suspendWhenHidden = FALSE)
  
  ## create prophet model --------------------------------------------------
  prophet_model <- eventReactive(input$plot_btn2,{
    
    req(dat(), 
        # ("ds" %in% dat()), "y" %in% names(dat()),
        input$n.changepoints,
        input$seasonality_scale, input$changepoint_scale,
        input$holidays_scale, input$mcmc.samples,
        input$mcmc.samples, input$interval.width,
        input$uncertainty.samples)
    
    if (input$growth == "logistic") {
      validate(
        need(try("cap" %in% names(dat())),
             paste("Error: for logistic 'growth', the input dataframe",
                   "must have a column 'cap' that specifies the capacity at each 'ds'.")))
    }
    
    kk <- prophet(dat(),
                  growth = input$growth,
                  # changepoints = NULL,
                  # n.changepoints = input$n.changepoints,
                  # yearly.seasonality = input$yearly,
                  # weekly.seasonality = input$monthly,
                  holidays = holidays_upload(),
                  # seasonality.prior.scale = input$seasonality_scale,
                  # changepoint.prior.scale = input$changepoint_scale,
                  # holidays.prior.scale = input$holidays_scale,
                  # mcmc.samples = input$mcmc.samples,
                  # interval.width = input$interval.width,
                  # uncertainty.samples = input$uncertainty.samples,
                  fit = TRUE)
    
    return(kk)
  })
  
  ## dup reactive prophet_model ------------------------------
  p_model <- duplicatedReactive(prophet_model)
  
  ## Make dataframe with future dates for forecasting -------------
  future <- eventReactive(input$plot_btn2,{
    req(p_model(),input$periods, input$freq)
    make_future_dataframe(p_model(),
                          periods = input$periods,
                          freq = input$freq,
                          include_history = TRUE)
  })
  
  ## dup reactive future--------------------------
  p_future <- duplicatedReactive(future)
  
  ## predict future values -----------------------
  forecast <- reactive({
    req(prophet_model(), p_future())
    predict(prophet_model(), p_future())
  })
  
  ## dup reactive forecast--------------------------
  p_forecast <- duplicatedReactive(forecast)
  
  ## output :  datatable from forecast dataframe --------------------
  output$data <- renderDataTable({
    # req(logistic_check()!="error")
    DT::datatable(forecast(), 
                  options = list(scrollX = TRUE, pageLength = 5)) %>% 
      formatRound(columns = 2:17, digits = 4)
  })
  
  ## download button ----------------
  output$dw_button <- renderUI({
    req(forecast())
    downloadButton('downloadData', 'Download Data',
                   style = "width:20%;
                   margin-bottom: 25px;
                   margin-top: 25px;")
  })
  
  output$downloadData <- downloadHandler(
    filename = "forecast_data.csv",
    content = function(file) {
      write.csv(forecast(), file)
    }
  )
  
  ## output:  plot forecast -------------
  output$ts_plot <- renderPlot({
    # req(logistic_check()!="error")
    g <- plot(p_model(), forecast())
    g + theme_minimal() + expand_limits(y = 0)
  })
  
  ## output:plot prophet components --------------
  output$prophet_comp_plot <- renderPlot({
    # req(logistic_check()!="error")
    prophet_plot_components(p_model(), forecast(), 
                            yearly_start = 31 + 28 + 31)
  })
  
  ## error msg for main dataset------------------------
  output$msg_main_data <- renderUI({
    if (c("ds","y") %in% names(dat()) %>% mean != 1)
      "Invalid Input: dataframe should have at least two columns named (ds & y)"
  })
  
  ## error msg for holidays ------------------------
  # output$msg_holidays <- renderUI({
  #   if (c("ds","holiday") %in% names(holidays_upload()) %>% mean != 1)
  #     "Invalid Input: dataframe should have at least two columns named (ds & holiday)"
  # })
  
}