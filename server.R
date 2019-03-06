source("utilities.R")

server <- function(input, output, session) {
  addClass(selector = "body", class = "sidebar-collapse")
  
  ## Next/Back Buttons actions (to be turned into modules)----
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
  
  ## read csv file main data----
  dat <- reactive({
    req(input$ts_file)
    file_in <- input$ts_file
    data <- read_csv(file_in$datapath) # read csv
    # Make sure we have a date
    if (is_date(data, date_col)) {
      return(data)
    } else {
      # Figure out right date format on small sample, then parse
      data_s <- data %>% sample_n(100) %>% pull(date_col)
      possible_formats <- Filter(function(form) {
        strptime(data_s, format = form) %>% 
          {!any(is.na(.))}
        }, datetime_formats)
      data <- data %>% 
        parse_date(date_col, format = possible_formats[1])
      
      return(data)
    }
  })
  
  ## Create reactive radio buttons for selecting columns----
  output$dateColumn <- renderUI({
    req(dat)
    columns <- names(dat())
    radioButtons("dColumns", "Select Date column",
                 columns, selected = character(0)) # No default
  })
  
  output$actualColumn <- renderUI({
    req(dat)
    columns <- names(dat())
    
    columns_s <- c("No actuals column",
                   columns[columns != input$dColumns])
    
    radioButtons("aColumns", "Select Actuals column",
                 columns_s, selected = "No actuals column")
  })
  
  
  ## Toggle submit button state according to main data----
  observe({
    if (is.null(input$dColumns))
      shinyjs::disable("next1")
    else shinyjs::enable("next1")
  })
  
  ## output: table of 1st 6 rows of uploaded main data----
  output$uploaded_data <- renderTable({
    req(dat)
    dat() %>% 
      head() %>% 
      mutate(!!date_col := strftime(!!sym(date_col), "%Y/%m/%d"))
  })
  
  ## panel status depending on main data----
  output$panelStatus <- reactive({
    nrow(dat()) > 0
  })
  
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
  
  ## Toggle submit button state according to data----
  observe({
    if (!(c(date_col, y_col) %in% names(dat()) %>% mean == 1))
      shinyjs::disable("plot_btn2")
    else if (c(date_col, y_col) %in% names(dat()) %>% mean == 1)
      shinyjs::enable("plot_btn2")
  })
  
  ## Create filters based on imported data----
  columns_filter <- character(0)
  observeEvent(input$next1, {
    req(dat, input$dColumns, input$aColumns)
    cnames <- names(dat())
    if (input$aColumns == "No actuals column") {
      columns_filter <<- cnames[cnames != date_col]
    } else {
      columns_filter <<- cnames[!cnames %in% c(date_col, y_col)]
    }
    for (cname in columns_filter) {
      options_filter <- sort(unique(dplyr::pull(dat(), cname)))
      
      insertUI(
        selector = '#placeholder',
        where = "afterEnd",
        ui = column(width = 12/length(columns_filter),
                    pickerInput(paste0(cname, "_sel"), cname,
                                choices = options_filter,
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE))
        )
      )
    }
  })
  
  ## generate holiday dataframe----
  holidays_upload <- reactive({
    req(dat, input$dColumns)
    if (input$holiday) {
      dateRange <- dat() %>% pull(input$dColumns) %>% range
      h <- rships::create_df_holidays(
        begin = dateRange[1],
        end = dateRange[2] + lubridate::days(input$periods)
        )
    } else h <- NULL
    return(h)
  })
  
  ## create prophet model----
  filter_values <- eventReactive(input$next2, {
    lapply(columns_filter, function(cname) {
      input[[paste0(cname, "_sel")]]
    })
  })
  
  prophet_model <- eventReactive(input$plot_btn2, {
    req(dat, filter_values)
    
    data_cleaned <- dat() %>% 
      dynamic_filter(columns_filter, filter_values()) %>% 
      group_by(ds = !!sym(input$dColumns)) %>% {
        if (input$aColumns == "No actuals column") {
          summarise(., y = n())
        } else {
          summarise(., y = sum(!!sym(input$aColumns)))
        }
      }
      
    model <- data_cleaned %>% 
      prophet(growth = "linear",
              holidays = holidays_upload(),
              fit = TRUE)
    
    return(model)
  })
  
  ## dup reactive prophet_model----
  p_model <- duplicatedReactive(prophet_model)
  
  ## Make dataframe with future dates for forecasting----
  future <- eventReactive(input$plot_btn2,{
    req(p_model(),input$periods, input$freq)
    make_future_dataframe(p_model(),
                          periods = input$periods,
                          freq = input$freq,
                          include_history = TRUE)
  })
  
  ## dup reactive future----
  p_future <- duplicatedReactive(future)
  
  ## predict future values----
  forecast <- reactive({
    req(prophet_model(), p_future())
    predict(prophet_model(), p_future())
  })
  
  ## dup reactive forecast----
  p_forecast <- duplicatedReactive(forecast)
  
  ## output: datatable from forecast dataframe----
  output$data <- renderDataTable({
    DT::datatable(forecast(), 
                  options = list(scrollX = TRUE, pageLength = 5)) %>% 
      formatRound(columns = 2:17, digits = 4)
  })
  
  ## download button----
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
  
  ## output: plot forecast----
  output$ts_plot <- renderPlot({
    g <- plot(p_model(), forecast())
    g + expand_limits(y = 0)
  })
  
  ## output: plot prophet components----
  output$prophet_comp_plot <- renderPlot({
    prophet_plot_components(p_model(), forecast(), 
                            yearly_start = 31 + 28 + 31) # Start at April 1st
  })
  
  ## error msg for main dataset----
  output$msg_main_data <- renderUI({
    if (c(date_col, y_col) %in% names(dat()) %>% mean != 1)
      "Invalid Input: dataframe should have at least two columns named (ds & y)"
  })
}