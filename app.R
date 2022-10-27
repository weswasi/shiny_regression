library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(rmarkdown)
library(knitr)
library(faux)
library(DT)

# Define UI
ui <- shiny::tagList(
  withMathJax(), 
  includeCSS(path = "www/css/styles.css"), 
  
  tags$div(
    tags$div(
      class = "app_title", 
      
      titlePanel(
        title = "Enkel Regression - Kriminologiska Institutionen", 
        windowTitle = "Enkel Regression"
      ),
    ),
    
    # Sidebar
    fluidPage(
      theme = shinythemes::shinytheme("flatly"),
      sidebarLayout(
        sidebarPanel(
          selectInput("gruppid", 
                      label = strong("Grupp-ID"),
                      width = "33%",
                      choices = list(
                        "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", 
                        "12", "13", "13", "14", "15", "16", "17", "18", "19", "20")),
          sliderInput("samband", "Pearsons R",
                      min = -0.99, max = 0.99,
                      value = 0, step = 0.1),
          sliderInput("observationer", "Observationer",
                      min = 50, max = 1000,
                      value = 100, step = 10),
          radioButtons("regline"
                       ,"Anpassa en regressionslinje"
                       ,c("Ingen linje" = "lm_no",
                          "Regressionslinje" = "lm",
                          "Regressionslinje med 95 % konfidensintervall" = "lm_95",
                          "Regressionslinje med 99 % konfidensintervall" = "lm_99")
                       ,selected = "lm_no"
                       ,inline = FALSE),
          tags$b("Övrigt"),
          checkboxInput("avg", "Visa genomsnittlig trygghet", FALSE),
          checkboxInput("resid", "Visa residualer", FALSE),
          checkboxInput("outlier", "Introducera extremvärden", FALSE),
          checkboxInput("dummy", "Ålder som dummyvariabel (0 om <=29 år; 1 om >=30 år)", FALSE)
        ),
        
        # Main page
        mainPanel(
          br(),
          tags$b("Punktdiagram över ålder och upplevd trygghet:"),
          plotlyOutput("plot"),
          br(),
          tags$b("Statistisk modell:"),
          br(),
          helpText("$$Y_{trygghet} = b_0 + b_{ålder}$$"),
          br(),
          tags$b("Regressionsvärden från diagrammet:"),
          uiOutput("results"),
          br(),
          tags$b("Deskriptiv statistik:"),
          uiOutput("data"),
          br(),
          tags$b("Dataset:"),
          DT::dataTableOutput("tbl"),
          br(),
        )
      )
    )
  ), 
  
  tags$footer(
    tags$div(
      class = "footer_container", 
      
      includeHTML(path = "www/html/footer.html")
    )
  )
  
)

# Server
server <- function(input, output, session) {
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
  safety_data <- reactive({
    rnorm_multi(n = input$observationer, 
                mu = c(30, 60),
                sd = c(5, 10),
                r = input$samband, 
                varnames = c("Ålder", "Trygghet"),
                empirical = FALSE,
                set.seed(input$gruppid)) %>% 
      mutate_if(is.numeric, round, 1) %>% 
      mutate(Trygghet = round(Trygghet),
             Ålder = round(Ålder))
    
  })
  
  safety_data_outlier <- reactive({
    safety_data() %>% 
      add_row(Ålder = 
                ifelse(input$samband > 0, 1, 63),
              Trygghet = 100) %>% 
      add_row(Ålder = 
                ifelse(input$samband > 0, 2, 60),
              Trygghet = 100) %>% 
      add_row(Ålder = 
                ifelse(input$samband > 0, 4, 59),
              Trygghet = 100) %>% 
      mutate(Outlier = ifelse(Ålder == 63 | Ålder == 60 | Ålder == 59 | Ålder == 1 | Ålder == 2 | Ålder == 4, 
                              1, 0))
  })
  
  # Data output
  output$plot <- renderPlotly({
    
    validate(
      need(input$gruppid, "")
    )
    
    if (input$outlier == FALSE) {
      y <- safety_data()[,1]
      x <- safety_data()[,2]
      fit <- lm(x ~ y)
      safety_data <- safety_data() %>%  
        mutate(`Predicerad trygghet` = round(predict(fit), 2))
      
      {if (input$dummy) safety_data <- safety_data %>% 
          mutate(Ålder = ifelse(Ålder > 29, 1, 0))} 
      
      p <- safety_data %>% 
        ggplot(aes(x = Ålder, y = Trygghet)) +
        geom_point() +
        {if (input$avg) geom_hline(yintercept = mean(safety_data$Trygghet), linetype = "dashed", color = "gray50")} +
        {if (input$resid) geom_segment(aes(xend = Ålder, yend = `Predicerad trygghet`), alpha = .2)} + 
        
        {if (input$dummy == FALSE) {if (input$regline == "lm") stat_smooth(method = "lm", se = FALSE, fullrange = TRUE)}} +
        {if (input$dummy == TRUE) {if  (input$regline == "lm") stat_smooth(method = "lm", se = FALSE, fullrange = FALSE)}} +
        {if (input$dummy == FALSE) {if (input$regline == "lm_95") stat_smooth(method = "lm", se = TRUE, fullrange = TRUE)}} +
        {if (input$dummy == TRUE) {if (input$regline == "lm_95") stat_smooth(method = "lm", se = TRUE, fullrange = FALSE)}} +
        {if (input$dummy == FALSE) {if (input$regline == "lm_99") stat_smooth(method = "lm", se = TRUE, level = .99,  fullrange = TRUE)}} +
        {if (input$dummy == TRUE) {if (input$regline == "lm_99") stat_smooth(method = "lm", se = TRUE, level = .99, fullrange = FALSE)}} +
        
        {if (input$dummy == FALSE) scale_x_continuous(breaks = seq(0, 65, by = 5), limits = c(0, 65), expand = c(0,0))} +
        {if (input$dummy == TRUE) scale_x_continuous(breaks = seq(0, 1, by = 1), limits = c(-2, 3), expand = c(0,0))} +

        scale_y_continuous(breaks = seq(0, 120, by = 10), limits = c(0, 120, expand = c(0,0))) +
        ylab("Upplevd trygghet")
    }
    
    else {
      y <- safety_data_outlier()[,1]
      x <- safety_data_outlier()[,2]
      fit <- lm(x ~ y)
      safety_data_outlier <- safety_data_outlier() %>%  
        mutate(`Predicerad trygghet` = round(predict(fit), 2))
      
      {if (input$dummy) safety_data_outlier <- safety_data_outlier %>% 
          mutate(Ålder = ifelse(Ålder > 29, 1, 0))} 
      
      p <- safety_data_outlier %>% 
        ggplot(aes(x = Ålder, y = Trygghet)) +
        geom_point(aes(colour = factor(Outlier))) +
        {if (input$avg) geom_hline(yintercept = mean(safety_data_outlier$Trygghet), linetype = "dashed", color = "gray50")} +
        {if (input$resid) geom_segment(aes(xend = Ålder, yend = `Predicerad trygghet`), alpha = .2)} + 
        
        {if (input$dummy == FALSE) {if (input$regline == "lm") stat_smooth(method = "lm", se = FALSE, fullrange = TRUE)}} +
        {if (input$dummy == TRUE) {if  (input$regline == "lm") stat_smooth(method = "lm", se = FALSE, fullrange = FALSE)}} +
        {if (input$dummy == FALSE) {if (input$regline == "lm_95") stat_smooth(method = "lm", se = TRUE, fullrange = TRUE)}} +
        {if (input$dummy == TRUE) {if (input$regline == "lm_95") stat_smooth(method = "lm", se = TRUE, fullrange = FALSE)}} +
        {if (input$dummy == FALSE) {if (input$regline == "lm_99") stat_smooth(method = "lm", se = TRUE, level = .99,  fullrange = TRUE)}} +
        {if (input$dummy == TRUE) {if (input$regline == "lm_99") stat_smooth(method = "lm", se = TRUE, level = .99, fullrange = FALSE)}} +
        
        {if (input$dummy == FALSE) scale_x_continuous(breaks = seq(0, 65, by = 5), limits = c(0, 65), expand = c(0,0))} +
        {if (input$dummy == TRUE) scale_x_continuous(breaks = seq(0, 1, by = 1), limits = c(-2, 3), expand = c(0,0))} +
        scale_y_continuous(breaks = seq(0, 120, by = 10), limits = c(0, 120, expand = c(0,0))) +
        theme(legend.position = "none") +
        scale_color_manual(values=c("#000000", "#e06666")) +
        ylab("Upplevd trygghet")
    }
    ggplotly(p)
  })
  
  output$results <- renderUI({
    
    validate(
      need(input$gruppid, "")
    )
    
    if (input$outlier == FALSE) {
      
      safety_data <- safety_data()
      
      {if (input$dummy) safety_data <- safety_data() %>% 
          mutate(Ålder = ifelse(Ålder > 29, 1, 0))} 
      
      y <- safety_data[,1]
      x <- safety_data[,2]
      fit <- lm(x ~ y)
      withMathJax(
        paste0("\\( R^2 = \\) ", round(summary(fit)$r.squared, 2)),
        br(),
        paste0("\\( \\beta_0 = \\) ", round(fit$coef[[1]], 2)),
        br(),
        paste0("\\( \\beta_{ålder} = \\) ", round(fit$coef[[2]], 2)),
        br(),
        paste0("P-värde ", "\\( = \\) ",  
               ifelse(signif(summary(fit)$coef[2, 4], 3) < 0.001, "< 0.001", signif(summary(fit)$coef[2, 4], 2)))
      )
    }
    
    else {
      
      safety_data_outlier <- safety_data_outlier()
      
      {if (input$dummy) safety_data_outlier <- safety_data_outlier %>% 
          mutate(Ålder = ifelse(Ålder > 29, 1, 0))} 
      
      y <- safety_data_outlier[,1]
      x <- safety_data_outlier[,2]
      fit <- lm(x ~ y)
      withMathJax(
        paste0("\\( R^2 = \\) ", round(summary(fit)$r.squared, 2)),
        br(),
        paste0("\\( \\beta_0 = \\) ", round(fit$coef[[1]], 2)),
        br(),
        paste0("\\( \\beta_{ålder} = \\) ", round(fit$coef[[2]], 2)),
        br(),
        paste0("P-värde ", "\\( = \\) ",  
               ifelse(signif(summary(fit)$coef[2, 4], 3) < 0.001, "< 0.001", signif(summary(fit)$coef[2, 4], 2)))
      )
    }
  })
  
  output$data <- renderUI({
    
    validate(
      need(input$gruppid, "")
    )
    
    if (input$outlier == FALSE) {
      
      safety_data <- safety_data()
      
      {if (input$dummy) safety_data <- safety_data() %>% 
          mutate(Ålder = ifelse(Ålder > 29, 1, 0))} 
      
      y <- safety_data[,2]
      x <- safety_data[,1]
      withMathJax(
        paste0("\\(\\bar{x} =\\) ", round(mean(x), 2)),
        br(),
        paste0("\\(\\bar{y} =\\) ", round(mean(y), 2)),
        br(),
        paste0("\\(n =\\) ", length(x))
      )
    }
    
    else {
      
      safety_data_outlier <- safety_data_outlier()
      
      {if (input$dummy) safety_data_outlier <- safety_data_outlier %>% 
          mutate(Ålder = ifelse(Ålder > 29, 1, 0))} 
      
      y <- safety_data_outlier[,2]
      x <- safety_data_outlier[,1]
      withMathJax(
        paste0("\\(\\bar{x} =\\) ", round(mean(x), 2)),
        br(),
        paste0("\\(\\bar{y} =\\) ", round(mean(y), 2)),
        br(),
        paste0("\\(n =\\) ", length(x))
      )
    }
  })
  
  output$tbl <- renderDataTable(server = FALSE, {
    
    validate(
      need(input$gruppid, "")
    )
    
    if (input$outlier == FALSE) {
      
      safety_data <- safety_data()
      
      {if (input$dummy) safety_data <- safety_data() %>% 
          mutate(Ålder = ifelse(Ålder > 29, 1, 0))} 
      
      safety_data %>% 
        datatable(extensions = "Buttons",
                  rownames= FALSE,
                  options = list(
                    dom = "Bfrtip",
                    buttons = list(list(extend = "csv", text = "Ladda ner datasetet", exportOptions = list(
                      modifier = list(page = "all"))))))
    }
    
    else {
      
      safety_data_outlier <- safety_data_outlier()
      
      {if (input$dummy) safety_data_outlier <- safety_data_outlier %>% 
          mutate(Ålder = ifelse(Ålder > 29, 1, 0))} 
      
      safety_data_outlier %>% 
        datatable(extensions = "Buttons",
                  rownames= FALSE,
                  options = list(
                    dom = "Bfrtip",
                    buttons = list(list(extend = "csv", text = "Ladda ner datasetet", exportOptions = list(
                      modifier = list(page = "all"))))))
    }
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)