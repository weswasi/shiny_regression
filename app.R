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
        title = "Enkel Regression - Metod II - Kriminologiska Institutionen", 
        windowTitle = "Enkel Regression"
      ),
    ),
    
    # Sidebar
    fluidPage(
      theme = shinythemes::shinytheme("flatly"),
      sidebarLayout(
        sidebarPanel(
          sliderInput("samband", "Pearsons r:",
                      min = -0.99, max = 0.99,
                      value = 0, step = 0.1),
          textInput("gruppid", "Grupp-ID", value = "1", placeholder = "Mata in grupp-ID"),
          checkboxInput("line", "Anpassa en regressionslinje", FALSE),
        ),
        
        mainPanel(
          tags$b("Punktdiagram"),
          plotlyOutput("plot"),
          br(),
          tags$b("Regressionsvärden från diagrammet:"),
          uiOutput("results"),
          br(),
          tags$b("Deskriptiv statistik:"),
          uiOutput("data"),
          br(),
          br(),
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

server <- function(input, output, session) {
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
  safety_data <- reactive({
    rnorm_multi(n = 200, 
                mu = c(30, 60),
                sd = c(5, 10),
                r = input$samband, 
                varnames = c("Ålder", "Trygghet"),
                empirical = FALSE,
                set.seed(input$gruppid))  %>% 
      mutate_if(is.numeric, round)
  })
  
  # Data output
  output$plot <- renderPlotly({
    
    validate(
      need(input$gruppid, "")
    )
    
    p <- safety_data() %>% 
      ggplot(aes(x = Ålder, y = Trygghet)) +
      geom_point() +
      {if (input$line) stat_smooth(method = "lm", colour="#e06666", se = FALSE, fullrange = TRUE)} +
      scale_x_continuous(breaks = seq(0, 45, by = 1), limits = c(0, 45), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 120, by = 10), limits = c(0, 120, expand = c(0,0))) +
      xlab("Ålder") +
      ylab("Upplevd trygghet")
    theme_gray()
    
    ggplotly(p)
  })
  
  output$data <- renderUI({
    
    validate(
      need(input$gruppid, "")
    )
    
    y <- safety_data()[,2]
    x <- safety_data()[,1]
    withMathJax(
      paste0("\\(\\bar{x} =\\) ", round(mean(x), 3)),
      br(),
      paste0("\\(\\bar{y} =\\) ", round(mean(y), 3)),
      br(),
      paste0("\\(n =\\) ", length(x))
    )
  })
  
  
  output$results <- renderUI({
    
    validate(
      need(input$gruppid, "")
    )
    
    y <- safety_data()[,1]
    x <- safety_data()[,2]
    fit <- lm(x ~ y)
    withMathJax(
      paste0("Adj. \\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3)),
      br(),
      paste0("\\( \\beta_0 = \\) ", round(fit$coef[[1]], 3)),
      br(),
      paste0("\\( \\beta_1 = \\) ", round(fit$coef[[2]], 3)),
      br(),
      paste0("P-värde ", "\\( = \\) ",  ifelse(signif(summary(fit)$coef[2, 4], 3) < 0.001, "< 0.001", signif(summary(fit)$coef[2, 4], 3)))
    )
  })
  
  output$tbl <- renderDataTable({
    
    validate(
      need(input$gruppid, "")
    )
    
    safety_data() %>% 
      datatable(extensions = "Buttons",
                rownames= FALSE,
                options = list(
                  lengthChange = FALSE,
                  dom = "Blfrtip",
                  buttons = list(list(extend = "csv", text = "Ladda ner dataset"))
                )
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)