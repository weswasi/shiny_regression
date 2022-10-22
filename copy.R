library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(rmarkdown)
library(knitr)
library(pander)
library(faux)

plus_60 <- rnorm_multi(n = 200, 
                       mu = c(30, 60),
                       sd = c(5, 10),
                       r = 0.6, 
                       varnames = c("Age", "Safety"),
                       empirical = FALSE,
                       set.seed(100))

plus_60 <- plus_60 %>% 
  mutate_if(is.numeric, round)

# Define UI for application that draws a histogram
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
    
    # Sidebar with a slider input for number of bins
    fluidPage(
      theme = shinythemes::shinytheme("flatly"),
      sidebarLayout(
        sidebarPanel(
          tags$b("Data:"),
          textInput("x", "x", value = "90, 100, 90, 80, 87, 75", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
          textInput("y", "y", value = "950, 1100, 850, 750, 950, 775", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
        ),
        
        mainPanel(
          br(),
          DT::dataTableOutput("tbl"),
          br(),
          tags$b("Deskriptiv statistik:"),
          uiOutput("data"),
          br(),
          tags$b("Regressionsvärden från figuren:"),
          uiOutput("results"),
          plotlyOutput("plot"),
          br(),
          br()
        )
      )
    )
  ), 
)

server <- function(input, output) {
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
  # Data output
  output$tbl <- DT::renderDataTable({
    y <- extract(input$y)
    x <- extract(input$x)
    DT::datatable(data.frame(x, y),
                  extensions = "Buttons",
                  options = list(
                    lengthChange = FALSE,
                    dom = "Blfrtip",
                    buttons = list(list(extend = "csv", text = "Ladda ner dataset"))
                  )
    )
  })
  
  output$data <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    if (anyNA(x) | length(x) < 2 | anyNA(y) | length(y) < 2) {
      "Invalid input or not enough observations"
    } else if (length(x) != length(y)) {
      "Number of observations must be equal for x and y"
    } else {
      withMathJax(
        paste0("\\(\\bar{x} =\\) ", round(mean(x), 3)),
        br(),
        paste0("\\(\\bar{y} =\\) ", round(mean(y), 3)),
        br(),
        paste0("\\(n =\\) ", length(x))
      )
    }
  })
  
  output$plot <- renderPlotly({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    dat <- data.frame(x, y)
    p <- ggplot(dat, aes(x = x, y = y)) +
      geom_point() +
      stat_smooth(method = "lm", se = FALSE) +
      ylab(input$ylab) +
      xlab(input$xlab) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$results <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    withMathJax(
      paste0("Adj. \\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3)),
      br(),
      paste0("\\( \\beta_0 = \\) ", round(fit$coef[[1]], 3)),
      br(),
      paste0("\\( \\beta_1 = \\) ", round(fit$coef[[2]], 3)),
      br(),
      paste0("P-värde ", "\\( = \\) ", signif(summary(fit)$coef[2, 4], 3))
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)