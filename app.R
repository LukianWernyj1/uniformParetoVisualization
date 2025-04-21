library(shiny)
library(ggplot2)

pareto_pdf <- function(x, scale, shape) {
  ifelse(x >= scale, shape * scale^shape / x^(shape + 1), 0)
}

ui <- fluidPage(
  titlePanel("Visualizing the Uniform-Pareto Conjugate Prior Relationship"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("truebeta", "True Beta (Uniform upper bound):", value = 10, min = 1, max = 100),
      numericInput("n", "Number of draws (n):", value = 10, min = 1, max = 100),
      actionButton("resample", "🔁 Redraw Uniform Samples"),
      br(), br(),
      sliderInput("draw_index", "Draw Number:", min = 1, max = 10, value = 1, step = 1,
                  animate = animationOptions(interval = 1500)),
      verbatimTextOutput("draw_info")
    ),
    mainPanel(
      plotOutput("draw_plot", height = "300px"),
      plotOutput("pareto_plot", height = "350px")
    )
  )
)

server <- function(input, output, session) {
  
  values <- reactiveValues(draws = NULL, posteriorscale = NULL, posteriorshape = NULL)
  
  observeEvent(c(input$resample, input$n, input$truebeta), {
    n <- input$n
    truebeta <- input$truebeta
    
    values$draws <- runif(n, min = 0, max = truebeta)
    values$posteriorscale <- numeric(n)
    values$posteriorshape <- numeric(n)
    currmax <- -Inf
    
    for (i in 1:n) {
      currmax <- max(currmax, values$draws[i])
      values$posteriorscale[i] <- currmax
      values$posteriorshape[i] <- i
    }
    
    updateSliderInput(session, "draw_index", max = n, value = min(input$draw_index, n))
  }, ignoreInit = FALSE)
  
  output$draw_info <- renderText({
    i <- input$draw_index
    paste0("Draw #", i, ": ", round(values$draws[i], 3),
           "\nMax so far: ", round(values$posteriorscale[i], 3),
           "\nPareto(shape = ", i, ", scale = ", round(values$posteriorscale[i], 3), ")")
  })
  
  output$draw_plot <- renderPlot({
    i <- input$draw_index
    draw_df <- data.frame(index = 1:input$n, value = values$draws)
    
    ggplot(draw_df, aes(x = index, y = value)) +
      geom_col(fill = ifelse(draw_df$index == i, "orange", "lightblue")) +
      geom_text(aes(label = round(value, 2)), vjust = -0.5) +
      labs(title = "Uniform Draws", x = "Draw Number", y = "Value") +
      theme_minimal()
  })
  
  output$pareto_plot <- renderPlot({
    i <- input$draw_index
    scale <- values$posteriorscale[i]
    shape <- values$posteriorshape[i]
    
    x_vals <- seq(0, max(15, scale + 1), length.out = 300)
    y_vals <- pareto_pdf(x_vals, scale, shape)
    df <- data.frame(x = x_vals, y = y_vals)
    
    ggplot(df, aes(x = x, y = y)) +
      geom_line(color = "blue", linewidth = 1.2) +
      labs(title = "Pareto Posterior", x = "Beta (Posterior Parameter)", y = "Density") +
      ylim(0, 2) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)