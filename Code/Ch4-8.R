library(shiny)

#3
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

ui <- fluidPage(
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = prod_codes,
                       width = "100%"
           )
    ),
    column(2, numericInput("rows", "Number of rows", value = 5, min = 1, max = 10)),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  max_no_rows <- reactive(
    max(length(unique(selected()$diag)),
        length(unique(selected()$body_part)),
        length(unique(selected()$location)))
  )
  
  observeEvent(input$code, {
    updateNumericInput(session, "rows", max = max_no_rows())
  })
  
  table_rows <- reactive(input$rows - 1)
  
  output$diag <- renderTable(
    count_top(selected(), diag, n = table_rows()), width = "100%"
  )
  output$body_part <- renderTable(
    count_top(selected(), body_part, n = table_rows()), width = "100%"
  )
  output$location <- renderTable(
    count_top(selected(), location, n = table_rows()), width = "100%"
  )
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  
  output$narrative <- renderText(narrative_sample())
}

shinyApp(ui, server)

#4
ui <- fluidPage(
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = prod_codes,
                       width = "100%"
           )
    ),
    column(2, numericInput("rows", "Number of rows", value = 5, min = 1, max = 10)),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("previousone", "Previous story")),
    column(2, actionButton("nextone", "Next story")),
    column(8, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  max_no_rows <- reactive(
    max(length(unique(selected()$diag)),
        length(unique(selected()$body_part)),
        length(unique(selected()$location)))
  )
  
  observeEvent(input$code, {
    updateNumericInput(session, "rows", max = max_no_rows())
  })
  
  table_rows <- reactive(input$rows - 1)
  
  output$diag <- renderTable(
    count_top(selected(), diag, n = table_rows()), width = "100%"
  )
  output$body_part <- renderTable(
    count_top(selected(), body_part, n = table_rows()), width = "100%"
  )
  output$location <- renderTable(
    count_top(selected(), location, n = table_rows()), width = "100%"
  )
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  
  max_no_stories <- reactive(length(selected()$narrative))
  
  story <- reactiveVal(1)
  
  observeEvent(input$code, {
    story(1)
  })
  
  observeEvent(input$nextone, {
    story((story() %% max_no_stories()) + 1)
  })
  
  observeEvent(input$previousone, {
    story((story() %% max_no_stories()) - 1)
  })
  
  output$narrative <- renderText({
    selected()$narrative[story()]
  })
}

shinyApp(ui, server)
