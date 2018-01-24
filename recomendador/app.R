library(shiny)
library(shinyjs)
library(tidyverse)
library(ggfortify)

source("functions.R")

example_person_data <- data_frame(
  name = c("Diego", "Diego", "Andres", "Andres", "Camilo", "Camilo"),
  question_nr = c(1, 2, 1, 2, 1, 2),
  question_position = c(2, -2, 1, -1, 0, 2)
)

example_question_data <- data_frame(
  question_nr = c(1, 2),
  thema = c("thema_1", "thema_2"),
  question = c("Que?", "Como?")
)

NUM_PAGES <- 3
NUM_QUESTIONS <- nrow(example_question_data)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Recomendador"),
  hidden(
    div(
      class = "page",
      id = paste0("step", 1),
      1:NUM_QUESTIONS %>%
        map(function(i) {
          create_question(i)
        })
    ),
    div(
      class = "page",
      id = paste0("step", 2),
      uiOutput("thema_select")
    ),
    div(
      class = "page",
      id = paste0("step", 3),
      tableOutput("table"),
      plotOutput("plot")
    )
  ),
  actionButton("prevBtn", "< Previous"),
  actionButton("nextBtn", "Next >")
)

server <- function(input, output) {
  rv <- reactiveValues(
    page = 1,
    themas = NULL
  )

  observe({
    rv$themas <- input$checkGroup
  })

  observe({
    toggleState(id = "prevBtn", condition = rv$page > 1)
    toggleState(id = "nextBtn", condition = rv$page < NUM_PAGES)
    hide(selector = ".page")
    show(paste0("step", rv$page))
  })

  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }

  observeEvent(input$prevBtn, navPage(-1))
  observeEvent(input$nextBtn, navPage(1))

  1:NUM_QUESTIONS %>%
    map(function(i) {
      callModule(observe_questions, i, rv, example_question_data$question[i])
    })

  output$thema_select <- renderUI({
    choices <- unique(example_question_data$thema) %>%
      set_names(unique(example_question_data$thema))
    checkboxGroupInput(
      "checkGroup", label = h3("Themas Importantas"),
      choices = choices
    )
  })

  output$table <- renderTable({
    example_question_data$user <- 1:NUM_QUESTIONS %>%
      map(function(i) {
        rv[[paste0(i, "-question")]]
      })

    example_question_data %>%
      mutate(thema_wheight = ifelse(thema %in% rv$themas, 3, 1)) %>%
      merge(example_person_data, by = "question_nr") %>%
      mutate(diff = (as.numeric(user) - question_position) * thema_wheight) %>%
      group_by(name) %>%
      summarise(score = sum(diff)) %>%
      arrange(score)
  })

  output$plot <- renderPlot({
    unweighted_positions_df <- 1:NUM_QUESTIONS %>%
      map(function(i) {
        rv[[paste0(i, "-question")]]
      }) %>%
      set_names(1:NUM_QUESTIONS) %>%
      as_data_frame() %>%
      gather(question_nr, question_position) %>%
      mutate(
        name = "You",
        question_nr = as.numeric(question_nr),
        question_position = as.numeric(question_position)
      ) %>%
      rbind(example_person_data)

    positions_df <- example_question_data %>%
      mutate(thema_wheight = ifelse(thema %in% rv$themas, 3, 1)) %>%
      merge(unweighted_positions_df, by = "question_nr") %>%
      mutate(question_position = question_position * thema_wheight) %>%
      select(name, question_nr, question_position) %>%
      spread(question_nr, question_position)

    rownames(positions_df) <- positions_df$name

    distance_matrix <- positions_df %>%
      select(-name) %>%
      dist()

    autoplot(
      cmdscale(distance_matrix, eig = TRUE), label = TRUE, label.size = 3, size = 0
    )
  })
}

shinyApp(ui = ui, server = server)
