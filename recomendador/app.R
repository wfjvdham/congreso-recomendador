library(shiny)
library(shinyjs)
library(tidyverse)

source("functions.R")

example_person_data <- data_frame(
  name = c("Diego", "Andres", "Camilo"),
  question_1 = c(4, 3, 2),
  question_2 = c(5, 2, 1)
)

example_question_data <- data_frame(
  id = c("question_1", "question_2"),
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
      checkboxGroupInput(
        "checkGroup", label = h3("Themas Importantas"),
        choices = list("Thema 1" = 1, "Thema 2" = 2)
      )
    ),
    div(
      class = "page",
      id = paste0("step", 3),
      tableOutput("table")
    )
  ),
  actionButton("prevBtn", "< Previous"),
  actionButton("nextBtn", "Next >")
)

server <- function(input, output) {
  rv <- reactiveValues(
    page = 1
  )

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

  output$table <- renderTable({
    #TODO calculate score here
    example_person_data %>%
      select(name)
  })
}

shinyApp(ui = ui, server = server)
