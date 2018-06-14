create_question <- function(id) {
  ns <- NS(id)

  uiOutput(ns("question"))
}

observe_questions <- function(input, output, session, rv, question) {
  observe({
    rv[[session$ns("question")]] <- input$radio
  })

  output$question <- renderUI({
    radioButtons(
      session$ns("radio"), label = h3(question),
      choices = list(
        "-2 - Disagree" = -2,
        "-1" = -1,
        "0 - Neutral" = 0,
        "1" = 1,
        "2 - Agree" = 2
      ),
      selected = 0
    )
  })
}
