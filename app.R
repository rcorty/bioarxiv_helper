library(shiny)
library(stringr)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel('bioarXiv submission helper'),

  # Sidebar with a slider input for number of bins
  splitLayout(
    wellPanel(
      textInput(inputId = 'to_ital',
                label = 'words to italicize',
                value = 'homo sapiens'),
      textAreaInput(inputId = 'abstract_in',
                    label = 'abstract in plaintext',
                    value = 'This is a very important paper about homo sapiens.  You should read it!',
                    width = '100%'),
      submitButton()
    ),
    mainPanel(),
    wellPanel(
      textOutput(outputId = 'abstract_out'),
      marin = '5px'
    ),
    cellWidths = c('48%', '4%', '48%')
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  abstract <- reactive(x = { input$abstract_in } )

  to_ital <- reactive(
    str_trim(
      str_split(string = input$to_ital,
                pattern = ',',
                simplify = TRUE)
    )
  )

  output$abstract_out <-
    renderText(expr = italicize_words(words = to_ital(),
                                      text = abstract()))
}


italicize_words <- function(words, text) {

  for (word in words) {

    text <- str_replace_all(string = text,
                            pattern = word,
                            replacement = paste0('<i>', word, '</i>'))
  }

  return(text)

}


# Run the application
shinyApp(ui = ui, server = server)

