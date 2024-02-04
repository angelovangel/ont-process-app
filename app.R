library(shiny)
library(shinyWidgets)
library(shinyjs)
library(bslib)
library(bsicons)
library(shinyjs)
library(tibble)
library(stringr)
library(dplyr)
library(processx)
library(rhandsontable)
library(lubridate)
library(shinyFiles)
library(shinybusy)
library(digest)

emptysheet <- tibble(
  #well = lapply(1:12, function(x) {str_c(LETTERS[1:8], x)}) %>% unlist(),
  sample = NA,
  barcode = str_c('barcode', formatC(1:96, width = 2, flag = '0'))
)

sidebar <- sidebar(
  title = 'Select run',
  checkboxInput('barcoded', 'Barcoded run', value = T),
  checkboxInput('report', 'Generate html report', value = T),
  fileInput('upload', 'Upload sample sheet', multiple = F, accept = c('xlsx', 'csv'), placeholder = 'xlsx or csv file'),
  shinyDirButton("fastq_folder", "Select fastq_pass folder", title ='Please select a fastq_pass folder from a run', multiple = F),
  tags$hr(),
  actionButton('start', 'Start processing')
)

cards <- list(
  card1 <- card(
    card_title(
      'Samplesheet',  
      tooltip(
        bsicons::bs_icon("question-circle"),
        "Either upload xlsx/csv or paste samples in table",
        placement = "right")
    ),
    rHandsontableOutput('samplesheet', height = "600px")
  ),

  card2 <- card(
    card_title('Code view'),
    verbatimTextOutput('stdout')
  )
)

ui <- page_navbar(
  fillable = T,
  title = 'ONT process run app',
  theme = bs_theme(bootswatch = 'yeti'),
  sidebar = sidebar,
  nav_panel(
    title = '',
    verbatimTextOutput('stdout')
  )
)

server <- function(input, output, session) {
  #outputs
  output$stdout <- renderText({
    'bla'
  })
}

shinyApp(ui, server)