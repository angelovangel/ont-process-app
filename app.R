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
library(shinyFiles)
library(shinybusy)
library(digest)
library(readxl)

bin_on_path = function(bin) {
  exit_code = suppressWarnings(system2("command", args = c("-v", bin), stdout = FALSE))
  return(exit_code == 0)
}

emptysheet <- tibble(
  #well = lapply(1:12, function(x) {str_c(LETTERS[1:8], x)}) %>% unlist(),
  sample = NA,
  barcode = str_c('barcode', formatC(1:96, width = 2, flag = '0'))
)

sidebar <- sidebar(
  title = 'Controls',
  shiny::div(id = 'controls',
    checkboxInput('barcoded', 'Barcoded run', value = T),
    checkboxInput('report', 'Generate html report', value = T),
    fileInput('upload', 'Upload sample sheet', multiple = F, accept = c('.xlsx', '.csv'), placeholder = 'xlsx or csv file'),
    shinyDirButton("fastq_folder", "Select fastq_pass folder", title ='Please select a fastq_pass folder from a run', multiple = F),
    tags$hr(),
    actionButton('start', 'Start processing')
  )
)

cards <- list(
  card1 <- card(
    card_title(
      'Samplesheet preview',  
      tooltip(
        bsicons::bs_icon("question-circle"),
        "Upload xlsx/csv with columns 'sample' and 'barcode'. Could have other columns too",
        placement = "right")
    ),
    tableOutput('samplesheet')
  ),

  card2 <- card(
    card_title(
      'Live terminal view',
      tooltip(
        bsicons::bs_icon("question-circle"),
        "Preview of the terminal, for viewing the selected parameters and monitor output",
        placement = "right")
    ),
    verbatimTextOutput('stdout')
  )
)

ui <- page_navbar(
  useShinyjs(),
  fillable = T,
  title = 'ONT process run app',
  theme = bs_theme(bootswatch = 'yeti', primary = '#196F3D'),
  sidebar = sidebar,
  nav_panel(
    use_busy_spinner(spin = "double-bounce", position = 'top-right', color = '#CB4335'),
    title = '',
    layout_column_wrap(
      #width = 1/2,
      width = NULL, height = 500, fill = TRUE,
      style = htmltools::css(grid_template_columns = "1fr 3fr"),
      !!!cards
    )
  )
)

server <- function(input, output, session) {
  # check ont-process-run.sh is on path
  if (!bin_on_path('ont-process-run.sh')) {
    notify_failure('ont-process-run.sh not found', position = 'center-bottom')
  } else {
    notify_success('ont-process-run.sh is ready', position = 'center-bottom')
  }
  
  # reactives
  samplesheet <- reactive({
    file <- input$upload
  })
  
  # dir choose management --------------------------------------
  default_path <- Sys.getenv('DEFAULT_PATH')
  volumes <- c(ont_data = default_path, getVolumes()())
  shinyDirChoose(input, "fastq_folder", 
                 roots = volumes,
                 session = session,
                 restrictions = system.file(package = "base")) 
  
  # build arguments for main call and display them on stdout at the same time
  output$stdout <- renderPrint({
    if (is.integer(input$fastq_folder)) {
      cat("No fastq folder selected\n")
    } else if (is.null(samplesheet()$datapath)) {
      cat("No samplesheet uploaded")
    } else {
      # hard set fastq folder and build arguments
      selectedFolder <<- parseDirPath(volumes, input$fastq_folder)
      nfastq <<- length(list.files(path = selectedFolder, pattern = "*fast(q|q.gz)$", recursive = input$barcoded))
      
      htmlreport <- if_else(input$report, '-r', '')
      barcoded <- if_else(input$barcoded, '', '-n') 
      arguments <<- c('-p', selectedFolder, '-c', samplesheet()$datapath, htmlreport, barcoded)  
      
      #:) remove empty strings
      #arguments <- arguments[arguments != ""] 
      cat(
        'Selected folder:\n', selectedFolder, '\n', '-------\n\n',
        'Number of fastq files:\n', nfastq, '\n',  '-------\n\n',
        'Command:\n',
        'ont-process-run.sh', arguments)
      
    }
    })
  
  # observers
  observeEvent(input$start, {
    if (is.integer(input$fastq_folder)) {
      notify_failure('Please select a fastq_pass folder!', position = 'center-bottom')
      return()
    }
    # disable button while running
    shinyjs::disable('controls')
    shinyjs::html(id = 'start', 'Please wait...')
    show_spinner() # show the spinner
    
    withCallingHandlers({
      shinyjs::html(id = "stdout", "")
      p <- processx::run(echo_cmd = T,
        'ont-process-run.sh', args = arguments[arguments != ""] , 
        #wd = selectedFolder, 
        stderr_to_stdout = TRUE, error_on_status = FALSE, 
        stdout_line_callback = function(line, proc) {message(line)}
      )
      
    }, 
      message = function(m) {
        shinyjs::html(id = "stdout", html = m$message, add = TRUE); 
        runjs("document.getElementById('stdout').parentElement.scrollTo({ top: 1e9, behavior: 'smooth' });") 
        #runjs("window.scrollTo(0,9999);")
      }
    )
    
    # restore buttons on success
    if(p$status == 0) {
      notify_success(paste0('Procesing finished, results are in ', selectedFolder, '/processed'), position = 'center-bottom')
      shinyjs::enable('controls')
      shinyjs::html(id = 'start', 'Start processing')
      hide_spinner() # hide the spinner
    }
  })
  
  #outputs
  output$samplesheet <- renderTable({
    req(samplesheet())
    ext <- tools::file_ext(samplesheet()$datapath)
    validate(need(ext == 'csv' | ext == 'xlsx', 'Please upload a csv or excel file'))
    if (ext == 'csv') {
      read.csv(samplesheet()$datapath, header = T)
    } else if (ext == 'xlsx') {
      read_excel(samplesheet()$datapath)
    }
    
    
  })
  
}

shinyApp(ui, server)