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
library(digest)
library(shinyvalidate)
library(shinymanager)

#### needed by faster-report, load here to have them managed by renv and not have to use docker..
library(optparse)
library(R.utils)
library(funr)
#library(writexl)
library(knitr)
#library(DT)
library(reactable)
library(sparkline)
library(parallelMap)
library(jsonlite)
library(htmlwidgets)
library(scales)
source('global.R')
#### needed by faster-report, load here to have them managed by renv and not have to use docker..

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
    checkboxInput('barcoded', 'Barcoded run', value = F),
    uiOutput('nonbc_sample_name'),
    conditionalPanel(
      condition = "input.barcoded",
      fileInput('upload', 'Upload sample sheet', 
                multiple = F, accept = c('.xlsx', '.csv'), placeholder = 'xlsx or csv file')
    ),
    shinyDirButton("fastq_folder", "Select fastq_pass folder", title ='Please select a fastq_pass folder from a run', multiple = F),
    tags$hr(),
    checkboxInput('report', 'Generate html report', value = T),
    conditionalPanel(
      condition = "input.report",
      checkboxInput('docker', 'Used docker for report', value = F)
    ),
    #uiOutput('usedocker'),
    actionButton('start', 'Start processing'),
    div(style="margin-bottom:10px"),
    actionButton('reset', 'Reset inputs'),
    div(style="margin-bottom:10px"),
    uiOutput('download_report')
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
    reactableOutput('samplesheet')
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
    use_busy_spinner(spin = "double-bounce", position = 'top-right', color = '#E67E22'),
    title = '',
    layout_column_wrap(
      #width = 1/2,
      width = NULL, height = 500, fill = TRUE,
      style = htmltools::css(grid_template_columns = "1fr 2fr"),
      !!!cards
    )
  )
)
### secure app -----------------------------###
ui <- secure_app(ui,theme = "simplex")
credentials <- readRDS("credentials.rds")

server <- function(input, output, session) {
  
  ### secure app -----------------------------###
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  # check ont-process-run.sh is on path
  if (!bin_on_path('ont-process-run.sh')) {
    notify_failure('ont-process-run.sh not found', position = 'center-bottom')
  } else {
    notify_success('ont-process-run.sh is ready', position = 'center-bottom')
  }
  
  iv <- InputValidator$new()
  iv$add_rule('sample_name', sv_required())
  iv$add_rule('sample_name', sv_regex(pattern = "^[A-Za-z0-9_-]{3,}$", message = "At least 3 alphanumerics, underscore, dash, no white space"))
  iv$enable()
  
  # reactives
  samplesheet <- reactive({
      file <- input$upload
  })
  
  # render sample name input if nonbc run
  output$nonbc_sample_name <- renderUI({
    if (!input$barcoded) {
      textInput('sample_name', 'Sample name', value = '')
    }
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
      shinyjs::disable('start')
      
    #} else if (!input$barcoded) {
    } else {
      # hard set fastq folder and build arguments
      selectedFolder <<- parseDirPath(volumes, input$fastq_folder)
        if (input$barcoded && !is.null(samplesheet()$datapath)) {
          sample_sheet <<- samplesheet()$datapath
        } else {
          sample_sheet <<- input$sample_name
        }
        
      
      nfastq <<- length(list.files(path = selectedFolder, pattern = "*fast(q|q.gz)$", recursive = input$barcoded))
      
      htmlreport <- ifelse(input$report, '-r', '')
      barcoded <- ifelse(input$barcoded, '', '-n')
      docker <- ifelse(input$docker, '-d', '')
      
      arguments <<- c('-p', selectedFolder, '-c', sample_sheet, htmlreport, barcoded, docker)  
      
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
  # checks on fastq_pass selected
  observeEvent(input$fastq_folder, {
    # start checking if something is selected, initially it is integer
    if (!is.integer(input$fastq_folder)) {
      path <- parseDirPath(volumes, input$fastq_folder)
      if (str_ends(path, 'fastq_pass|demux')) {
        notify_success(path, position = 'center-center', timeout = 3000)
        shinyjs::enable('start')
      } else {
        notify_failure('Select a fastq_pass or demux folder!', position = 'center-center', timeout = 3000)
        shinyjs::disable('start')
      }
    }
  })
  
  observeEvent(input$start, {
    if (is.integer(input$fastq_folder)) {
      notify_failure('Please select a fastq_pass folder!', position = 'center-bottom')
      return()
    }
    
    if (!input$barcoded && !iv$is_valid()) {
      notify_failure('Please fix sample name!', position = 'center-bottom')
      return()
    }
    # disable button while running
    shinyjs::disable('controls')
    shinyjs::html(id = 'start', 'Please wait...')
    show_spinner() # show the spinner
    
    withCallingHandlers({
      shinyjs::html(id = "stdout", "")
      
        p <- processx::run(
          'ont-process-run.sh', 
          args = arguments[arguments != ""] , 
          echo_cmd = T,
          #wd = selectedFolder, 
          stderr_to_stdout = TRUE, error_on_status = FALSE, 
          stdout_line_callback = function(line, proc) {message(line)}
        )
    }, 
      message = function(m) {
        shinyjs::html(id = "stdout", html = m$message, add = TRUE)
        runjs("document.getElementById('stdout').parentElement.scrollTo({ top: 1e9, behavior: 'smooth' });") 
        #runjs("window.scrollTo(0,9999);")
      }
    )
    
    # restore buttons on success, 
    if(p$status == 0) {
      notify_success(paste0('Procesing finished, results are in ', selectedFolder, '/processed'), position = 'center-bottom')
      shinyjs::enable('controls')
      shinyjs::html(id = 'start', 'Start processing')
      hide_spinner() # hide the spinner
      
      #render download report button etc
      # generate hash for faster-report names, only of report is there
      if (input$report) {
        report_hash <- sprintf("%s-%s.html", 'faster-report', digest::digest(runif(1), algo = 'crc32') )
        pathtoreport <- paste0(dirname(selectedFolder), '/processed/faster-report.html')
        system2('cp', args = c(pathtoreport, paste0('www/', report_hash)))
        #shinyjs::html('stdout', paste0('Copying ', report_hash), add = T)
        
        output$download_report <- renderUI({
          actionButton(
            'report', 'View HTML report',
            onclick = sprintf("window.open('%s', '_blank')", report_hash)
          )
        })
      }
      
    } else {
      notify_failure('Processing failed!')
      shinyjs::enable('controls')
      shinyjs::html(id = 'start', 'Start processing')
      hide_spinner() # hide the spinner
    }
  })
  
  observeEvent(input$reset, {
    session$reload()
  })
  
  #outputs
  # because samplesheet is read here to preview, we can do some checks on it - see global.R
  output$samplesheet <- renderReactable({
    req(samplesheet(), input$barcoded)
    ext <- tools::file_ext(samplesheet()$datapath)
    shiny::validate(need(ext == 'csv' | ext == 'xlsx', 'Please upload a csv or excel file'))
    
    x <- validate_samplesheet(samplesheet()$datapath)
    if(x$a) {
      notify_success(x$d, position = 'center-center')
    } else {
      notify_failure(x$d, position = 'center-center')
      shinyjs::disable('start')
    }
    reactable(
      x$b, 
      compact = T, wrap = F, pagination = FALSE,
      rowStyle = function(index) {
        if (index %in% x$c) {
          list(background = "#f5b7b1")
        }
      } 
    )
  })
  
}
cleanup <- function() {
  rmfiles <- list.files(path = "www", pattern = "faster-report*", full.names = T)
  lapply(rmfiles, file.remove)
}
onStop(function() { cleanup() })
shinyApp(ui, server)