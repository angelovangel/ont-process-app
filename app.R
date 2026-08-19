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

kill_process_tree <- function(pid) {
  # Recursively kill process tree to ensure children are also terminated
  # Wait up to 2 seconds for the process to exit to prevent .fuse_hidden files
  cmd <- sprintf("killtree() { local pid=$1; kill -stop $pid 2>/dev/null; for child in $(pgrep -P $pid 2>/dev/null); do killtree $child; done; kill -TERM $pid 2>/dev/null; }; killtree %1$d; for i in {1..10}; do if ! kill -0 %1$d 2>/dev/null; then break; fi; sleep 0.2; done", pid)
  tryCatch(
    system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE),
    error = function(e) NULL
  )
}

emptysheet <- tibble(
  #well = lapply(1:12, function(x) {str_c(LETTERS[1:8], x)}) %>% unlist(),
  sample = NA,
  barcode = str_c('barcode', formatC(1:96, width = 2, flag = '0'))
)

# Persistent temp directory inside the app (survives R session restarts)
app_tmp_dir <- file.path(getwd(), "tmp")
dir.create(app_tmp_dir, showWarnings = FALSE, recursive = TRUE)

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
      numericInput('subsample', 'Subsample reads for report', value = 0.1, min = 0.1, max = 1.0, step = 0.1),
      div(style="margin-bottom:10px")
    ),
    
    #uiOutput('usedocker'),
    actionButton('start', 'Start processing'),
    div(style="margin-bottom:10px"),
    actionButton('reset', 'Reset inputs'),
    div(style="margin-bottom:10px"),
    uiOutput('download_report')
  ),
  div(style="margin-bottom:10px"),
  shinyjs::hidden(actionButton('kill', 'Kill process', class = 'btn-danger', style = 'width:100%'))
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
  
  # --- Reactive state (replaces <<- globals) ---
  rv <- reactiveValues(
    selected_folder = NULL,
    sample_sheet    = NULL,
    nfastq          = NULL,
    arguments       = NULL,
    log_file        = NULL,   # path to stdout/stderr capture file
    status_file     = NULL,   # path to file written with exit code on completion
    is_running      = FALSE,  # TRUE while process is alive
    show_log        = FALSE,  # TRUE once a run has started (stays TRUE after completion)
    report_requested = FALSE
  )
  
  # --- Per-user state file (persists across reconnects) ---
  user_state_file <- reactive({
    req(res_auth$user)
    file.path(app_tmp_dir, paste0("state_", res_auth$user, ".rds"))
  })
  
  # --- On session start: reconnect to a previous/running process ---
  observe({
    req(res_auth$user)
    sf <- user_state_file()
    if (!file.exists(sf)) return()
    
    state <- readRDS(sf)
    
    # Case 1: process already finished while we were disconnected
    if (!is.null(state$status_file) && file.exists(state$status_file)) {
      status <- trimws(readLines(state$status_file, warn = FALSE)[1])
      # Show the final log
      if (!is.null(state$log_file) && file.exists(state$log_file)) {
        rv$log_file  <- state$log_file
        rv$show_log  <- TRUE
      }
      # Restore report button if run succeeded
      if (status == "0" && !is.null(state$report_hash)) {
        rh <- state$report_hash
        output$download_report <- renderUI({
          actionButton('report', 'View HTML report',
            onclick = sprintf("window.open('%s', '_blank')", rh))
        })
        notify_success('Previous run completed. Report is available.', position = 'center-bottom')
      } else if (status != "0") {
        notify_failure('Previous run failed.', position = 'center-bottom')
      }
      return()
    }
    
    # Case 2: process may still be running — check PID
    if (!is.null(state$pid)) {
      pid_alive <- tryCatch(
        system2("kill", c("-0", as.character(state$pid)), stdout = FALSE, stderr = FALSE) == 0,
        error = function(e) FALSE
      )
      if (pid_alive && !is.null(state$log_file) && file.exists(state$log_file)) {
        rv$log_file        <- state$log_file
        rv$status_file     <- state$status_file
        rv$is_running      <- TRUE
        rv$show_log        <- TRUE
        rv$report_requested <- isTRUE(state$report)
        rv$selected_folder <- state$selected_folder
        
        shinyjs::disable('controls')
        shinyjs::show('kill')
        shinyjs::html(id = 'start', 'Please wait...')
        show_spinner()
        notify_success('Reconnected to running process', position = 'center-bottom')
      }
    }
  })
  
  # --- Reactives ---
  samplesheet <- reactive({ input$upload })
  
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
  
  # --- Poll log file every 500ms (active whether running or just showing final output) ---
  poll_log_content <- reactivePoll(
    500, session,
    checkFunc = function() {
      lf <- rv$log_file
      if (is.null(lf) || !file.exists(lf)) return(0)
      file.info(lf)$size
    },
    valueFunc = function() {
      lf <- rv$log_file
      if (is.null(lf) || !file.exists(lf)) return("")
      paste(readLines(lf, warn = FALSE), collapse = "\n")
    }
  )
  
  # --- Build arguments reactively (independent of render) ---
  observe({
    if (is.integer(input$fastq_folder)) return()
    
    selectedFolder <- parseDirPath(volumes, input$fastq_folder)
    rv$selected_folder <- selectedFolder
    
    if (isTRUE(input$barcoded) && !is.null(samplesheet()$datapath)) {
      rv$sample_sheet <- samplesheet()$datapath
    } else {
      rv$sample_sheet <- input$sample_name
    }
    
    rv$nfastq <- length(list.files(path = selectedFolder, pattern = "*fast(q|q.gz)$", recursive = isTRUE(input$barcoded)))
    
    htmlreport <- ifelse(isTRUE(input$report), '-r', '')
    subsample <- if(isTRUE(input$report)) c('-s', input$subsample) else ''
    barcoded   <- ifelse(isTRUE(input$barcoded), '', '-n')
    rv$arguments <- c('-p', selectedFolder, '-c', rv$sample_sheet, htmlreport, subsample, barcoded)
  })
  
  # --- Terminal output: command preview OR streaming log ---
  output$stdout <- renderPrint({
    if (rv$show_log) {
      cat(poll_log_content())
    } else {
      # Command preview mode
      if (is.integer(input$fastq_folder)) {
        cat("No fastq folder selected\n")
        shinyjs::disable('start')
      } else {
        cat(
          'Selected folder:\n', rv$selected_folder, '\n', '-------\n\n',
          'Number of fastq files:\n', rv$nfastq, '\n', '-------\n\n',
          'Command:\n',
          'ont-process-run.sh', rv$arguments
        )
      }
    }
  })
  
  # --- Auto-scroll terminal while running ---
  observe({
    req(rv$is_running)
    poll_log_content()  # take dependency so this fires on new log content
    runjs("var el = document.getElementById('stdout'); if(el) el.parentElement.scrollTo({ top: 1e9, behavior: 'smooth' });")
  })
  
  # --- Detect process completion by polling the status file ---
  observe({
    req(rv$is_running)
    invalidateLater(1000, session)
    
    sf <- rv$status_file
    if (is.null(sf) || !file.exists(sf)) return()
    
    # Status file appeared — process has exited
    status <- trimws(readLines(sf, warn = FALSE)[1])
    
    isolate({
      rv$is_running <- FALSE
      shinyjs::enable('controls')
      shinyjs::hide('kill')
      shinyjs::html(id = 'start', 'Start processing')
      hide_spinner()
      
      state_f <- user_state_file()
      state   <- if (file.exists(state_f)) readRDS(state_f) else list()
      
      if (status == "0") {
        folder <- rv$selected_folder
        notify_success(
          paste0('Processing finished, results are in ', folder, '/processed'),
          position = 'center-bottom'
        )
        
        if (isTRUE(rv$report_requested)) {
          report_hash  <- sprintf("faster-report-%s.html", digest::digest(runif(1), algo = 'crc32'))
          pathtoreport <- paste0(dirname(folder), '/processed/faster-report.html')
          if (file.exists(pathtoreport)) {
            system2('cp', args = c(pathtoreport, paste0('www/', report_hash)))
            rh <- report_hash
            output$download_report <- renderUI({
              actionButton('report', 'View HTML report',
                onclick = sprintf("window.open('%s', '_blank')", rh))
            })
            state$report_hash <- report_hash
          }
        }
        state$status <- "finished"
        saveRDS(state, state_f)
        
      } else {
        notify_failure('Processing failed!')
        state$status <- "failed"
        saveRDS(state, state_f)
      }
    })
  })
  
  # --- Observers ---
  # Validate selected folder name
  observeEvent(input$fastq_folder, {
    if (!is.integer(input$fastq_folder)) {
      path <- parseDirPath(volumes, input$fastq_folder)
      if (str_ends(path, 'fastq_pass|demux|combined')) {
        notify_success(path, position = 'center-center', timeout = 3000)
        shinyjs::enable('start')
      } else {
        notify_failure('Select a folder named fastq_pass, demux or combined!', position = 'center-center', timeout = 3000)
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
    
    args_clean <- rv$arguments[rv$arguments != ""]
    
    # Files inside app_tmp_dir (survives R restarts)
    run_id      <- digest::digest(Sys.time(), algo = 'crc32')
    log_file    <- file.path(app_tmp_dir, paste0("run_", run_id, ".log"))
    status_file <- file.path(app_tmp_dir, paste0("run_", run_id, ".status"))
    inner_script <- file.path(app_tmp_dir, paste0("run_", run_id, ".sh"))
    
    # Inner script: runs the job, then writes exit code to status_file.
    # status_file presence is used by the polling observer to detect completion
    # even if R has been restarted.
    writeLines(c(
      "#!/bin/bash",
      paste(c("ont-process-run.sh", shQuote(args_clean)), collapse = " "),
      paste0("echo $? > ", shQuote(status_file))
    ), inner_script)
    system2("chmod", c("+x", inner_script))
    
    # TRUE process detachment via shell backgrounding + nohup:
    #   1. R calls system(), which spawns a /bin/sh
    #   2. /bin/sh starts "nohup bash inner_script &" and exits immediately
    #   3. The OS reparents the nohup process to launchd/init (PID 1)
    #   4. It is now fully independent — NOT killed when R or Shiny die
    # nohup  : ignores SIGHUP
    # </dev/null : detaches stdin (prevents terminal-close HUP)
    # >>log 2>&1 : all output goes to log_file
    # & echo $! : captures PID before the outer sh exits
    pid_str <- system(paste0(
      "nohup bash ", shQuote(inner_script),
      " >> ", shQuote(log_file),
      " 2>&1 </dev/null & echo $!"
    ), intern = TRUE)
    actual_pid <- suppressWarnings(as.integer(trimws(pid_str[1])))
    
    # Update reactive state
    rv$log_file         <- log_file
    rv$status_file      <- status_file
    rv$is_running       <- TRUE
    rv$show_log         <- TRUE
    rv$report_requested <- input$report
    
    # Persist state for reconnect
    sf <- user_state_file()
    saveRDS(list(
      pid             = actual_pid,
      log_file        = log_file,
      status_file     = status_file,
      report          = input$report,
      selected_folder = rv$selected_folder
    ), sf)
    
    # Update UI
    shinyjs::disable('controls')
    shinyjs::show('kill')
    shinyjs::html(id = 'start', 'Please wait...')
    show_spinner()
  })
  
  observeEvent(input$reset, {
    # 1. Kill running process if one exists
    sf <- user_state_file()
    if (file.exists(sf)) {
      state <- tryCatch(readRDS(sf), error = function(e) NULL)
      if (!is.null(state$pid) && !is.na(state$pid)) {
        kill_process_tree(state$pid)
      }
      # 2. Remove the user state file
      file.remove(sf)
    }
    
    # 3. Wipe all run artefacts from tmp (logs, status files, scripts)
    tmp_files <- list.files(app_tmp_dir, 
                            pattern = "^run_.*\\.(log|status|sh)$", 
                            full.names = TRUE)
    if (length(tmp_files) > 0) file.remove(tmp_files)
    
    # 4. Reload the session (resets all inputs / reactive state)
    session$reload()
  })
  
  observeEvent(input$kill, {
    # 1. Kill running process if one exists
    sf <- user_state_file()
    if (file.exists(sf)) {
      state <- tryCatch(readRDS(sf), error = function(e) NULL)
      if (!is.null(state$pid) && !is.na(state$pid)) {
        kill_process_tree(state$pid)
      }
      # 2. Remove the user state file
      file.remove(sf)
    }
    
    # 3. Wipe all run artefacts from tmp (logs, status files, scripts)
    tmp_files <- list.files(app_tmp_dir, 
                            pattern = "^run_.*\\.(log|status|sh)$", 
                            full.names = TRUE)
    if (length(tmp_files) > 0) file.remove(tmp_files)
    
    # 4. Update UI manually without reloading
    rv$is_running <- FALSE
    shinyjs::enable('controls')
    shinyjs::html(id = 'start', 'Start processing')
    shinyjs::hide('kill')
    hide_spinner()
    notify_success('Process killed and cleaned up.', position = 'center-bottom')
  })
  
  # Samplesheet preview
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