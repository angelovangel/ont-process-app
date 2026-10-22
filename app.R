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
library(reactable)


source('global.R')


bin_on_path = function(bin) {
  exit_code = suppressWarnings(system2("command", args = c("-v", bin), stdout = FALSE))
  return(exit_code == 0)
}

kill_process_tree <- function(pid) {
  # Recursively kill process tree to ensure children are also terminated
  # Each process is stopped first (no new children while walking the tree), then TERM + CONT:
  # a stopped process only acts on TERM once continued
  # Wait up to 2 seconds for the process to exit to prevent .fuse_hidden files
  cmd <- sprintf("killtree() { local pid=$1; kill -stop $pid 2>/dev/null; for child in $(pgrep -P $pid 2>/dev/null); do killtree $child; done; kill -TERM $pid 2>/dev/null; kill -CONT $pid 2>/dev/null; }; killtree %1$d; for i in {1..10}; do if ! kill -0 %1$d 2>/dev/null; then break; fi; sleep 0.2; done", pid)
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
# tmp/state_<user>.rds : the user's last run (pid, log/status files, outdir, www/ copies)
# tmp/run_<id>.{sh,log,status} : files of one run; removed with the user's next run, Reset or Kill
app_tmp_dir <- file.path(getwd(), "tmp")
dir.create(app_tmp_dir, showWarnings = FALSE, recursive = TRUE)

# tmp/run_<id>.{log,status,sh} of a run, from its log file
run_files <- function(log_file) {
  if (is.null(log_file)) return(character(0))
  paste0(sub('\\.log$', '', log_file), c('.log', '.status', '.sh'))
}

# TRUE only if pid is still this run's script (a finished run's pid may have been reused)
pid_is_run <- function(pid, log_file) {
  if (is.null(pid) || is.na(pid) || is.null(log_file)) return(FALSE)
  cmd <- suppressWarnings(tryCatch(
    system2('ps', c('-o', 'command=', '-p', as.character(pid)), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)))
  length(cmd) > 0 && grepl(basename(run_files(log_file)[3]), cmd[1], fixed = TRUE)
}

# copy an html output to www/ under a random name so it can be opened from the app
publish_html <- function(path, prefix) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  hash <- sprintf("%s-%s.html", prefix, digest::digest(runif(1), algo = 'crc32'))
  file.copy(path, file.path('www', hash), overwrite = TRUE)
  hash
}

# remove one user's last run: kill it if still running, delete its tmp/ and www/ files and the state file
clear_user_run <- function(state_f) {
  if (!file.exists(state_f)) return(invisible())
  state <- tryCatch(readRDS(state_f), error = function(e) list())
  if (pid_is_run(state$pid, state$log_file)) kill_process_tree(state$pid)
  hashes <- c(state$report_hash, state$summary_hash)
  www_files <- if (length(hashes)) file.path('www', hashes)
  unlink(c(run_files(state$log_file), www_files))
  file.remove(state_f)
  invisible()
}

# at startup: remove run files no user state refers to (left over from older versions / lost states)
local({
  states <- list.files(app_tmp_dir, pattern = '^state_.*\\.rds$', full.names = TRUE)
  keep <- as.character(unlist(lapply(states, function(f) {
    run_files(tryCatch(readRDS(f)$log_file, error = function(e) NULL))
  })))
  all_runs <- list.files(app_tmp_dir, pattern = '^run_.*\\.(log|status|sh)$', full.names = TRUE)
  # compare by file name (all in app_tmp_dir); base:: as dplyr masks setdiff
  unlink(all_runs[!basename(all_runs) %in% basename(keep)])
})

# label with a hover help icon
with_help <- function(label, text, placement = 'right') {
  tags$span(
    label, ' ',
    tooltip(bsicons::bs_icon('question-circle', class = 'text-muted'), text, placement = placement)
  )
}

# hover help on a button (wrapped, so the tooltip also shows while the button is disabled)
button_help <- function(button, text, placement = 'right', inline = FALSE) {
  tooltip(tags$div(style = if (inline) 'display:inline-block;' else 'display:block;', button), text, placement = placement)
}

# buttons opening the html outputs copied to www/ (NULL hashes are skipped)
report_buttons <- function(report_hash = NULL, summary_hash = NULL) {
  open_btn <- function(id, label, hash, icon_name) {
    if (is.null(hash)) return(NULL)
    actionButton(id, label, icon = icon(icon_name), class = 'btn-report',
                 onclick = sprintf("window.open('%s', '_blank')", hash))
  }
  div(class = 'bar-group',
    open_btn('view_report', 'View reads report', report_hash, 'file-lines'),
    open_btn('view_summary', 'View utag report', summary_hash, 'table-cells')
  )
}

sidebar <- sidebar(
  width = "400px",
  title = 'Controls',
  shiny::div(id = 'controls',
    checkboxInput('barcoded',
      with_help('Barcoded run', 'Tick if the run has several samples (ONT barcodes or utags). Untick for a single, non-barcoded sample.'),
      value = F),
    uiOutput('nonbc_sample_name'),
    conditionalPanel(
      condition = "input.barcoded",
      checkboxInput('custom_barcoding',
        with_help('utag demux (custom CDI/UDI)', 'Demultiplex custom utag amplicon libraries with ont-process-utag.sh (utag demux) instead of ont-process-run.sh. The utag pair (or a single utag with --single-end) identifies the sample. Select a fastq folder, e.g. fastq_pass/barcode01. Output is laid out like ont-process-run.sh, plus utag/ and a run summary report (read fate, patterns, plate heatmap).'),
        value = FALSE),
      conditionalPanel(
        condition = "input.barcoded && input.custom_barcoding",
        selectInput('bc_context',
          with_help('Context (-c)', 'Constant sequence 5\' of the utag. nbd: SQK-NBD114 reads, untrimmed; lsk: SQK-LSK114, no ONT barcode; mab: SQK-MAB114 reads, untrimmed; bare: ONT adapter/barcode already trimmed.'),
          choices = c('nbd', 'lsk', 'mab', 'bare'), selected = 'nbd'),
        conditionalPanel(
          condition = "input.bc_context == 'nbd'",
          textInput('bc_ont_barcode',
            with_help('ONT barcode (--ont-barcode)', 'Optional, NB01..NB96. The native barcode of this library; added to the flank for more specific matching. Leave empty to match reads of any NB barcode.'),
            value = '', placeholder = 'e.g. NB01')
        ),
        checkboxInput('bc_single_end',
          with_help('Single-end (--single-end)', 'One utag at the read start (fwd or rev) names the sample: unique dual indexes, e.g. MAB. The far read end is not used.'),
          value = FALSE),
        textInput('bc_primer_f',
          with_help('Forward primer (--primer-f)', 'Optional, 5\'->3\'. Leave empty for the default 16S 27F (AGRGTTYGATYMTGGCTCAG).'),
          value = '', placeholder = 'default 16S 27F'),
        textInput('bc_primer_r',
          with_help('Reverse primer (--primer-r)', 'Optional, 5\'->3\'. Leave empty for the default 16S 1492R (RGYTACCTTGTTACGACTT).'),
          value = '', placeholder = 'default 16S 1492R')
      ),
      uiOutput('upload_ui')
    ),
    uiOutput('fastq_folder_ui'),
    tags$hr(),
    checkboxInput('report',
      with_help('Generate html report', 'Run faster-report on the processed reads and make an html report available here.'),
      value = T),
    conditionalPanel(
      condition = "input.report",
      numericInput('subsample',
        with_help('Subsample reads for report', 'Fraction of reads (0.1-1.0) used for the html report. Lower is faster.'),
        value = 0.1, min = 0.1, max = 1.0, step = 0.1)
    )
  )
)

# command buttons, sticky on top of the main panel
command_bar <- div(
  class = 'command-bar',
  div(class = 'bar-group',
    button_help(actionButton('start', 'Start processing', icon = icon('play'), class = 'btn-primary'),
      'Start the run in the background with the command shown in the terminal view. It keeps running if you close the browser.',
      placement = 'bottom', inline = TRUE),
    conditionalPanel(
      condition = "input.barcoded && input.custom_barcoding",
      button_help(actionButton('run_context', 'Run utag context', icon = icon('magnifying-glass')),
        'Scan a sample of the reads (utag context, ~20,000 reads) to find which sequence sits before the utags and which context (-c) / ONT barcode to use. Uses the selected folder, primers and sample sheet. The output is shown in the terminal view; when done, a button opens the report.',
        placement = 'bottom', inline = TRUE)
    ),
    button_help(actionButton('reset', 'Reset inputs', icon = icon('rotate-right')),
      'Kill any running process, clear the inputs and remove the logs.',
      placement = 'bottom', inline = TRUE),
    shinyjs::hidden(actionButton('kill', 'Kill process', icon = icon('xmark'), class = 'btn-danger'))
  ),
  div(class = 'bar-spacer'),
  uiOutput('context_report_ui', inline = TRUE),
  uiOutput('download_report', inline = TRUE)
)

cards <- list(
  card1 <- card(
    height = "100%",
    style = "flex: 1 1 auto; width: 100%;",
    card_title(
      'Samplesheet preview',  
      tooltip(
        bsicons::bs_icon("question-circle"),
        "Upload xlsx/csv with columns 'sample' and 'barcode' (ont-process-run.sh), or for utag demux 'sample','plate','well' (or 'sample','utag-fwd','utag-rev'). Could have other columns too",
        placement = "right")
    ),
    reactableOutput('samplesheet', height = "100%")
  ),

  card2 <- card(
    height = "100%",
    style = "flex: 1 1 auto; width: 100%;",
    card_title(
      'Live terminal view',
      tooltip(
        bsicons::bs_icon("question-circle"),
        "Preview of the terminal, for viewing the selected parameters and monitor output",
        placement = "right")
    ),
    # fills the card below the title; scrolls itself (auto-scroll targets this div)
    div(
      style = "flex: 1 1 auto; min-height: 0; overflow: auto;",
      verbatimTextOutput('stdout')
    )
  )
)

ui <- page_navbar(
  useShinyjs(),
  fillable = T,
  title = 'ONT process run app',
  # Inter is linked from Google Fonts by the browser (local = FALSE: nothing downloaded on the server)
  theme = bs_theme(
    version = 5, primary = '#196F3D',
    base_font = font_google('Inter', local = FALSE),
    heading_font = font_google('Inter', local = FALSE),
    'font-size-base' = '0.9rem'
  ),
  tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css')),
  sidebar = sidebar,
  nav_panel(
    use_busy_spinner(spin = "double-bounce", position = 'top-right', color = '#E67E22'),
    title = '',
    fillable = TRUE,
    command_bar,
    div(
      style = "display: flex; flex: 1 1 auto; min-height: 0;",
      div(
        id = "leftPanel",
        style = "overflow: auto; width: calc((100% - 16px) / 3); flex: 0 0 calc((100% - 16px) / 3); min-width: 20%; max-width: 80%; display: flex; flex-direction: column;",
        card1
      ),
      div(
        id = "dragbar",
        style = "width: 16px; flex: 0 0 16px; cursor: col-resize; display: flex; align-items: center; justify-content: center; color: #999;",
        bsicons::bs_icon("grip-vertical")
      ),
      div(
        id = "rightPanel",
        style = "flex-grow: 1; overflow: auto; display: flex; flex-direction: column; min-width: 0;",
        card2
      )
    ),
    tags$script(HTML("
      $(document).on('mousedown', '#dragbar', function(e) {
        e.preventDefault();
        var container = $('#leftPanel').parent();
        $(document).on('mousemove.dragbar', function(e) {
          var newWidth = e.pageX - container.offset().left;
          $('#leftPanel').css({'width': newWidth + 'px', 'flex': '0 0 ' + newWidth + 'px'});
        });
        $(document).on('mouseup.dragbar', function() {
          $(document).off('mousemove.dragbar mouseup.dragbar');
        });
      });
    "))
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
  # check ont-process-utag.sh and utag are on path
  for (bin in c('ont-process-utag.sh', 'utag')) {
    if (!bin_on_path(bin)) notify_failure(paste(bin, 'not found on PATH'), position = 'center-bottom')
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
    report_requested = FALSE,
    outdir          = NULL,   # where results are written
    ctx_log         = NULL,   # utag context output, shown in the terminal view
    ctx_running     = FALSE,
    ctx_cmd         = NULL,   # utag context command line, shown above its output
    ctx_hash        = NULL    # www/ copy of the context report
  )
  ctx_proc <- NULL            # processx handle of the running utag context
  
  # --- Counters used to force fresh fileInput/shinyDirButton widgets on reset ---
  # (fileInput and shinyDirButton can't be reset via shinyjs::reset; re-rendering
  #  them with a new input id is the standard workaround)
  upload_id <- reactiveVal(0)
  dir_id    <- reactiveVal(0)
  
  # --- Lock inputs and command buttons while a run is going (they are in different containers) ---
  lock_ui <- function() {
    shinyjs::disable('controls')
    for (id in c('start', 'reset', 'run_context')) shinyjs::disable(id)
    shinyjs::show('kill')
    shinyjs::html(id = 'start', 'Please wait...')
  }
  unlock_ui <- function() {
    shinyjs::enable('controls')
    for (id in c('start', 'reset')) shinyjs::enable(id)
    if (!isTRUE(isolate(rv$ctx_running))) shinyjs::enable('run_context')
    shinyjs::hide('kill')
    shinyjs::html(id = 'start', as.character(tagList(icon('play'), 'Start processing')))
  }

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
      # Restore report buttons if run succeeded; www/ is emptied when the app stops,
      # so re-copy the html files from the output folder when needed
      if (status == "0") {
        missing_www <- function(hash) is.null(hash) || !file.exists(file.path('www', hash))
        if (isTRUE(state$report) && missing_www(state$report_hash))
          state$report_hash <- publish_html(file.path(state$outdir, 'faster-report.html'), 'faster-report')
        if (missing_www(state$summary_hash))
          state$summary_hash <- publish_html(file.path(state$outdir, 'run_summary.html'), 'utag-summary')
        saveRDS(state, sf)
        if (length(c(state$report_hash, state$summary_hash))) {
          rh <- state$report_hash
          ph <- state$summary_hash
          output$download_report <- renderUI({ report_buttons(rh, ph) })
          notify_success('Previous run completed. Report is available.', position = 'center-bottom')
        }
      } else {
        notify_failure('Previous run failed.', position = 'center-bottom')
      }
      return()
    }
    
    # Case 2: process may still be running — check PID
    if (!is.null(state$pid)) {
      if (pid_is_run(state$pid, state$log_file) && !is.null(state$log_file) && file.exists(state$log_file)) {
        rv$log_file        <- state$log_file
        rv$status_file     <- state$status_file
        rv$is_running      <- TRUE
        rv$show_log        <- TRUE
        rv$report_requested <- isTRUE(state$report)
        rv$selected_folder <- state$selected_folder
        rv$outdir          <- state$outdir
        
        lock_ui()
        show_spinner()
        notify_success('Reconnected to running process', position = 'center-bottom')
      }
    }
  })
  
  # --- Reactives ---
  samplesheet <- reactive({ input[[paste0('upload_', upload_id())]] })

  # utag demux mode (only valid for barcoded runs)
  is_utag <- reactive({ isTRUE(input$barcoded) && isTRUE(input$custom_barcoding) })

  
  # render sample name input if nonbc run
  output$nonbc_sample_name <- renderUI({
    if (!input$barcoded) {
      textInput('sample_name',
        with_help('Sample name', 'Name of the single sample in this run: at least 3 letters, digits, _ or -, no spaces.'),
        value = '')
    }
  })
  
  # Re-rendered with a fresh input id whenever upload_id() changes (e.g. on reset)
  output$upload_ui <- renderUI({
    help <- "csv/xlsx with columns 'sample' and 'barcode' (barcode01 ...). For utag demux: 'sample','plate','well' (or 'sample','utag-fwd','utag-rev'); optional, without it every plate-map well is written as a sample (Plate1-A1 ...)."
    fileInput(paste0('upload_', upload_id()), with_help('Upload sample sheet', help),
              multiple = F, accept = c('.xlsx', '.csv'), placeholder = 'xlsx or csv file')
  })
  
  # Re-rendered with a fresh input id whenever dir_id() changes (e.g. on reset)
  output$fastq_folder_ui <- renderUI({
    button_help(
      shinyDirButton(paste0('fastq_folder_', dir_id()), "Select fastq/bam pass folder",
                     title = 'Please select a fastq_pass/bam_pass folder from a run', multiple = F),
      'Folder with the reads: fastq_pass, bam_pass, demux or combined. For utag demux a fastq folder, e.g. fastq_pass or fastq_pass/barcode01 (searched recursively).')
  })
  
  # dir choose management --------------------------------------
  default_path <- Sys.getenv('DEFAULT_PATH')
  volumes <- c(ont_data = default_path, getVolumes()())
  
  # Re-register shinyDirChoose against the current dynamic id (fires once at
  # startup too, matching the initial dir_id() == 0 widget)
  observeEvent(dir_id(), {
    shinyDirChoose(input, paste0('fastq_folder_', dir_id()),
                   roots = volumes,
                   session = session,
                   restrictions = system.file(package = "base"))
  })
  
  # Current value of the (dynamically-id'd) folder-choose input
  fastq_folder_val <- reactive({ input[[paste0('fastq_folder_', dir_id())]] })
  
  # --- Poll log file every 500ms (active whether running or just showing final output) ---
  # main run log, or utag context output when no run log is shown
  current_log <- function() if (isTRUE(rv$show_log)) rv$log_file else rv$ctx_log
  poll_log_content <- reactivePoll(
    500, session,
    checkFunc = function() {
      lf <- current_log()
      if (is.null(lf) || !file.exists(lf)) return(0)
      file.info(lf)$size
    },
    valueFunc = function() {
      lf <- current_log()
      if (is.null(lf) || !file.exists(lf)) return("")
      paste(readLines(lf, warn = FALSE), collapse = "\n")
    }
  )
  
  # --- Reactive helpers for folder, samplesheet, files, and arguments ---
  selected_folder_path <- reactive({
    fv <- fastq_folder_val()
    if (is.null(fv) || is.integer(fv)) return(NULL)
    parseDirPath(volumes, fv)
  })
  
  sample_sheet_val <- reactive({
    if (isTRUE(input$barcoded)) {
      if (!is.null(samplesheet()$datapath)) {
        samplesheet()$datapath
      } else {
        NULL
      }
    } else {
      input$sample_name
    }
  })
  
  nfastq_val <- reactive({
    folder <- selected_folder_path()
    if (is.null(folder)) return(0)
    length(list.files(path = folder, pattern = "*fast(q|q.gz)$", recursive = isTRUE(input$barcoded)))
  })
  
  computed_command <- reactive({
    if (is_utag()) "ont-process-utag.sh" else "ont-process-run.sh"
  })

  # where results are written - mirrors ont-process-run.sh / ont-process-utag.sh:
  # <parent of folder>/processed; for a barcodeNN folder <run>/processed-barcodeNN
  processed_dir <- reactive({
    folder <- selected_folder_path()
    if (is.null(folder)) return(NULL)
    if (is_utag() && str_detect(basename(folder), '^barcode[0-9]+$')) {
      file.path(dirname(dirname(folder)), paste0('processed-', basename(folder)))
    } else {
      file.path(dirname(folder), 'processed')
    }
  })

  computed_arguments <- reactive({
    folder <- selected_folder_path()
    if (is.null(folder)) return(NULL)

    if (is_utag()) {
      # ont-process-utag.sh -p <dir> [-c sheet] [-r -s] -- <utag demux options>
      sheet <- sample_sheet_val()
      args <- c('-p', folder)
      if (!is.null(sheet) && nzchar(sheet))
        args <- c(args, '-c', sheet)
      if (isTRUE(input$report))
        args <- c(args, '-r', if (!is.null(input$subsample)) c('-s', input$subsample))
      args <- c(args, '--', '-c', input$bc_context)
      if (input$bc_context == 'nbd' && nzchar(trimws(input$bc_ont_barcode)))
        args <- c(args, '--ont-barcode', toupper(trimws(input$bc_ont_barcode)))
      if (isTRUE(input$bc_single_end))
        args <- c(args, '--single-end')
      if (nzchar(trimws(input$bc_primer_f)))
        args <- c(args, '--primer-f', toupper(trimws(input$bc_primer_f)))
      if (nzchar(trimws(input$bc_primer_r)))
        args <- c(args, '--primer-r', toupper(trimws(input$bc_primer_r)))
      args
    } else {
      sheet <- sample_sheet_val()
      htmlreport <- ifelse(isTRUE(input$report), '-r', '')
      subsample <- if(isTRUE(input$report) && !is.null(input$subsample)) c('-s', input$subsample) else ''
      barcoded   <- ifelse(isTRUE(input$barcoded), '', '-n')
      args <- c('-p', folder, '-c', sheet, htmlreport, subsample, barcoded)
      args[args != ""]
    }
  })
  
  # --- Terminal output: command preview OR streaming log ---
  output$stdout <- renderPrint({
    if (rv$show_log) {
      cat(poll_log_content())
    } else if (!is.null(rv$ctx_log)) {
      cat('utag context:\n', rv$ctx_cmd, '\n-------\n\n')
      cat(poll_log_content())
    } else {
      # Command preview mode
      folder <- selected_folder_path()
      if (is.null(folder)) {
        cat("No fastq folder selected\n")
        shinyjs::disable('start')
      } else {
        cat(
          'Selected folder:\n', folder, '\n', '-------\n\n',
          'Number of fastq files:\n', nfastq_val(), '\n', '-------\n\n',
          'Command:\n',
          computed_command(), computed_arguments()
        )
      }
    }
  })
  
  # --- Auto-scroll terminal while a run or utag context is going ---
  observe({
    req(rv$is_running || rv$ctx_running)
    poll_log_content()  # take dependency so this fires on new log content
    # delayed: the new output is rendered after this message arrives
    runjs("setTimeout(function() { var el = document.getElementById('stdout'); if(el) el.parentElement.scrollTo({ top: 1e9, behavior: 'smooth' }); }, 150);")
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
      unlock_ui()
      hide_spinner()
      
      state_f <- user_state_file()
      state   <- if (file.exists(state_f)) readRDS(state_f) else list()
      
      if (status == "0") {
        notify_success(
          paste0('Processing finished, results are in ', rv$outdir),
          position = 'center-bottom'
        )
        
        if (isTRUE(rv$report_requested))
          state$report_hash <- publish_html(file.path(rv$outdir, 'faster-report.html'), 'faster-report')
        state$summary_hash <- publish_html(file.path(rv$outdir, 'run_summary.html'), 'utag-summary')
        rh <- state$report_hash
        ph <- state$summary_hash
        output$download_report <- renderUI({ report_buttons(rh, ph) })
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
  observeEvent(list(fastq_folder_val(), is_utag()), {
    if (!isTRUE(rv$ctx_running)) rv$ctx_log <- NULL
    fv <- fastq_folder_val()
    if (!is.null(fv) && !is.integer(fv)) {
      path <- parseDirPath(volumes, fv)
      ok_pattern <- if (is_utag()) 'fastq_pass|demux|combined|barcode[0-9]+' else 'fastq_pass|bam_pass|demux|combined'
      if (str_ends(path, ok_pattern)) {
        notify_success(path, position = 'center-center', timeout = 3000)
        shinyjs::enable('start')
        shinyjs::runjs("$('#fastq_folder_ui button').addClass('btn-selected-ok');")
      } else {
        msg <- if (is_utag()) 'Select a fastq folder named fastq_pass, demux, combined or barcodeNN!' else 'Select a folder named fastq_pass, bam_pass, demux or combined!'
        notify_failure(msg, position = 'center-center', timeout = 3000)
        shinyjs::disable('start')
        shinyjs::runjs("$('#fastq_folder_ui button').removeClass('btn-selected-ok');")
      }
    }
  })
  
  observeEvent(input$start, {
    if (is.null(fastq_folder_val()) || is.integer(fastq_folder_val())) {
      notify_failure('Please select a fastq_pass folder!', position = 'center-bottom')
      return()
    }
    if (!isTRUE(input$barcoded) && !iv$is_valid()) {
      notify_failure('Please fix sample name!', position = 'center-bottom')
      return()
    }
    
    if (is_utag() && !is.null(input$bc_ont_barcode) && nzchar(trimws(input$bc_ont_barcode)) &&
        !str_detect(toupper(trimws(input$bc_ont_barcode)), '^NB(0[1-9]|[1-8][0-9]|9[0-6])$')) {
      notify_failure('ONT barcode must be NB01..NB96!', position = 'center-bottom')
      return()
    }
    
    args_clean <- computed_arguments()
    
    # Remove this user's previous run (tmp/ files, www/ copies) and its report buttons
    clear_user_run(user_state_file())
    output$download_report <- renderUI(NULL)
    rv$ctx_log <- NULL
    
    # Files inside app_tmp_dir (survives R restarts)
    run_id      <- digest::digest(Sys.time(), algo = 'crc32')
    log_file    <- file.path(app_tmp_dir, paste0("run_", run_id, ".log"))
    status_file <- file.path(app_tmp_dir, paste0("run_", run_id, ".status"))
    inner_script <- file.path(app_tmp_dir, paste0("run_", run_id, ".sh"))
    
    # Inner script: runs the job, then writes exit code to status_file.
    # status_file presence is used by the polling observer to detect completion
    # even if R has been restarted.
    cmd_clean <- computed_command()
    writeLines(c(
      "#!/bin/bash",
      # RStudio sets CLICOLOR_FORCE=1; barbell then writes ANSI codes into its output files
      "unset CLICOLOR_FORCE FORCE_COLOR",
      paste(c(cmd_clean, shQuote(args_clean)), collapse = " "),
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
    rv$selected_folder  <- selected_folder_path()
    rv$outdir           <- processed_dir()
    
    # Persist state for reconnect
    sf <- user_state_file()
    saveRDS(list(
      pid             = actual_pid,
      log_file        = log_file,
      status_file     = status_file,
      report          = input$report,
      selected_folder = rv$selected_folder,
      outdir          = rv$outdir
    ), sf)
    
    # Update UI
    lock_ui()
    show_spinner()
  })
  
  observeEvent(input$reset, {
    clear_context()
    # 1-3. Kill this user's process if still running, remove its tmp/ and www/ files and state
    clear_user_run(user_state_file())
    output$download_report <- renderUI(NULL)
    
    # 4. Reset inputs using shinyjs and clear reactive state
    shinyjs::reset('controls')
    upload_id(upload_id() + 1)  # force a fresh, empty fileInput widget
    dir_id(dir_id() + 1)        # force a fresh, empty shinyDirButton widget
    rv$selected_folder <- NULL
    rv$sample_sheet <- NULL
    rv$nfastq <- NULL
    rv$arguments <- NULL
    rv$log_file <- NULL
    rv$status_file <- NULL
    rv$is_running <- FALSE
    rv$show_log <- FALSE
    rv$report_requested <- FALSE
    rv$outdir <- NULL
    
    # Reset UI elements
    unlock_ui()
    shinyjs::disable('start')
    shinyjs::runjs("$('#fastq_folder_ui button').removeClass('btn-selected-ok');")
    shinyjs::runjs("$('#upload_ui .btn-file').removeClass('btn-selected-ok');")
    hide_spinner()
  })
  
  observeEvent(input$kill, {
    # 1-3. Kill this user's process if still running, remove its tmp/ and www/ files and state
    clear_user_run(user_state_file())
    output$download_report <- renderUI(NULL)
    
    # 4. Update UI manually without reloading
    rv$is_running <- FALSE
    unlock_ui()
    hide_spinner()
    notify_success('Process killed and cleaned up.', position = 'center-bottom')
  })
  
  # --- utag context: find the sequence before the utags, then show its report ---
  ctx_files <- reactive({
    req(res_auth$user)
    base <- file.path(app_tmp_dir, paste0("context_", res_auth$user))
    list(log = paste0(base, ".log"), html = paste0(base, ".html"))
  })

  # kill a running context scan, remove its files and www/ copy
  clear_context <- function() {
    if (!is.null(ctx_proc) && ctx_proc$is_alive()) ctx_proc$kill_tree()
    ctx_proc <<- NULL
    f <- isolate(ctx_files())
    unlink(c(f$log, f$html, if (!is.null(isolate(rv$ctx_hash))) file.path('www', isolate(rv$ctx_hash))))
    rv$ctx_hash <- NULL
    rv$ctx_log <- NULL
    if (isTRUE(isolate(rv$ctx_running)) && !isTRUE(isolate(rv$is_running))) hide_spinner()
    rv$ctx_running <- FALSE
    shinyjs::enable('run_context')
    shinyjs::html(id = 'run_context', as.character(tagList(icon('magnifying-glass'), 'Run utag context')))
  }

  output$context_report_ui <- renderUI({
    req(rv$ctx_hash)
    actionButton('view_context', 'View context report', icon = icon('magnifying-glass-chart'), class = 'btn-report',
      onclick = sprintf("window.open('%s', '_blank')", rv$ctx_hash))
  })

  observeEvent(input$run_context, {
    folder <- selected_folder_path()
    if (is.null(folder)) {
      notify_failure('Please select a fastq folder first!', position = 'center-bottom')
      return()
    }
    if (!bin_on_path('utag')) {
      notify_failure('utag not found on PATH', position = 'center-bottom')
      return()
    }
    clear_context()
    f <- ctx_files()

    args <- c('context', '--color', 'never', '-r', folder, '--html', f$html)
    sheet <- sample_sheet_val()
    if (!is.null(sheet) && nzchar(sheet))
      args <- c(args, '--samplesheet', sheet)
    if (nzchar(trimws(input$bc_primer_f)))
      args <- c(args, '--primer-f', toupper(trimws(input$bc_primer_f)))
    if (nzchar(trimws(input$bc_primer_r)))
      args <- c(args, '--primer-r', toupper(trimws(input$bc_primer_r)))

    rv$ctx_cmd <- paste('utag', paste(args, collapse = ' '))
    # CLICOLOR_FORCE (set by RStudio) would put ANSI codes into the output
    ctx_proc <<- processx::process$new(
      'utag', args, stdout = f$log, stderr = '2>&1',
      env = c('current', CLICOLOR_FORCE = '0', FORCE_COLOR = '0', NO_COLOR = '1'),
      cleanup_tree = TRUE
    )
    rv$ctx_log <- f$log
    rv$ctx_running <- TRUE
    shinyjs::disable('run_context')
    shinyjs::html(id = 'run_context', 'Please wait...')
    show_spinner()
  })

  # poll the context scan; on success copy the report to www/ and show it
  observe({
    req(rv$ctx_running)
    invalidateLater(1000, session)
    if (is.null(ctx_proc) || ctx_proc$is_alive()) return()
    status <- ctx_proc$get_exit_status()
    ctx_proc <<- NULL
    isolate({
      rv$ctx_running <- FALSE
      shinyjs::enable('run_context')
      shinyjs::html(id = 'run_context', as.character(tagList(icon('magnifying-glass'), 'Run utag context')))
      if (!isTRUE(rv$is_running)) hide_spinner()
      hash <- if (identical(status, 0L)) publish_html(ctx_files()$html, 'utag-context')
      if (!is.null(hash)) {
        rv$ctx_hash <- hash
        notify_success('utag context finished, report is available.', position = 'center-bottom')
      } else {
        notify_failure('utag context failed, see the terminal view', position = 'center-bottom')
      }
    })
  })
  session$onSessionEnded(function() {
    if (!is.null(ctx_proc) && ctx_proc$is_alive()) ctx_proc$kill_tree()
  })

  # Samplesheet preview
  output$samplesheet <- renderReactable({
    req(samplesheet(), input$barcoded)
    ext <- tools::file_ext(samplesheet()$datapath)
    shiny::validate(need(ext == 'csv' | ext == 'xlsx', 'Please upload a csv or excel file'))
    
    x <- validate_samplesheet(samplesheet()$datapath, mode = if (is_utag()) 'utag' else 'ont')
    if(x$a) {
      notify_success(x$d, position = 'center-center')
      shinyjs::runjs("$('#upload_ui .btn-file').addClass('btn-selected-ok');")
    } else {
      notify_failure(x$d, position = 'center-center')
      shinyjs::disable('start')
      shinyjs::runjs("$('#upload_ui .btn-file').removeClass('btn-selected-ok');")
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
  rmfiles <- list.files(path = "www", pattern = "^(faster-report|utag-summary|utag-context)-.*\\.html$", full.names = T)
  lapply(rmfiles, file.remove)
}
onStop(function() { cleanup() })
shinyApp(ui, server)