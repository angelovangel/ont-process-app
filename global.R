# helper functions for 
# ont-process-app


# takes in samplesheet csv or xlsx, returns a list with
# a --> logical, TRUE if all checks pass
# b --> dataframe
# c --> index of bad rows
# d --> message to display

# to be used for rendering output$samplesheet
validate_samplesheet <- function(x) {
  require(readxl)
  require(vroom)
  require(tools)
  
  ext <- tools::file_ext(x)
  if (ext == 'csv') {
    df <- vroom(x, col_names = T, na = c("NA", "", " "), skip_empty_rows = T, trim_ws = T)
  } else if (ext == 'xlsx') {
    df <- read_excel(x, na = c("NA", "", " "), trim_ws = T)
  }
  
  # remove rows where sample or barcode is NA
  df <- df[complete.cases(df[ ,c('sample', 'barcode')]), ]
  nsamples <- nrow(df)
  
  # CHECKS
  # 
  bc_pattern <- '^barcode[0-9]+$' 
  # 
  sn_pattern <- '^(?!\\d+$)[A-Za-z0-9_-]{3,24}$'
  
  res <- list(a = TRUE, b = df, c = NA, d = paste0('Samplesheet OK: ', nsamples, ' samples'))
  
  # colnames contain 'sample', 'barcode'
  if (!all( c('sample', 'barcode') %in% colnames(df) )) {
    res$a <- FALSE
    res$d <- "Samplesheet must contain columns 'sample' and 'barcode'!"
  }
  
  # remove white space in sample names
  df$sample <- str_replace_all(df$sample, " ", "")
  
  # barcode unique
  # get indices of duplicates:) stupid R
  dups_vector <- duplicated(df$barcode) | duplicated(df$barcode, fromLast = T)
  if(any(dups_vector)) {
    res$a <- FALSE
    res$c <- which(dups_vector)
    res$d <- 'Barcodes must be unique!'
  }
  
  # sample names unique
  grouped_df <- df %>% group_by(sample) %>% mutate(n_samples = n())
  snames_vector <- grouped_df$n_samples == 1
  if (any(!snames_vector)) {
    res$a <- FALSE
    res$c <- c(res$c, which(!snames_vector))
    res$d <- 'Sample names must be unique!'
  }
  
  # sample names check
  sn_vector <- str_detect(df$sample, sn_pattern)
  if(!all(sn_vector)) {
    res$a <- FALSE
    res$c <- c(res$c, which(!sn_vector))
    res$d <- 'Sample names not valid!'
  }
  
  # barcode names check
  bc_vector <- str_detect(df$barcode, bc_pattern)
  if (!all(bc_vector)) {
    res$a <- FALSE
    res$c <- c(res$c, which(!bc_vector))
    res$d <- 'Barcode names not valid!'
  }
  
  res
  
}
