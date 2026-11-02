# helper functions for 
# ont-process-app

# sequencing kits offered for 'dorado trim --sequencing-kit' (ont-process-run.sh -t)
trim_kits <- c(
  'SQK-NBD114-24', 'SQK-NBD114-96',
  'SQK-RBK114-24', 'SQK-RBK114-96',
  'SQK-16S114-24', 'SQK-RPB114-24', 'SQK-PCB114-24',
  'SQK-MAB114-24',
  'SQK-LSK114', 'SQK-RAD114', 'SQK-ULK114',
  'SQK-NBD111-24', 'SQK-NBD111-96', 'SQK-RBK004'
)

# primer presets for utag (--primer-f / --primer-r), 5'->3'
# selecting a set prefills the primer fields; other: sequences typed in by the user
primer_sets <- list(
  '16S' = c(f = 'AGRGTTYGATYMTGGCTCAG', r = 'RGYTACCTTGTTACGACTT'),        # 27F / 1492R (utag default)
  'ITS' = c(f = 'TCCGTAGGTGAACCTGCGG', r = 'TCCTCCGCTTATTGATATGC'),         # ITS1F / ITS4 from MAB kit
  '18S' = c(f = 'AACCTGGTTGATCCTGCCAGT', r = 'TGATCCTTCTGCAGGTTCACCTAC'),      # EukA / EukB
  'COI' = c(f = 'KBTCHACAAAYCAYAARGAYATHGG', r = 'TAAACYTCWGGRTGWCCRAARAAYCA'), # LoboF1 / LoboR1
  'other' = c(f = '', r = '')
)


# takes in samplesheet csv or xlsx, returns a list with
# a --> logical, TRUE if all checks pass
# b --> dataframe
# c --> index of bad rows
# d --> message to display

# to be used for rendering output$samplesheet
validate_samplesheet <- function(x, mode = c('ont', 'utag')) {
  require(readxl)
  require(vroom)
  require(tools)
  mode <- match.arg(mode)
  
  ext <- tools::file_ext(x)
  if (ext == 'csv') {
    df <- vroom(x, col_names = T, na = c("NA", "", " "), skip_empty_rows = T, trim_ws = T)
  } else if (ext == 'xlsx') {
    df <- read_excel(x, na = c("NA", "", " "), trim_ws = T)
  }
  
  sn_pattern <- '^(?!\\d+$)[A-Za-z0-9_-]{3,24}$'
  
  # --- utag demux schema: sample,plate,well | sample,well (Plate1-A1) | sample,utag-fwd,utag-rev ---
  if (mode == 'utag') {
    # utag reads headers case-insensitively
    colnames(df) <- str_to_lower(str_trim(colnames(df)))
    key_cols <- if (all(c('utag-fwd', 'utag-rev') %in% colnames(df))) {
      c('sample', 'utag-fwd', 'utag-rev')
    } else if (all(c('plate', 'well') %in% colnames(df))) {
      c('sample', 'plate', 'well')
    } else if ('well' %in% colnames(df)) {
      c('sample', 'well')
    } else {
      NULL
    }
    
    if (is.null(key_cols) || !'sample' %in% colnames(df)) {
      return(list(a = FALSE, b = df, c = NA,
                  d = "utag samplesheet must contain 'sample' and 'plate','well' (or 'utag-fwd','utag-rev')!"))
    }
    
    df <- df[complete.cases(df[ , key_cols]), ]
    df$sample <- str_replace_all(df$sample, ' ', '')
    nsamples <- nrow(df)
    res <- list(a = TRUE, b = df, c = NA,
                d = paste0('Samplesheet OK (utag): ', nsamples, ' samples'))
    
    # sample names unique
    snames_vector <- duplicated(df$sample) | duplicated(df$sample, fromLast = T)
    if (any(snames_vector)) {
      res$a <- FALSE
      res$c <- which(snames_vector)
      res$d <- 'Sample names must be unique!'
    }
    
    # one sample per well / tag pair
    pos <- do.call(paste, c(df[ , setdiff(key_cols, 'sample')], sep = '|'))
    pos_dups <- duplicated(pos) | duplicated(pos, fromLast = T)
    if (any(pos_dups)) {
      res$a <- FALSE
      res$c <- c(res$c, which(pos_dups))
      res$d <- 'Two samples in the same well / tag pair!'
    }
    
    # sample names check
    sn_vector <- str_detect(df$sample, sn_pattern)
    if (!all(sn_vector)) {
      res$a <- FALSE
      res$c <- c(res$c, which(!sn_vector))
      res$d <- 'Sample names not valid!'
    }
    
    return(res)
  }
  
  # --- ONT-style schema: sample, barcode ---
  bc_pattern <- '^barcode[0-9]+$'
  
  if (!all(c('sample', 'barcode') %in% colnames(df))) {
    return(list(a = FALSE, b = df, c = NA,
                d = "Samplesheet must contain columns 'sample' and 'barcode'!"))
  }

  df <- df[complete.cases(df[ , c('sample', 'barcode')]), ]
  nsamples <- nrow(df)

  res <- list(a = TRUE, b = df, c = NA, d = paste0('Samplesheet OK: ', nsamples, ' samples'))

  df$sample <- str_replace_all(df$sample, ' ', '')
  
  dups_vector <- duplicated(df$barcode) | duplicated(df$barcode, fromLast = T)
  if (any(dups_vector)) {
    res$a <- FALSE
    res$c <- which(dups_vector)
    res$d <- 'Barcodes must be unique!'
  }
  
  grouped_df <- df %>% group_by(sample) %>% mutate(n_samples = n())
  snames_vector <- grouped_df$n_samples == 1
  if (any(!snames_vector)) {
    res$a <- FALSE
    res$c <- c(res$c, which(!snames_vector))
    res$d <- 'Sample names must be unique!'
  }
  
  sn_vector <- str_detect(df$sample, sn_pattern)
  if (!all(sn_vector)) {
    res$a <- FALSE
    res$c <- c(res$c, which(!sn_vector))
    res$d <- 'Sample names not valid!'
  }
  
  bc_vector <- str_detect(df$barcode, bc_pattern)
  if (!all(bc_vector)) {
    res$a <- FALSE
    res$c <- c(res$c, which(!bc_vector))
    res$d <- 'Barcode names not valid!'
  }
  
  res
  
}
