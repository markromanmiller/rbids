BidsData <- R6::R6Class("BIDS Data", list(

  #--- Initialize data
  root = NULL,
  all_files = NULL,
  write_perm = FALSE,
  initialize = function(root) {
    # TODO: length 1 and character type
    self$root <- root
    self$all_files <- list.files(path = root, recursive = TRUE)
  },

  #--- Query BIDS state and available functions
  print = function(...) {
    cat("Bids data rooted at \"")
    cat(self$root)
    cat("\"")
    invisible(self)
  },
  subject_data_types = function() {
    files_and_suffixes <- self$match_path(
      paste0(
        "^sub-(?<participant_id>[[:alnum:]]+)[\\\\\\/]sub-\\g1_(?<data_type_suffix>[[:alnum:]]+).tsv$"
      ),
      full.names = T
    )
    sort(unique(files_and_suffixes$data_type_suffix))
  },

  #--- Workhorse search functions
  match_path = function(pattern, full.names) {
    regexpr_result <- regexpr(pattern, self$all_files, perl = T)

    matching_rows <- regexpr_result == 1
    starts <- attr(regexpr_result, "capture.start")[matching_rows, , drop = F]

    file_path <- self$all_files[matching_rows]
    if (full.names) {
      file_path <- file.path(self$root, file_path)
    }

    df <- data.frame(file_path = file_path)
    for (colname in colnames(starts)) {
      df[colname] <- substr(self$all_files[matching_rows], starts[,colname], starts[,colname] + attr(regexpr_result, "capture.length")[matching_rows,colname] - 1)
    }

    df
  },

  #--- Specific data type helpers
  motion = function(full.names = T) {
    self$match_path(paste0(
      "^sub-(?<participant_id>[[:alnum:]]+)[\\\\\\/]ses-(?<session_id>[[:alnum:]]+)/",
      "motion/",
      "sub-\\g1_ses-\\g2_task-(?<task_label>[[:alnum:]]+)_motion.tsv$"
    ), full.names = full.names)
  },
  subject_data = function(suffix, full.names = T) {
    self$match_path(paste0(
      "^sub-(?<participant_id>[[:alnum:]]+)[\\\\\\/]sub-\\g1_",
      suffix,
      ".tsv$"
    ), full.names = full.names)
  },
  sessions = function(full.names = T) {
    self$subject_data("sessions", full.names = full.names)
  }
))

bids <- function(root) {
  BidsData$new(root = root)
}

# bids_table has a "file_path" column,
# ... becomes the arguments to read_tsv
bids_read_tsvs <- function(bids_table, ...) {
  # select only the filenames, read all, and unnest
  bids_table %>%
    mutate(
      ...bids_readable = map(file_path, read_tsv, ...),
    ) %>%
    select(-file_path) %>%
    unnest(...bids_readable)
}



