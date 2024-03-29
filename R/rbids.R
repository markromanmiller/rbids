# Initialize

#' @export
bids <- function(root, readonly = T) {
  # TODO: error if root is not a character or longer than one

  if(!dir.exists(root) && readonly) {
    rlang::abort(paste0("Root directory `", root, "` does not exist"))
  }

  root <- tools::file_path_as_absolute(root)

  bids_dataset <- list(
    root = root,
    all_files = list.files(path = root, recursive = TRUE),
    readonly = readonly
  )
  class(bids_dataset) <- "bids_dataset"
  bids_dataset
}

# Regex helpers (all internal)

subject_capture <- "sub-(?<participant_id>[[:alnum:]]+)"
path_sep <- "[\\\\\\/]"
subject_backref <- "sub-(?P=participant_id)"

# Printing and querying data
#' @method print bids_dataset
#' @export
print.bids_dataset <- function(bd) {
  cat("BIDS format data consisting of ")
  cat(length(bd$all_files))
  cat(" files rooted at \"")
  cat(bd$root)
  cat("\"")
  invisible(bd)
}

#' @export
bids_subject_data_types = function(bd) {
  files_and_suffixes <- bids_match_path(
    bd,
    paste0(
      "^",
      subject_regex,
      path_sep,
      subject_backref,
      "_",
      "(?<data_type_suffix>[[:alnum:]]+)",
      ".tsv$"
    ),
    full.names = T # this isn't used, just provided for function signature.
  )
  sort(unique(files_and_suffixes$data_type_suffix))
}

#' @keywords Internal
all_files <- function(bd, full.names) {
  files <- bd$all_files
  if (full.names) {
    files <- file.path(bd$root, files)
  }
  files
}

#--- Workhorse search functions
#' @export
bids_match_path <- function(bd, pattern, full.names) {
  regexpr_result <- regexpr(pattern, bd$all_files, perl = T)

  matching_rows <- regexpr_result == 1
  starts <- attr(regexpr_result, "capture.start")[matching_rows, , drop = F]

  df <- data.frame(file_path = all_files(bd, full.names)[matching_rows])
  for (colname in colnames(starts)) {
    lengths <- attr(regexpr_result, "capture.length")[matching_rows, colname]
    ends <- starts[,colname] + lengths - 1
    df[colname] <- substr(bd$all_files[matching_rows], starts[,colname], ends)
  }

  df
}

#--- Specific data helpers
#' @export
bids_all_files <- function(bd, full.names = T) {
  data.frame(file_path = all_files(bd, full.names))
}

#' @export
bids_motion_regex <- function() {
  # I am unsure if this should be exported normally...
  paste0(
    "^",
    subject_capture,
    path_sep,
    "ses-(?<session_id>[[:alnum:]]+)/",
    "motion/",
    subject_backref,
    "_",
    "ses-(?P=session_id)",
    "_task-(?<task_label>[[:alnum:]]+)_motion.tsv$"
  )
}

#' @export
bids_motion <- function(bd, full.names = T) {
  bids_match_path(
    bd,
    bids_motion_regex(),
    full.names = full.names
  )
}

#' @export
bids_subject_data <- function(bd, suffix, full.names = T) {
  # bad name, perhaps subject-level data e.g.?
  bids_match_path(
    bd,
    paste0(
      "^sub-(?<participant_id>[[:alnum:]]+)[\\\\\\/]sub-\\g1_",
      suffix,
      ".tsv$"
    ),
    full.names = full.names
  )
}

#' @export
bids_subjects <- function(bd, full.names = T) {
  read_tsv(file.path(bd$root, "participants.tsv"))
}

#' @export
bids_sessions <- function(bd, full.names = T) {
  bids_subject_data(bd, "sessions", full.names = full.names)
}

#' @export
bids_events <- function(bd, full.names = T) {
  # TODO: this doesn't follow the convention - I'm not sure if it should though
  bids_match_path(
    bd,
    "ses-(?<session_id>[a-zA-Z0-9]+)_task-(?<task_label>[a-zA-Z0-9]+)_events.tsv",
    full.names = full.names
  )
}

# bids_table has a "file_path" column,
# ... becomes the arguments to read_tsv
#' @export
bids_read_tsvs <- function(bids_table, ...) {
  # select only the filenames, read all, and unnest
  bids_table %>%
    mutate(
      ...bids_readable = map(file_path, read_tsv, ...),
    ) %>%
    select(-file_path) %>%
    unnest(...bids_readable)
}

#' @keywords Internal
ensure_write_access <- function(bids_dataset) {
  if (is.null(bids_dataset$readonly) | bids_dataset$readonly != F) {
    rlang::abort("Bids dataset argument is readonly. Refusing to write.")
  }
}

#' @keywords Internal
#' @importFrom readr write_tsv read_tsv
write_tsv_at <- function(x, file, append = F) {
  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = T)
  }
  if (append && file.exists(file)) {
    ttsv <- read_tsv(file, col_names = T, n_max = 0, show_col_types = F)
    if (!isTRUE(all.equal(colnames(ttsv), colnames(x)))) {
      rlang::abort(paste0("Could not append to file `", file, "` because column names are not equivalent."))
    }
    write_tsv(x, file, append = T)
  } else {
    # either the file doesn't exist or we're not appending, so erase & include the header.
    write_tsv(x, file, append = F)
  }
}

#' @export
bids_write_motion_file <- function(participant_id, session_id, task_id, data, bids_dataset, append = F) {
  # ugh: I don't like the argument order here.
  # TODO: check if motion.json matches the file

  # TODO: ensure lengths too.

  ensure_write_access(bids_dataset)

  # dataset/sub-<label>/ses-<label>/motion/sub-<label>_ses-<label>_task-<label>_motion.tsv
  sub_label <- glue::glue("sub-{participant_id}")
  ses_label <- glue::glue("ses-{session_id}")
  destination <- glue::glue("{bids_dataset$root}/{sub_label}/{ses_label}/motion/{sub_label}_{ses_label}_task-{task_id}_motion.tsv")

  write_tsv_at(data, destination, append = append)
  "success"
}

#' @export
bids_write_motion_files <- function(df, bids_dataset, .progress = T, append = F) {

  # this assumes df has columns of
  # participant_id, session_id, and data

  required_names <- c("participant_id", "session_id", "task_id", "data")
  match_test <- required_names %in% colnames(df)

  if (!all(match_test)) {
    rlang::abort(paste0("Missing column names: ", paste0(required_names[!match_test], collapse = ", ")))
  }

  if (.progress) {
    pb <- progress::progress_bar$new(total = nrow(df))
    pb$tick(0)
  }

  for (i in 1:nrow(df)) {
    bids_write_motion_file(
      df$participant_id[[i]],
      df$session_id[[i]],
      df$task_id[[i]],
      df$data[[i]],
      bids_dataset,
      append = append
    )

    if (.progress) {
      pb$tick()
    }
  }

  invisible(df)
}



