# In-memory metadata cache (per session)
.ibger_cache <- new.env(parent = emptyenv())

#' Get metadata with cache
#' @noRd
get_cached_metadata <- function(agregado) {
  key <- as.character(agregado)
  
  if (exists(key, envir = .ibger_cache)) {
    return(get(key, envir = .ibger_cache))
  }
  
  meta <- ibge_metadata(agregado)
  assign(key, meta, envir = .ibger_cache)
  meta
}

#' Clear metadata cache
#'
#' Removes all cached metadata (aggregates and surveys), forcing fresh API
#' calls on subsequent requests.
#'
#' @export
ibge_clear_cache <- function() {
  rm(list = ls(envir = .ibger_cache), envir = .ibger_cache)
  rm(list = ls(envir = .ibger_survey_cache), envir = .ibger_survey_cache)
  cli::cli_alert_success("Metadata cache cleared.")
  invisible()
}

#' Validate geographic level against metadata
#' @noRd
validate_level <- function(meta, level) {
  valid <- c(
    meta$territorial_level$administrative,
    meta$territorial_level$special,
    meta$territorial_level$ibge
  )
  
  if (length(valid) == 0) return(invisible(TRUE))
  
  requested <- if (is.character(level)) level else character()
  invalid <- setdiff(requested, valid)
  
  if (length(invalid) > 0) {
    cli::cli_abort(c(
      "!" = "Geographic level(s) {.val {invalid}} not available for aggregate {meta$id}.",
      "i" = "Available levels: {.val {valid}}."
    ), call = NULL)
  }
  
  invisible(TRUE)
}

#' Validate localities against metadata
#' @noRd
validate_localities <- function(meta, localities) {
  valid <- c(
    meta$territorial_level$administrative,
    meta$territorial_level$special,
    meta$territorial_level$ibge
  )
  
  if (length(valid) == 0) return(invisible(TRUE))
  
  requested <- extract_levels(localities)
  if (length(requested) == 0) return(invisible(TRUE))
  
  invalid <- setdiff(requested, valid)
  
  if (length(invalid) > 0) {
    cli::cli_abort(c(
      "!" = "Geographic level(s) {.val {invalid}} not available for aggregate {meta$id}.",
      "i" = "Available levels: {.val {valid}}."
    ), call = NULL)
  }
  
  invisible(TRUE)
}

#' Validate periods against metadata
#' @noRd
validate_periods <- function(meta, periods) {
  periods_num <- extract_numeric_periods(periods)
  if (length(periods_num) == 0) return(invisible(TRUE))
  
  start <- as.numeric(meta$periodicity$start)
  end <- as.numeric(meta$periodicity$end)
  
  if (is.na(start) || is.na(end)) return(invisible(TRUE))
  
  out_of_range <- periods_num[periods_num < start | periods_num > end]
  
  if (length(out_of_range) > 0) {
    freq <- meta$periodicity$frequency %||% "N/A"
    cli::cli_abort(c(
      "!" = "Period(s) {.val {out_of_range}} out of range for aggregate {meta$id}.",
      "i" = "Valid range: {.val {start}} to {.val {end}} ({freq})."
    ), call = NULL)
  }
  
  invisible(TRUE)
}

#' Validate variables against metadata
#' @noRd
validate_variables <- function(meta, variable) {
  if (is.null(variable)) return(invisible(TRUE))
  if (identical(variable, "all") || identical(variable, "todas")) return(invisible(TRUE))
  if (identical(variable, "allxp")) return(invisible(TRUE))
  
  requested <- as.character(variable)
  valid <- meta$variables
  
  if (nrow(valid) == 0) return(invisible(TRUE))
  
  invalid <- setdiff(requested, valid$id)
  
  if (length(invalid) > 0) {
    disp <- paste0(valid$id, " - ", valid$name, " (", valid$unit, ")")
    cli::cli_abort(c(
      "!" = "Variable(s) {.val {invalid}} not found in aggregate {meta$id}.",
      "i" = "Available variables:",
      stats::setNames(paste(" ", disp), rep("", length(disp)))
    ), call = NULL)
  }
  
  invisible(TRUE)
}

#' Validate classifications against metadata
#' @noRd
validate_classifications <- function(meta, classification) {
  if (is.null(classification) || !is.list(classification)) return(invisible(TRUE))
  
  valid_cls <- meta$classifications
  if (nrow(valid_cls) == 0) return(invisible(TRUE))
  
  for (cls_id in names(classification)) {
    idx <- which(valid_cls$id == cls_id)
    
    if (length(idx) == 0) {
      disp <- paste0(valid_cls$id, " - ", valid_cls$name)
      cli::cli_abort(c(
        "!" = "Classification {.val {cls_id}} not found in aggregate {meta$id}.",
        "i" = "Available classifications:",
        stats::setNames(paste(" ", disp), rep("", length(disp)))
      ), call = NULL)
    }
    
    cats_requested <- classification[[cls_id]]
    if (identical(cats_requested, "all") || identical(cats_requested, "todos")) next
    
    cats_valid <- valid_cls$categories[[idx]]
    cats_chr <- as.character(cats_requested)
    cats_invalid <- setdiff(cats_chr, cats_valid$category_id)
    
    if (length(cats_invalid) > 0) {
      cls_name <- valid_cls$name[idx]
      n_total <- nrow(cats_valid)
      n_show <- min(10, n_total)
      preview <- paste0(
        cats_valid$category_id[seq_len(n_show)], " - ",
        cats_valid$category_name[seq_len(n_show)]
      )
      more <- if (n_total > n_show) {
        c("i" = "... and {n_total - n_show} more. Use {.code ibge_metadata({meta$id})$classifications} to see all.")
      }
      
      cli::cli_abort(c(
        "!" = "Category(ies) {.val {cats_invalid}} not found in classification {.val {cls_id}} ({cls_name}).",
        "i" = "First categories available ({n_total} total):",
        stats::setNames(paste(" ", preview), rep("", length(preview))),
        more
      ), call = NULL)
    }
  }
  
  invisible(TRUE)
}

#' Validate all parameters against metadata
#' @noRd
validate_query <- function(meta,
                           localities = NULL,
                           periods = NULL,
                           variable = NULL,
                           classification = NULL,
                           level = NULL) {
  
  if (!is.null(level))           validate_level(meta, level)
  if (!is.null(localities))      validate_localities(meta, localities)
  if (!is.null(periods))         validate_periods(meta, periods)
  if (!is.null(variable))        validate_variables(meta, variable)
  if (!is.null(classification))  validate_classifications(meta, classification)
  
  invisible(TRUE)
}

#' Extract geographic level codes from localities
#' @noRd
extract_levels <- function(localities) {
  if (is.null(localities)) return(character())
  
  if (is.character(localities)) {
    loc_str <- paste(localities, collapse = "|")
    
    if (grepl("^BR$", loc_str, ignore.case = TRUE)) {
      return("N1")
    }
    
    levels <- regmatches(loc_str, gregexpr("N\\d+", loc_str))[[1]]
    return(unique(levels))
  }
  
  if (is.list(localities) && !is.null(names(localities))) {
    return(unique(names(localities)))
  }
  
  character()
}

#' Extract positive numeric periods (ignores negative = last N)
#' @noRd
extract_numeric_periods <- function(periods) {
  if (is.null(periods)) return(numeric())
  
  if (is.numeric(periods) && length(periods) == 1 && periods < 0) {
    return(numeric())
  }
  
  if (is.numeric(periods)) {
    return(periods[periods > 0])
  }
  
  if (is.character(periods)) {
    per_str <- as.character(periods)
    if (grepl("^-\\d+$", per_str)) return(numeric())
    
    all_periods <- c()
    parts <- unlist(strsplit(per_str, "\\|"))
    for (p in parts) {
      if (grepl("-", p) && !grepl("^-", p)) {
        bounds <- as.numeric(unlist(strsplit(p, "-")))
        if (length(bounds) == 2 && all(!is.na(bounds))) {
          all_periods <- c(all_periods, seq(bounds[1], bounds[2]))
        }
      } else {
        val <- suppressWarnings(as.numeric(p))
        if (!is.na(val) && val > 0) {
          all_periods <- c(all_periods, val)
        }
      }
    }
    return(all_periods)
  }
  
  numeric()
}