# Value limit enforced by the IBGE API on a single request. The documented
# limit is 100,000, but empirically requests fail above ~50,000 values
# (e.g. 44,560 values -> HTTP 200; 50,130 values -> HTTP 500).
ibge_value_limit <- 50000

# Cap on locality ids per request to keep URLs at a safe length
ibge_max_localities_per_request <- 500

#' Resolve the `chunk` argument into a per-request value limit
#'
#' @param chunk `TRUE` (API limit), `FALSE` (disable) or a positive number
#'   (custom limit).
#' @return The value limit, or `NULL` when chunking is disabled.
#' @noRd
resolve_chunk_limit <- function(chunk) {
  if (isFALSE(chunk)) return(NULL)
  if (isTRUE(chunk)) return(ibge_value_limit)
  if (is.numeric(chunk) && length(chunk) == 1 && !is.na(chunk) && chunk > 0) {
    return(chunk)
  }
  cli::cli_abort(c(
    "{.arg chunk} must be {.val TRUE}, {.val FALSE} or a positive number.",
    "i" = "Use {.val TRUE} to split queries above the API limit of {ibge_value_limit} values."
  ), call = NULL)
}

#' Period ids available for an aggregate (cached per session)
#' @noRd
get_cached_period_ids <- function(aggregate) {
  cache_key <- paste0("period_ids_", aggregate)

  if (exists(cache_key, envir = .ibger_cache)) {
    return(get(cache_key, envir = .ibger_cache))
  }

  data <- ibge_request(
    aggregate, "periodos",
    .label = glue::glue("period list for aggregate {aggregate}")
  )
  ids <- purrr::map_chr(data, function(p) pluck_chr(p, "id"))

  assign(cache_key, ids, envir = .ibger_cache)
  ids
}

#' Locality ids for an aggregate at a level (cached per session)
#' @noRd
get_cached_locality_ids <- function(aggregate, level) {
  cache_key <- paste0("locality_ids_", aggregate, "_", level)

  if (exists(cache_key, envir = .ibger_cache)) {
    return(get(cache_key, envir = .ibger_cache))
  }

  data <- ibge_request(
    aggregate, "localidades", level,
    .label = glue::glue("{level} localities for aggregate {aggregate}")
  )
  ids <- purrr::map_chr(data, function(loc) pluck_chr(loc, "id"))

  assign(cache_key, ids, envir = .ibger_cache)
  ids
}

#' Estimated number of variables in the result
#'
#' @param get_meta Zero-argument function returning the aggregate metadata
#'   (lazy, so metadata is only fetched when actually needed).
#' @noRd
estimate_n_variables <- function(get_meta, variable) {
  if (is.null(variable) || identical(variable, "allxp")) {
    return(max(1, nrow(get_meta()$variables)))
  }
  if (identical(variable, "all") || identical(variable, "todas")) {
    # "all" adds auto-generated percentage variables; assume up to twice
    return(max(1, 2 * nrow(get_meta()$variables)))
  }
  length(variable)
}

#' Estimated number of category combinations in the result
#'
#' Without `classificacao` the API returns only the "Total" combination,
#' so the default is 1.
#' @noRd
estimate_n_categories <- function(get_meta, classification) {
  if (is.null(classification) || !is.list(classification) ||
      length(classification) == 0) {
    return(1)
  }

  counts <- purrr::imap_dbl(classification, function(cats, cls_id) {
    if (identical(cats, "all") || identical(cats, "todos")) {
      cls <- get_meta()$classifications
      idx <- which(cls$id == as.character(cls_id))
      if (length(idx) == 0) return(1)
      max(1, nrow(cls$categories[[idx]]))
    } else {
      length(cats)
    }
  })

  prod(counts)
}

#' Estimated number of periods (without hitting the API)
#' @noRd
estimate_n_periods <- function(periods) {
  if (is.null(periods)) return(6)

  if (is.numeric(periods) && length(periods) == 1 && periods < 0) {
    return(abs(periods))
  }

  if (is.character(periods) && length(periods) == 1 &&
      grepl("^-\\d+$", periods)) {
    return(abs(as.numeric(periods)))
  }

  max(length(extract_numeric_periods(periods)), length(periods))
}

#' Resolve the concrete period ids requested (may hit the API)
#' @noRd
resolve_period_ids <- function(aggregate, periods) {
  all_ids <- get_cached_period_ids(aggregate)

  if (is.null(periods)) periods <- -6

  is_last_n <- (is.numeric(periods) && length(periods) == 1 && periods < 0) ||
    (is.character(periods) && length(periods) == 1 && grepl("^-\\d+$", periods))

  if (is_last_n) {
    n <- min(abs(as.numeric(periods)), length(all_ids))
    return(utils::tail(all_ids, n))
  }

  requested <- as.character(extract_numeric_periods(periods))
  ids <- all_ids[all_ids %in% requested]

  if (length(ids) == 0) {
    # Unrecognized form: keep the values as given
    ids <- unlist(strsplit(paste(periods, collapse = "|"), "\\|", fixed = FALSE))
  }

  ids
}

#' Resolve localities into countable units for chunking
#'
#' @return A list with `n` (locality count) and `units` (tibble with
#'   `level` and `id` columns, or `NULL` when the localities cannot be
#'   enumerated -- e.g. `"BR"` or an unrecognized format).
#' @noRd
resolve_localities_for_chunking <- function(aggregate, localities) {
  if (is.character(localities)) {
    codes <- toupper(localities)

    if (length(codes) == 1 && codes == "BR") {
      return(list(n = 1, units = NULL))
    }

    if (all(grepl("^N\\d+$", codes))) {
      ids <- purrr::map(codes, function(lv) get_cached_locality_ids(aggregate, lv))
      units <- tibble::tibble(
        level = rep(codes, lengths(ids)),
        id    = unlist(ids)
      )
      return(list(n = nrow(units), units = units))
    }

    return(list(n = 1, units = NULL))
  }

  if (is.list(localities) && !is.null(names(localities))) {
    units <- tibble::tibble(
      level = rep(names(localities), lengths(localities)),
      id    = as.character(unlist(localities, use.names = FALSE))
    )
    return(list(n = nrow(units), units = units))
  }

  list(n = 1, units = NULL)
}

#' Format a units tibble back into the API localities syntax
#' @noRd
units_to_localities_str <- function(units) {
  parts <- purrr::imap_chr(
    split(units$id, units$level),
    function(ids, level) paste0(level, "[", paste(ids, collapse = ","), "]")
  )
  paste(parts, collapse = "|")
}

#' Split a vector into consecutive groups of at most `size` elements
#' @noRd
split_in_groups <- function(x, size) {
  unname(split(x, ceiling(seq_along(x) / size)))
}

#' Plan how to split a query that exceeds the API value limit
#'
#' Estimates the result size (variables x periods x localities x category
#' combinations) and, when it exceeds `limit`, builds a list of smaller
#' requests: first splitting periods, then localities when a single period
#' is still too large.
#'
#' @param meta Aggregate metadata, or `NULL` to fetch lazily when needed.
#' @return `NULL` when a single request fits within `limit`; otherwise a
#'   list of chunks, each with `periods_str` and `localities_str`.
#' @noRd
build_chunk_plan <- function(aggregate, meta, variable, periods, localities,
                             classification, limit) {

  get_meta <- function() {
    if (is.null(meta)) meta <<- get_cached_metadata(aggregate)
    meta
  }

  n_var <- estimate_n_variables(get_meta, variable)
  n_cat <- estimate_n_categories(get_meta, classification)

  loc <- resolve_localities_for_chunking(aggregate, localities)
  base <- n_var * n_cat * loc$n

  if (base * estimate_n_periods(periods) <= limit) return(NULL)

  per_ids <- resolve_period_ids(aggregate, periods)
  if (base * length(per_ids) <= limit) return(NULL)

  # 1) Split periods: each chunk keeps the original localities
  p_max <- floor(limit / base)
  if (p_max >= 1) {
    per_groups <- split_in_groups(per_ids, p_max)
    localities_str <- format_localities(localities)
    return(purrr::map(per_groups, function(group) {
      list(
        periods_str    = paste(group, collapse = "|"),
        localities_str = localities_str
      )
    }))
  }

  # 2) A single period still exceeds the limit: split localities as well
  if (is.null(loc$units)) {
    cli::cli_alert_warning(paste0(
      "A single period may still exceed the API limit of {limit} values ",
      "and the localities cannot be subdivided. ",
      "Consider reducing variables or classification categories."
    ))
    localities_str <- format_localities(localities)
    return(purrr::map(per_ids, function(p) {
      list(periods_str = p, localities_str = localities_str)
    }))
  }

  per_loc <- n_var * n_cat
  if (per_loc > limit) {
    cli::cli_alert_warning(paste0(
      "A single period and locality may still exceed the API limit of ",
      "{limit} values. Consider reducing variables or classification ",
      "categories."
    ))
  }

  l_max <- max(1, min(floor(limit / per_loc), ibge_max_localities_per_request))
  row_groups <- split_in_groups(seq_len(nrow(loc$units)), l_max)

  chunks <- purrr::map(per_ids, function(p) {
    purrr::map(row_groups, function(rows) {
      list(
        periods_str    = p,
        localities_str = units_to_localities_str(loc$units[rows, , drop = FALSE])
      )
    })
  })

  do.call(c, chunks)
}
