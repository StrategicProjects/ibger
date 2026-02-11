#' Parse a SIDRA API URL into ibger parameters
#'
#' Converts a SIDRA API URL (from the SIDRA Query Builder or sidrar package)
#' into a human-readable breakdown of its parameters, enriched with names
#' from the aggregate metadata.
#'
#' @param url Character string. A SIDRA API URL, typically starting with
#'   `https://apisidra.ibge.gov.br/values/`.
#'
#' @return A list of class `sidra_query` with:
#'   - `aggregate`: list with `id` and `name`
#'   - `variables`: tibble with `id` and `name`
#'   - `periods`: character vector of period codes
#'   - `localities`: list of level/locality pairs
#'   - `classifications`: list of classification/category details
#'   - `ibger_call`: string with the equivalent `ibge_variables()` call
#'
#' @examples
#' \dontrun{
#' url <- "https://apisidra.ibge.gov.br/values/t/5434/n1/all/v/4090/p/last%201/c888/47946,56623"
#' parse_sidra_url(url)
#' }
#'
#' @export
parse_sidra_url <- function(url) {
  
  url <- utils::URLdecode(url)
  
  # Extract path after /values/
  path <- sub(".*?/values/", "", url, ignore.case = TRUE)
  path <- sub("\\?.*", "", path)  # remove query string if any
  path <- sub("/$", "", path)     # remove trailing slash
  
  segments <- strsplit(path, "/")[[1]]
  
  # Parse segments in pairs
  aggregate_id    <- NULL
  variables       <- character()
  periods         <- character()
  localities      <- list()
  classifications <- list()
  
  i <- 1
  while (i <= length(segments)) {
    seg <- segments[i]
    
    if (seg == "t" && i + 1 <= length(segments)) {
      aggregate_id <- segments[i + 1]
      i <- i + 2
      
    } else if (grepl("^n\\d+$", seg) && i + 1 <= length(segments)) {
      level <- toupper(paste0("N", sub("^n", "", seg)))
      codes <- segments[i + 1]
      localities[[length(localities) + 1]] <- list(level = level, codes = codes)
      i <- i + 2
      
    } else if (seg == "v" && i + 1 <= length(segments)) {
      variables <- strsplit(segments[i + 1], ",")[[1]]
      i <- i + 2
      
    } else if (seg == "p" && i + 1 <= length(segments)) {
      periods <- segments[i + 1]
      i <- i + 2
      
    } else if (grepl("^c\\d+$", seg) && i + 1 <= length(segments)) {
      cls_id <- sub("^c", "", seg)
      cats <- strsplit(segments[i + 1], ",")[[1]]
      classifications[[cls_id]] <- cats
      i <- i + 2
      
    } else {
      i <- i + 1
    }
  }
  
  if (is.null(aggregate_id)) {
    cli::cli_abort(c(
      "Could not find an aggregate ID in the URL.",
      "i" = "Expected a SIDRA API URL with {.code /t/{{id}}} in the path."
    ), call = NULL)
  }
  
  # Fetch metadata to resolve names
  meta <- get_cached_metadata(as.integer(aggregate_id))
  
  # --- Resolve variable names ---
  var_info <- if (length(variables) > 0 && !identical(variables, "allxp")) {
    matched <- meta$variables[meta$variables$id %in% variables, ]
    purrr::map_dfr(variables, function(vid) {
      row <- matched[matched$id == vid, ]
      if (nrow(row) > 0) row else tibble::tibble(id = vid, name = NA_character_, unit = NA_character_)
    })
  } else {
    meta$variables
  }
  
  # --- Resolve classification/category names ---
  cls_info <- purrr::imap(classifications, function(cats, cls_id) {
    cls_row <- meta$classifications[meta$classifications$id == cls_id, ]
    cls_name <- if (nrow(cls_row) > 0) cls_row$name else NA_character_
    
    if (identical(cats, "all")) {
      cat_detail <- tibble::tibble(category_id = "all", category_name = "(all categories)")
    } else if (nrow(cls_row) > 0) {
      all_cats <- cls_row$categories[[1]]
      cat_detail <- purrr::map_dfr(cats, function(cid) {
        row <- all_cats[all_cats$category_id == cid, ]
        if (nrow(row) > 0) {
          tibble::tibble(category_id = cid, category_name = row$category_name)
        } else {
          tibble::tibble(category_id = cid, category_name = NA_character_)
        }
      })
    } else {
      cat_detail <- tibble::tibble(category_id = cats, category_name = NA_character_)
    }
    
    list(id = cls_id, name = cls_name, categories = cat_detail)
  })
  
  # --- Resolve locality level names ---
  all_levels <- c(
    N1 = "Brazil", N2 = "Major region", N3 = "State (UF)",
    N6 = "Municipality", N7 = "Metropolitan area",
    N8 = "Mesoregion", N9 = "Microregion", N10 = "District",
    N11 = "Sub-district", N13 = "Legal Amazon",
    N14 = "Semiarid", N15 = "Immediate geographic region",
    N17 = "Intermediate geographic region"
  )
  
  loc_info <- purrr::map(localities, function(loc) {
    level_name <- all_levels[[loc$level]]
    if (is.null(level_name)) level_name <- loc$level
    list(level = loc$level, level_name = level_name, codes = loc$codes)
  })
  
  # --- Build equivalent ibge_variables() call ---
  ibger_call <- build_ibger_call(
    aggregate_id, var_info$id, periods, localities, classifications
  )
  
  result <- list(
    aggregate       = list(id = aggregate_id, name = meta$name),
    variables       = var_info,
    periods         = periods,
    localities      = loc_info,
    classifications = cls_info,
    ibger_call      = ibger_call,
    periodicity     = meta$periodicity
  )
  
  class(result) <- c("sidra_query", "list")
  result
}

#' Build equivalent ibge_variables() call as a string
#' @noRd
build_ibger_call <- function(aggregate_id, var_ids, periods, localities, classifications) {
  parts <- paste0("ibge_variables(\n  aggregate = ", aggregate_id)
  
  # Variable
  if (length(var_ids) > 0 && !identical(var_ids, "allxp")) {
    if (length(var_ids) == 1) {
      parts <- paste0(parts, ",\n  variable = ", var_ids)
    } else {
      parts <- paste0(parts, ",\n  variable = c(", paste(var_ids, collapse = ", "), ")")
    }
  }
  
  # Periods
  if (length(periods) > 0 && nchar(periods) > 0) {
    if (grepl("^last\\s+", periods, ignore.case = TRUE)) {
      n <- sub("^last\\s+", "", periods, ignore.case = TRUE)
      parts <- paste0(parts, ",\n  periods = -", n)
    } else {
      parts <- paste0(parts, ',\n  periods = "', periods, '"')
    }
  }
  
  # Localities
  if (length(localities) > 0) {
    loc_parts <- purrr::map_chr(localities, function(loc) {
      if (tolower(loc$codes) == "all" && loc$level == "N1") return('"BR"')
      if (tolower(loc$codes) == "all") return(paste0('"', loc$level, '"'))
      paste0(loc$level, " = c(", loc$codes, ")")
    })
    
    if (length(loc_parts) == 1 && grepl('^"', loc_parts)) {
      parts <- paste0(parts, ",\n  localities = ", loc_parts)
    } else if (length(loc_parts) == 1 && !grepl('^"', loc_parts)) {
      parts <- paste0(parts, ",\n  localities = list(", loc_parts, ")")
    } else {
      parts <- paste0(parts, ",\n  localities = list(", paste(loc_parts, collapse = ", "), ")")
    }
  }
  
  # Classifications
  if (length(classifications) > 0) {
    cls_parts <- purrr::imap_chr(classifications, function(cats, cls_id) {
      if (identical(cats, "all")) {
        paste0('"', cls_id, '" = "all"')
      } else if (length(cats) == 1) {
        paste0('"', cls_id, '" = ', cats)
      } else {
        paste0('"', cls_id, '" = c(', paste(cats, collapse = ", "), ")")
      }
    })
    parts <- paste0(parts, ",\n  classification = list(", paste(cls_parts, collapse = ", "), ")")
  }
  
  paste0(parts, "\n)")
}


#' @export
print.sidra_query <- function(x, ...) {
  
  cli::cli_h1("SIDRA Query")
  
  # Aggregate
  cli::cli_h2("Aggregate")
  cli::cli_text("{.strong {x$aggregate$id}}: {x$aggregate$name}")
  
  # Variables
  if (nrow(x$variables) > 0) {
    cli::cli_h2("Variables ({nrow(x$variables)})")
    for (i in seq_len(nrow(x$variables))) {
      v <- x$variables[i, ]
      unit_str <- if (!is.na(v$unit)) paste0(" (", v$unit, ")") else ""
      name_str <- if (!is.na(v$name)) v$name else "?"
      cli::cli_text("  {v$id}: {name_str}{unit_str}")
    }
  }
  
  # Periods
  cli::cli_h2("Periods")
  if (nchar(x$periods) > 0) {
    if (grepl("^last\\s+", x$periods, ignore.case = TRUE)) {
      n <- sub("^last\\s+", "", x$periods, ignore.case = TRUE)
      cli::cli_text("  Last {n} period{?s} ({x$periodicity$frequency})")
    } else {
      cli::cli_text("  {x$periods}")
    }
  }
  
  # Localities
  if (length(x$localities) > 0) {
    cli::cli_h2("Localities")
    for (loc in x$localities) {
      if (tolower(loc$codes) == "all") {
        cli::cli_text("  {loc$level} ({loc$level_name}): all")
      } else {
        cli::cli_text("  {loc$level} ({loc$level_name}): {loc$codes}")
      }
    }
  }
  
  # Classifications
  if (length(x$classifications) > 0) {
    cli::cli_h2("Classifications ({length(x$classifications)})")
    for (cls in x$classifications) {
      name_str <- if (!is.na(cls$name)) cls$name else "?"
      cli::cli_text("  {cls$id}: {name_str}")
      for (j in seq_len(nrow(cls$categories))) {
        cat_row <- cls$categories[j, ]
        cat_name <- if (!is.na(cat_row$category_name)) cat_row$category_name else "?"
        cli::cli_text("    {cat_row$category_id}: {cat_name}")
      }
    }
  }
  
  # Equivalent call
  cli::cli_h2("Equivalent ibger call")
  cli::cli_code(x$ibger_call)
  
  invisible(x)
}



#' Fetch data from a SIDRA API URL
#'
#' Parses a SIDRA API URL and fetches the data using [ibge_variables()],
#' returning the same tidy tibble format.
#'
#' @param url Character string. A SIDRA API URL, typically starting with
#'   `https://apisidra.ibge.gov.br/values/`.
#' @param validate Logical. If `TRUE` (default), validates parameters against
#'   aggregate metadata before querying.
#'
#' @return A [tibble][tibble::tibble] in tidy (long) format, same as
#'   [ibge_variables()].
#'
#' @examples
#' \dontrun{
#' url <- "https://apisidra.ibge.gov.br/values/t/7060/n1/all/v/63/p/last%2012/c315/7169"
#' fetch_sidra_url(url)
#'
#' # Pipe-friendly: inspect then fetch
#' url |> parse_sidra_url()
#' url |> fetch_sidra_url()
#' }
#'
#' @export
fetch_sidra_url <- function(url, validate = TRUE) {
  
  parsed <- parse_sidra_url(url)
  
  # Build localities argument
  localities <- if (length(parsed$localities) == 0) {
    "BR"
  } else if (length(parsed$localities) == 1) {
    loc <- parsed$localities[[1]]
    if (tolower(loc$codes) == "all" && loc$level == "N1") {
      "BR"
    } else if (tolower(loc$codes) == "all") {
      loc$level
    } else {
      ids <- as.numeric(strsplit(loc$codes, ",")[[1]])
      stats::setNames(list(ids), loc$level)
    }
  } else {
    loc_list <- purrr::map(parsed$localities, function(loc) {
      if (tolower(loc$codes) == "all") return(NULL)
      as.numeric(strsplit(loc$codes, ",")[[1]])
    })
    names(loc_list) <- purrr::map_chr(parsed$localities, "level")
    # "all" levels become just the level code
    all_levels <- purrr::map_lgl(parsed$localities, ~ tolower(.x$codes) == "all")
    if (any(all_levels)) {
      level_strs <- purrr::map_chr(parsed$localities[all_levels], "level")
      specific <- loc_list[!all_levels]
      paste(c(level_strs, purrr::imap_chr(specific, function(ids, lvl) {
        paste0(lvl, "[", paste(ids, collapse = ","), "]")
      })), collapse = "|")
    } else {
      loc_list
    }
  }
  
  # Build classification argument
  classification <- if (length(parsed$classifications) > 0) {
    cls <- purrr::map(parsed$classifications, function(cls) {
      cats <- cls$categories$category_id
      if (identical(cats, "all")) "all" else as.numeric(cats)
    })
    names(cls) <- purrr::map_chr(parsed$classifications, "id")
    cls
  } else {
    NULL
  }
  
  # Build variable argument
  variable <- if (nrow(parsed$variables) > 0) {
    ids <- parsed$variables$id
    if (identical(ids, "allxp") || identical(ids, "all")) {
      NULL
    } else {
      as.numeric(ids)
    }
  } else {
    NULL
  }
  
  # Build periods argument
  periods <- if (nchar(parsed$periods) > 0) {
    if (grepl("^last\\s+", parsed$periods, ignore.case = TRUE)) {
      -as.integer(sub("^last\\s+", "", parsed$periods, ignore.case = TRUE))
    } else {
      parsed$periods
    }
  } else {
    -6
  }
  
  ibge_variables(
    aggregate      = as.integer(parsed$aggregate$id),
    variable       = variable,
    periods        = periods,
    localities     = localities,
    classification = classification,
    validate       = validate
  )
}