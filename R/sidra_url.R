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
#' @examplesIf interactive()
#' url <- paste0(
#'   "https://apisidra.ibge.gov.br/values",
#'   "/t/5434/n1/all/v/4090/p/last%201/c888/47946,56623"
#' )
#' parse_sidra_url(url)
#'
#' @export
parse_sidra_url <- function(url) {

  parsed <- parse_sidra_path(utils::URLdecode(url))

  if (is.null(parsed$aggregate_id)) {
    cli::cli_abort(c(
      "Could not find an aggregate ID in the URL.",
      "i" = "Expected a SIDRA API URL with {.code /t/{{id}}} in the path."
    ), call = NULL)
  }

  # Fetch metadata to resolve names
  meta <- get_cached_metadata(as.integer(parsed$aggregate_id))

  var_info <- resolve_sidra_variables(meta, parsed$variables)
  cls_info <- resolve_sidra_classifications(meta, parsed$classifications)
  loc_info <- resolve_sidra_localities(parsed$localities)

  ibger_call <- build_ibger_call(
    parsed$aggregate_id, var_info$id, parsed$periods,
    parsed$localities, parsed$classifications
  )

  result <- list(
    aggregate       = list(id = parsed$aggregate_id, name = meta$name),
    variables       = var_info,
    periods         = parsed$periods,
    localities      = loc_info,
    classifications = cls_info,
    ibger_call      = ibger_call,
    periodicity     = meta$periodicity
  )

  class(result) <- c("sidra_query", "list")
  result
}

#' Split a SIDRA URL path into its raw components
#'
#' @return A list with `aggregate_id`, `variables`, `periods`, `localities`
#'   (list of level/codes pairs) and `classifications` (named list of
#'   category ids).
#' @noRd
parse_sidra_path <- function(url) {
  # Extract path after /values/
  path <- sub(".*?/values/", "", url, ignore.case = TRUE)
  path <- sub("\\?.*", "", path)  # remove query string if any
  path <- sub("/$", "", path)     # remove trailing slash

  segments <- strsplit(path, "/", fixed = TRUE)[[1]]

  # Parse segments in pairs
  out <- list(
    aggregate_id    = NULL,
    variables       = character(),
    periods         = character(),
    localities      = list(),
    classifications = list()
  )

  i <- 1
  while (i + 1 <= length(segments)) {
    seg <- segments[i]
    val <- segments[i + 1]

    if (seg == "t") {
      out$aggregate_id <- val
    } else if (grepl("^n\\d+$", seg)) {
      level <- toupper(paste0("N", sub("^n", "", seg)))
      out$localities[[length(out$localities) + 1]] <-
        list(level = level, codes = val)
    } else if (seg == "v") {
      out$variables <- strsplit(val, ",", fixed = TRUE)[[1]]
    } else if (seg == "p") {
      out$periods <- val
    } else if (grepl("^c\\d+$", seg)) {
      cls_id <- sub("^c", "", seg)
      out$classifications[[cls_id]] <- strsplit(val, ",", fixed = TRUE)[[1]]
    } else {
      i <- i + 1
      next
    }

    i <- i + 2
  }

  out
}

#' Resolve variable names for a parsed SIDRA URL
#' @noRd
resolve_sidra_variables <- function(meta, variables) {
  if (length(variables) == 0 || identical(variables, "allxp")) {
    return(meta$variables)
  }

  matched <- meta$variables[meta$variables$id %in% variables, ]
  purrr::map_dfr(variables, function(vid) {
    row <- matched[matched$id == vid, ]
    if (nrow(row) > 0) {
      row
    } else {
      tibble::tibble(id = vid, name = NA_character_, unit = NA_character_)
    }
  })
}

#' Resolve classification and category names for a parsed SIDRA URL
#' @noRd
resolve_sidra_classifications <- function(meta, classifications) {
  purrr::imap(classifications, function(cats, cls_id) {
    cls_row <- meta$classifications[meta$classifications$id == cls_id, ]
    cls_name <- if (nrow(cls_row) > 0) cls_row$name else NA_character_

    if (identical(cats, "all")) {
      cat_detail <- tibble::tibble(
        category_id = "all",
        category_name = "(all categories)"
      )
    } else if (nrow(cls_row) > 0) {
      cat_detail <- resolve_sidra_categories(cls_row$categories[[1]], cats)
    } else {
      cat_detail <- tibble::tibble(
        category_id = cats,
        category_name = NA_character_
      )
    }

    list(id = cls_id, name = cls_name, categories = cat_detail)
  })
}

#' Match requested category ids against the metadata categories
#' @noRd
resolve_sidra_categories <- function(all_cats, cats) {
  purrr::map_dfr(cats, function(cid) {
    row <- all_cats[all_cats$category_id == cid, ]
    if (nrow(row) > 0) {
      tibble::tibble(category_id = cid, category_name = row$category_name)
    } else {
      tibble::tibble(category_id = cid, category_name = NA_character_)
    }
  })
}

# Human-readable names for the IBGE territorial levels
sidra_level_names <- c(
  N1 = "Brazil", N2 = "Major region", N3 = "State (UF)",
  N6 = "Municipality", N7 = "Metropolitan area",
  N8 = "Mesoregion", N9 = "Microregion", N10 = "District",
  N11 = "Sub-district", N13 = "Legal Amazon",
  N14 = "Semiarid", N15 = "Immediate geographic region",
  N17 = "Intermediate geographic region"
)

#' Attach level names to the parsed localities
#' @noRd
resolve_sidra_localities <- function(localities) {
  purrr::map(localities, function(loc) {
    level_name <- sidra_level_names[[loc$level]]
    if (is.null(level_name)) level_name <- loc$level
    list(level = loc$level, level_name = level_name, codes = loc$codes)
  })
}

#' Build equivalent ibge_variables() call as a string
#' @noRd
build_ibger_call <- function(aggregate_id, var_ids, periods, localities,
                             classifications) {
  paste0(
    "ibge_variables(\n  aggregate = ", aggregate_id,
    format_call_variable(var_ids),
    format_call_periods(periods),
    format_call_localities(localities),
    format_call_classifications(classifications),
    "\n)"
  )
}

#' Format the `variable` argument of the equivalent call
#' @noRd
format_call_variable <- function(var_ids) {
  if (length(var_ids) == 0 || identical(var_ids, "allxp")) return("")
  if (length(var_ids) == 1) {
    paste0(",\n  variable = ", var_ids)
  } else {
    paste0(",\n  variable = c(", toString(var_ids), ")")
  }
}

#' Format the `periods` argument of the equivalent call
#' @noRd
format_call_periods <- function(periods) {
  if (length(periods) == 0 || nchar(periods) == 0) return("")
  if (grepl("^last\\s+", periods, ignore.case = TRUE)) {
    n <- sub("^last\\s+", "", periods, ignore.case = TRUE)
    paste0(",\n  periods = -", n)
  } else {
    paste0(',\n  periods = "', periods, '"')
  }
}

#' Format the `localities` argument of the equivalent call
#' @noRd
format_call_localities <- function(localities) {
  if (length(localities) == 0) return("")

  loc_parts <- purrr::map_chr(localities, function(loc) {
    if (tolower(loc$codes) == "all" && loc$level == "N1") return('"BR"')
    if (tolower(loc$codes) == "all") return(paste0('"', loc$level, '"'))
    paste0(loc$level, " = c(", loc$codes, ")")
  })

  if (length(loc_parts) == 1 && grepl('^"', loc_parts)) {
    paste0(",\n  localities = ", loc_parts)
  } else {
    paste0(",\n  localities = list(", toString(loc_parts), ")")
  }
}

#' Format the `classification` argument of the equivalent call
#' @noRd
format_call_classifications <- function(classifications) {
  if (length(classifications) == 0) return("")

  cls_parts <- purrr::imap_chr(classifications, function(cats, cls_id) {
    if (identical(cats, "all")) {
      paste0('"', cls_id, '" = "all"')
    } else if (length(cats) == 1) {
      paste0('"', cls_id, '" = ', cats)
    } else {
      paste0('"', cls_id, '" = c(', toString(cats), ")")
    }
  })

  paste0(",\n  classification = list(", toString(cls_parts), ")")
}


#' @export
print.sidra_query <- function(x, ...) {

  cli::cli_h1("SIDRA Query")

  cli::cli_h2("Aggregate")
  cli::cli_text("{.strong {x$aggregate$id}}: {x$aggregate$name}")

  print_sidra_variables(x)
  print_sidra_periods(x)
  print_sidra_localities(x)
  print_sidra_classifications(x)

  cli::cli_h2("Equivalent ibger call")
  cli::cli_code(x$ibger_call)

  invisible(x)
}

#' @noRd
print_sidra_variables <- function(x) {
  if (nrow(x$variables) == 0) return(invisible())

  cli::cli_h2("Variables ({nrow(x$variables)})")
  for (i in seq_len(nrow(x$variables))) {
    v <- x$variables[i, ]
    unit_str <- if (!is.na(v$unit)) paste0(" (", v$unit, ")") else ""
    name_str <- if (!is.na(v$name)) v$name else "?"
    cli::cli_text("  {v$id}: {name_str}{unit_str}")
  }
}

#' @noRd
print_sidra_periods <- function(x) {
  cli::cli_h2("Periods")
  if (nchar(x$periods) == 0) return(invisible())

  if (grepl("^last\\s+", x$periods, ignore.case = TRUE)) {
    n <- sub("^last\\s+", "", x$periods, ignore.case = TRUE)
    cli::cli_text("  Last {n} period{?s} ({x$periodicity$frequency})")
  } else {
    cli::cli_text("  {x$periods}")
  }
}

#' @noRd
print_sidra_localities <- function(x) {
  if (length(x$localities) == 0) return(invisible())

  cli::cli_h2("Localities")
  for (loc in x$localities) {
    if (tolower(loc$codes) == "all") {
      cli::cli_text("  {loc$level} ({loc$level_name}): all")
    } else {
      cli::cli_text("  {loc$level} ({loc$level_name}): {loc$codes}")
    }
  }
}

#' @noRd
print_sidra_classifications <- function(x) {
  if (length(x$classifications) == 0) return(invisible())

  cli::cli_h2("Classifications ({length(x$classifications)})")
  for (cls in x$classifications) {
    name_str <- if (!is.na(cls$name)) cls$name else "?"
    cli::cli_text("  {cls$id}: {name_str}")
    for (j in seq_len(nrow(cls$categories))) {
      cat_row <- cls$categories[j, ]
      cat_name <- cat_row$category_name
      if (is.na(cat_name)) cat_name <- "?"
      cli::cli_text("    {cat_row$category_id}: {cat_name}")
    }
  }
}


#' Fetch data from a SIDRA API URL
#'
#' Parses a SIDRA API URL and fetches the data using [ibge_variables()],
#' returning the same tidy tibble format.
#'
#' @inheritParams parse_sidra_url
#' @param validate Logical. If `TRUE` (default), validates parameters against
#'   aggregate metadata before querying.
#'
#' @return A [tibble][tibble::tibble] in tidy (long) format, same as
#'   [ibge_variables()].
#'
#' @examplesIf interactive()
#' url <- paste0(
#'   "https://apisidra.ibge.gov.br/values",
#'   "/t/7060/n1/all/v/63/p/last%2012/c315/7169"
#' )
#' fetch_sidra_url(url)
#'
#' # Pipe-friendly: inspect then fetch
#' url |> parse_sidra_url()
#' url |> fetch_sidra_url()
#'
#' @export
fetch_sidra_url <- function(url, validate = TRUE) {

  parsed <- parse_sidra_url(url)

  ibge_variables(
    aggregate      = as.integer(parsed$aggregate$id),
    variable       = sidra_variable_arg(parsed$variables),
    periods        = sidra_periods_arg(parsed$periods),
    localities     = sidra_localities_arg(parsed$localities),
    classification = sidra_classification_arg(parsed$classifications),
    validate       = validate
  )
}

#' Translate parsed localities into an ibge_variables() argument
#' @noRd
sidra_localities_arg <- function(localities) {
  if (length(localities) == 0) return("BR")

  if (length(localities) == 1) {
    return(sidra_single_locality_arg(localities[[1]]))
  }

  loc_list <- purrr::map(localities, function(loc) {
    if (tolower(loc$codes) == "all") return(NULL)
    as.numeric(strsplit(loc$codes, ",", fixed = TRUE)[[1]])
  })
  names(loc_list) <- purrr::map_chr(localities, "level")

  # "all" levels become just the level code
  all_levels <- purrr::map_lgl(localities, ~ tolower(.x$codes) == "all")
  if (!any(all_levels)) return(loc_list)

  level_strs <- purrr::map_chr(localities[all_levels], "level")
  specific <- loc_list[!all_levels]
  paste(c(level_strs, purrr::imap_chr(specific, function(ids, lvl) {
    paste0(lvl, "[", paste(ids, collapse = ","), "]")
  })), collapse = "|")
}

#' Translate a single parsed locality into an ibge_variables() argument
#' @noRd
sidra_single_locality_arg <- function(loc) {
  if (tolower(loc$codes) == "all" && loc$level == "N1") {
    "BR"
  } else if (tolower(loc$codes) == "all") {
    loc$level
  } else {
    ids <- as.numeric(strsplit(loc$codes, ",", fixed = TRUE)[[1]])
    stats::setNames(list(ids), loc$level)
  }
}

#' Translate parsed classifications into an ibge_variables() argument
#' @noRd
sidra_classification_arg <- function(classifications) {
  if (length(classifications) == 0) return(NULL)

  cls <- purrr::map(classifications, function(cls) {
    cats <- cls$categories$category_id
    if (identical(cats, "all")) "all" else as.numeric(cats)
  })
  names(cls) <- purrr::map_chr(classifications, "id")
  cls
}

#' Translate parsed variables into an ibge_variables() argument
#' @noRd
sidra_variable_arg <- function(variables) {
  if (nrow(variables) == 0) return(NULL)

  ids <- variables$id
  if (identical(ids, "allxp") || identical(ids, "all")) {
    NULL
  } else {
    as.numeric(ids)
  }
}

#' Translate parsed periods into an ibge_variables() argument
#' @noRd
sidra_periods_arg <- function(periods) {
  if (nchar(periods) == 0) return(-6)

  if (grepl("^last\\s+", periods, ignore.case = TRUE)) {
    -as.integer(sub("^last\\s+", "", periods, ignore.case = TRUE))
  } else {
    periods
  }
}
