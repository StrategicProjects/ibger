#' List IBGE aggregates
#'
#' Retrieves the set of available aggregates (tables), grouped by survey.
#' Each aggregate corresponds to a SIDRA table. Results are cached in memory
#' per unique combination of parameters, so repeated calls with the same
#' filters are instant.
#'
#' @param period Period of interest, as a periodicity code followed by one
#'   or more period ids in brackets: `"P5[202001]"` (January 2020, monthly
#'   aggregates), `"P1[2019,2020]"` (2019 and 2020, annual aggregates).
#' @param subject Numeric subject code (e.g. `70` for animal slaughter).
#'   Use [ibge_subjects()] to look up codes.
#' @param classification Numeric classification code (e.g. `12026`).
#' @param periodicity Periodicity code (see Details): `"P1"` (annual),
#'   `"P5"` (monthly), `"P8"` (semi-annual), `"P9"` (quarterly), `"P13"`
#'   (rolling quarter), etc.
#' @param level Geographic level: `"N1"` (Brazil), `"N2"` (region), `"N3"`
#'   (state), `"N6"` (municipality), etc.
#'
#' @details
#' All filters are optional. Their format is checked before the request:
#' the IBGE API silently ignores filters it cannot parse (returning the
#' whole catalog) or answers HTTP 500, so malformed values are rejected
#' here with an informative error. A well-formed filter that matches no
#' aggregate returns an empty tibble with a warning.
#'
#' Periodicity codes used by the API (as observed in the catalog):
#'
#' | Code  | Periodicity                        |
#' |-------|------------------------------------|
#' | `P1`  | Annual                             |
#' | `P5`  | Monthly                            |
#' | `P7`  | Every three years                  |
#' | `P8`  | Semi-annual                        |
#' | `P9`  | Quarterly                          |
#' | `P11` | Every two years                    |
#' | `P13` | Rolling quarter (PNAD Contínua)    |
#' | `P16` | Every six years                    |
#'
#' @return A [tibble][tibble::tibble] with columns:
#'   `survey_id`, `survey_name`, `aggregate_id`, `aggregate_name`
#'
#' @examplesIf interactive()
#' ibge_aggregates()
#' ibge_aggregates(periodicity = "P5")
#' ibge_aggregates(level = "N6")
#' ibge_aggregates(subject = 70)
#' ibge_aggregates(period = "P5[202001]")
#'
#' @export
ibge_aggregates <- function(period = NULL,
                            subject = NULL,
                            classification = NULL,
                            periodicity = NULL,
                            level = NULL) {

  validate_aggregates_filters(
    period = period, subject = subject, classification = classification,
    periodicity = periodicity, level = level
  )

  # Build a cache key from the parameter combination
  params <- list(
    periodo       = period,
    assunto       = subject,
    classificacao = classification,
    periodicidade = periodicity,
    nivel         = level
  )
  cache_key <- paste0("aggregates_", rlang::hash(params))

  # Return cached result if available
  if (exists(cache_key, envir = .ibger_cache)) {
    result <- get(cache_key, envir = .ibger_cache)
    n <- nrow(result)
    cli::cli_alert_success("{n} aggregate{?s} found (cached).")
    return(result)
  }

  query <- purrr::compact(params)

  data <- ibge_request(query = query, .label = "aggregates")

  result <- parse_aggregates(data)

  # Store in cache
  assign(cache_key, result, envir = .ibger_cache)

  n <- nrow(result)
  if (n == 0) {
    cli::cli_alert_warning("No aggregates found for the given filters.")
  } else {
    cli::cli_alert_success("{n} aggregate{?s} found.")
  }
  result
}

#' Parse the catalog JSON (surveys with nested aggregates) into a tibble
#' @noRd
parse_aggregates <- function(data) {
  empty <- tibble::tibble(
    survey_id      = character(),
    survey_name    = character(),
    aggregate_id   = character(),
    aggregate_name = character()
  )

  rows <- purrr::map(data, function(survey) {
    aggregates <- survey[["agregados"]]
    if (is.null(aggregates) || length(aggregates) == 0) return(NULL)

    purrr::map_dfr(aggregates, function(ag) {
      tibble::tibble(
        survey_id      = pluck_chr(survey, "id"),
        survey_name    = pluck_chr(survey, "nome"),
        aggregate_id   = pluck_chr(ag, "id"),
        aggregate_name = pluck_chr(ag, "nome")
      )
    })
  })

  dplyr::bind_rows(empty, rows)
}

#' Check the format of the `ibge_aggregates()` filters
#'
#' The API does not validate its query parameters: an unparseable filter is
#' dropped (so the whole catalog comes back) and some values trigger HTTP
#' 500. Each filter is checked against the syntax the API expects.
#' @noRd
validate_aggregates_filters <- function(period, subject, classification,
                                        periodicity, level) {
  check_filter(subject, "subject", "^[0-9]+$", "70")
  check_filter(classification, "classification", "^[0-9]+$", "12026")
  check_filter(periodicity, "periodicity", "^P[0-9]+$", "P5")
  check_filter(level, "level", "^N[0-9]+$", "N3")
  check_filter(period, "period", "^P[0-9]+\\[[0-9]+(,[0-9]+)*\\]$",
               "P5[202001]")
  invisible(TRUE)
}

#' Abort unless `value` is NULL or a single string matching `pattern`
#' @noRd
check_filter <- function(value, arg, pattern, example) {
  if (is.null(value)) return(invisible(TRUE))

  ok <- length(value) == 1 && !is.na(value) &&
    (is.character(value) || is.numeric(value)) &&
    grepl(pattern, as.character(value))

  if (!ok) {
    cli::cli_abort(c(
      "Invalid {.arg {arg}} filter: {.val {value}}.",
      "i" = "Expected a single value like {.val {example}}.",
      "i" = "See {.code ?ibge_aggregates} for the accepted formats."
    ), call = NULL)
  }

  invisible(TRUE)
}
