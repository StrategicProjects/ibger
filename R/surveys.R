# Metadata API (v2) ---------------------------------------------------------
# Base URL for the IBGE Metadata API (different from the Aggregates API v3)
ibge_metadados_url <- "https://servicodados.ibge.gov.br/api/v2/metadados"

# In-memory survey catalog cache (per session)
.ibger_survey_cache <- new.env(parent = emptyenv())

#' Build and execute a request to the IBGE Metadata API (v2)
#'
#' This is a separate request helper because the Metadata API lives at
#' a different base URL (`/api/v2/metadados`) than the Aggregates API
#' (`/api/v3/agregados`).
#'
#' @param ... Path segments appended to the base URL.
#' @param .label Friendly label for cli progress messages.
#' @return Parsed JSON response as a list.
#' @noRd
ibge_metadados_request <- function(..., .label = "data") {
  
  path_parts <- c(...)
  url <- paste(c(ibge_metadados_url, path_parts), collapse = "/")
  
  cli::cli_progress_step("Fetching {(.label)} from IBGE Metadata API...")
  
  req <- httr2::request(url) |>
    httr2::req_user_agent("ibger (R package)") |>
    httr2::req_retry(max_tries = 3, backoff = ~ 2)
  
  req <- httr2::req_error(
    req,
    is_error = function(resp) httr2::resp_status(resp) >= 400,
    body = function(resp) paste0("HTTP error ", httr2::resp_status(resp))
  )
  
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to access the IBGE Metadata API.",
        "x" = conditionMessage(e),
        "i" = "URL: {.url {url}}"
      ), call = NULL)
    }
  )
  
  result <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  
  cli::cli_progress_done()
  
  result
}


# --- Survey catalog cache --------------------------------------------------

#' Get the survey catalog (cached)
#'
#' Fetches the full list of valid survey codes from the Metadata API once
#' per session and caches the result.
#'
#' @return A named list where names are survey codes and values are survey
#'   names.
#' @noRd
get_cached_survey_catalog <- function() {
  if (exists("catalog", envir = .ibger_survey_cache)) {
    return(get("catalog", envir = .ibger_survey_cache))
  }
  
  data <- ibge_metadados_request("pesquisas", .label = "survey catalog")
  
  catalog <- stats::setNames(
    purrr::map_chr(data, function(s) s[["nome"]] %||% NA_character_),
    purrr::map_chr(data, function(s) s[["codigo"]] %||% "")
  )
  
  assign("catalog", catalog, envir = .ibger_survey_cache)
  catalog
}

#' Get survey periods (cached)
#'
#' Fetches the available periods for a survey and caches the result.
#'
#' @param survey Valid survey code (already validated).
#' @return A tibble with columns `year`, `month`, `order`.
#' @noRd
get_cached_survey_periods <- function(survey) {
  key <- paste0("periods_", survey)
  
  if (exists(key, envir = .ibger_survey_cache)) {
    return(get(key, envir = .ibger_survey_cache))
  }
  
  data <- ibge_metadados_request(
    "pesquisas", survey, "periodos",
    .label = glue::glue("periods for survey {survey}")
  )
  
  result <- purrr::map_dfr(data, function(p) {
    month_val <- p[["mes"]]
    # The API returns mes = "0" or mes = "" for structural (annual) surveys,
    # meaning "no month". Treat all of these as NA.
    month_int <- if (is.null(month_val) || month_val == "" || month_val == "0") {
      NA_integer_
    } else {
      suppressWarnings(as.integer(month_val))
    }
    
    tibble::tibble(
      year  = as.integer(pluck_chr(p, "ano")),
      month = month_int,
      order = as.integer(pluck_chr(p, "ordem"))
    )
  })
  
  assign(key, result, envir = .ibger_survey_cache)
  result
}


# --- Validators ------------------------------------------------------------

#' Validate that a survey code exists in the IBGE catalog
#'
#' Fetches the survey catalog (cached) and checks if the given code is
#' present. If not, suggests the closest matches using string distance.
#'
#' @param survey Character string to validate.
#' @return Invisible `TRUE` if valid.
#' @noRd
validate_survey_code <- function(survey) {
  if (!is.character(survey) || length(survey) != 1 || nchar(survey) == 0) {
    cli::cli_abort(c(
      "{.arg survey} must be a single non-empty string.",
      "i" = "Use {.code ibge_surveys()} to see available survey codes."
    ), call = NULL)
  }
  
  catalog <- get_cached_survey_catalog()
  valid_codes <- names(catalog)
  
  if (survey %in% valid_codes) {
    return(invisible(TRUE))
  }
  
  # Suggest closest matches by string distance
  distances <- utils::adist(toupper(survey), toupper(valid_codes))[1, ]
  closest_idx <- order(distances)[seq_len(min(5, length(distances)))]
  suggestions <- paste0(
    valid_codes[closest_idx], " - ", catalog[closest_idx]
  )
  
  cli::cli_abort(c(
    "Survey code {.val {survey}} not found in the IBGE catalog.",
    "i" = "Did you mean one of these?",
    stats::setNames(paste(" ", suggestions), rep("*", length(suggestions))),
    "i" = "Use {.code ibge_surveys()} to see all {length(valid_codes)} valid codes."
  ), call = NULL)
}

#' Validate that a year/month combination exists for a survey
#'
#' Fetches the available periods for the survey (cached) and checks if the
#' requested year and month are present.
#'
#' @param survey Valid survey code (already validated).
#' @param year Integer year.
#' @param month Integer month or `NULL`.
#' @return Invisible `TRUE` if valid.
#' @noRd
validate_survey_period <- function(survey, year, month = NULL) {
  periods <- get_cached_survey_periods(survey)
  
  if (nrow(periods) == 0) {
    cli::cli_abort(c(
      "No metadata periods found for survey {.val {survey}}.",
      "i" = "This survey may not have period-specific metadata."
    ), call = NULL)
  }
  
  available_years <- sort(unique(periods$year))
  
  if (!as.integer(year) %in% available_years) {
    yr_range <- range(available_years)
    cli::cli_abort(c(
      "Year {.val {year}} not available for survey {.val {survey}}.",
      "i" = "Available years: {yr_range[1]} to {yr_range[2]} ({length(available_years)} total).",
      "i" = "Use {.code ibge_survey_periods(\"{survey}\")} to see all periods."
    ), call = NULL)
  }
  
  # If month was provided, check it exists for that year
  if (!is.null(month)) {
    year_rows <- periods[periods$year == as.integer(year), ]
    available_months <- sort(unique(stats::na.omit(year_rows$month)))
    
    if (length(available_months) == 0) {
      # Survey has no months (structural) — month should not be provided
      cli::cli_abort(c(
        "Survey {.val {survey}} does not use monthly periods (it is structural/annual).",
        "i" = "Remove the {.arg month} argument: {.code ibge_survey_metadata(\"{survey}\", {year})}"
      ), call = NULL)
    }
    
    if (!as.integer(month) %in% available_months) {
      cli::cli_abort(c(
        "Month {.val {month}} not available for survey {.val {survey}} in {.val {year}}.",
        "i" = "Available months for {year}: {.val {available_months}}.",
        "i" = "Use {.code ibge_survey_periods(\"{survey}\")} to see all periods."
      ), call = NULL)
    }
  } else {
    # Month not provided — warn if the survey has monthly data
    year_rows <- periods[periods$year == as.integer(year), ]
    has_months <- any(!is.na(year_rows$month))
    
    if (has_months) {
      available_months <- sort(unique(stats::na.omit(year_rows$month)))
      cli::cli_warn(c(
        "Survey {.val {survey}} has monthly periods for {.val {year}}.",
        "i" = "Available months: {.val {available_months}}.",
        "i" = "Consider specifying {.arg month} for more precise results."
      ))
    }
  }
  
  invisible(TRUE)
}


# --- Public functions -------------------------------------------------------

#' List IBGE surveys with metadata
#'
#' Retrieves the full catalog of IBGE surveys (statistical operations) that
#' have associated metadata in the IBGE Metadata API, including status,
#' category, collection and publication frequency, and thematic classifications.
#'
#' This function queries a **different API** from the other `ibge_*` functions.
#' While `ibge_aggregates()` and `ibge_metadata()` use the Aggregates API (v3),
#' this function uses the Metadata API (v2), which provides institutional and
#' methodological documentation about surveys.
#'
#' @param thematic_classifications Logical. If `TRUE` (default), includes a
#'   list-column `thematic_classifications` with the classification details
#'   for each survey. Set to `FALSE` for a simpler flat tibble.
#'
#' @return A [tibble][tibble::tibble] with columns:
#'   \describe{
#'     \item{id}{Survey code (e.g. `"SC"`, `"CD"`)}
#'     \item{name}{Survey name in Portuguese}
#'     \item{name_en}{Survey name in English (may be `NA`)}
#'     \item{status}{Survey status: Ativa, Encerrada, etc.}
#'     \item{category}{Conjuntural, Estrutural, or Especial}
#'     \item{collection_frequency}{Data collection frequency (Mensal, Anual, etc.)}
#'     \item{publication_frequency}{Publication frequency}
#'     \item{thematic_classifications}{List-column of tibbles with thematic
#'       classification details (name, domain, description). Only if
#'       `thematic_classifications = TRUE`.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Full catalog
#' ibge_surveys()
#'
#' # Flat table without classifications
#' ibge_surveys(thematic_classifications = FALSE)
#'
#' # Filter active conjunctural surveys
#' library(dplyr)
#' ibge_surveys(thematic_classifications = FALSE) |>
#'   filter(status == "Ativa", category == "Conjuntural")
#' }
#'
#' @seealso [ibge_survey_periods()], [ibge_survey_metadata()]
#' @export
ibge_surveys <- function(thematic_classifications = TRUE) {
  
  data <- ibge_metadados_request("pesquisas", .label = "survey catalog")
  
  result <- purrr::map_dfr(data, function(s) {
    row <- tibble::tibble(
      id                      = pluck_chr(s, "codigo"),
      name                    = pluck_chr(s, "nome"),
      name_en                 = pluck_chr(s, "nome_ingles"),
      status                  = pluck_chr(s, "situacao"),
      category                = pluck_chr(s, "categoria"),
      collection_frequency    = pluck_chr(s, "periodicidade_coleta"),
      publication_frequency   = pluck_chr(s, "periodicidade_divulgacao")
    )
    
    if (thematic_classifications) {
      cls_raw <- s[["classificacoes_tematicas"]]
      cls_tbl <- if (!is.null(cls_raw) && length(cls_raw) > 0) {
        purrr::map_dfr(cls_raw, function(cls) {
          tibble::tibble(
            name        = pluck_chr(cls, "nome"),
            name_en     = pluck_chr(cls, "nome_ingles"),
            domain      = pluck_chr(cls, "dominio"),
            domain_en   = pluck_chr(cls, "dominio_ingles"),
            description = pluck_chr(cls, "descricao")
          )
        })
      } else {
        tibble::tibble(
          name = character(), name_en = character(),
          domain = character(), domain_en = character(),
          description = character()
        )
      }
      row$thematic_classifications <- list(cls_tbl)
    }
    
    row
  })
  
  n <- nrow(result)
  cli::cli_alert_success("{n} survey{?s} found.")
  result
}


#' List periods with metadata for a survey
#'
#' Retrieves the periods (year/month combinations) for which a given survey
#' has associated metadata records. Before querying, the survey code is
#' validated against the IBGE catalog; invalid codes produce a helpful error
#' with suggestions.
#'
#' @param survey Character string. The survey code as returned by
#'   [ibge_surveys()] (e.g. `"SC"` for Pesquisa Mensal de Serviços,
#'   `"CD"` for Censo Demográfico). Invalid codes are caught before the
#'   request is sent, with suggestions for similar codes.
#'
#' @return A [tibble][tibble::tibble] with columns:
#'   \describe{
#'     \item{year}{Integer year with metadata available.}
#'     \item{month}{Integer month (1--12) or `NA` for structural
#'       (annual or longer) surveys.}
#'     \item{order}{Publication order within the period (0 = most recent).}
#'   }
#'
#' @examples
#' \dontrun{
#' # Conjunctural survey (monthly periods)
#' ibge_survey_periods("SC")   # Pesquisa Mensal de Serviços
#'
#' # Structural survey (annual periods)
#' ibge_survey_periods("CD")   # Censo Demográfico
#'
#' # Invalid code: helpful error with suggestions
#' ibge_survey_periods("PMS")
#' #> Error: Survey code "PMS" not found in the IBGE catalog.
#' #> i Did you mean one of these?
#' #>   * SC - Pesquisa Mensal de Serviços
#' #>   ...
#' }
#'
#' @seealso [ibge_surveys()], [ibge_survey_metadata()]
#' @export
ibge_survey_periods <- function(survey) {
  
  # Pre-flight: validate survey code against catalog
  validate_survey_code(survey)
  
  result <- get_cached_survey_periods(survey)
  
  n <- nrow(result)
  cli::cli_alert_success("{n} period{?s} found for survey {.val {survey}}.")
  result
}


#' Get survey metadata for a specific period
#'
#' Retrieves detailed institutional and methodological metadata for a
#' survey in a given reference period. Before querying, both the survey
#' code and the year/month combination are validated against the IBGE
#' catalog, so invalid inputs produce clear errors without wasting an
#' API call.
#'
#' The structure of `occurrences` varies by survey and may include fields
#' such as objective, data collection method, sample design, geographic
#' scope, reference period, and more.
#'
#' @param survey Character string. The survey code (e.g. `"SC"`, `"CD"`).
#'   Must match a code from [ibge_surveys()].
#' @param year Integer. Reference year. Must be a year with available
#'   metadata for the survey (see [ibge_survey_periods()]).
#' @param month Integer or `NULL`. Reference month (1--12). Required for
#'   conjunctural (sub-annual) surveys; omit for structural (annual+)
#'   surveys. If omitted for a conjunctural survey, a warning is issued
#'   listing the available months.
#' @param order Integer. Publication order within the period (default `0`,
#'   the most recent).
#'
#' @return A list of class `ibge_survey_metadata` with:
#'   \describe{
#'     \item{status}{Survey status (corrente, concluída, desativada, etc.)}
#'     \item{category}{conjuntural or estrutural}
#'     \item{type}{Type of statistical operation}
#'     \item{area}{Responsible area}
#'     \item{acronym}{Survey acronym}
#'     \item{start_date}{Survey start date}
#'     \item{deactivation_date}{Deactivation date, if applicable}
#'     \item{sidra_url}{URL to SIDRA data}
#'     \item{concla_url}{URL to CONCLA (CNAE classifications)}
#'     \item{thematic_classifications}{Tibble of thematic classifications}
#'     \item{occurrences}{List of metadata records for the period (structure
#'       varies by survey)}
#'   }
#'
#' @examples
#' \dontrun{
#' # Structural survey (no month needed)
#' ibge_survey_metadata("CD", year = 2022)
#'
#' # Conjunctural survey (specify month)
#' ibge_survey_metadata("SC", year = 2023, month = 6)
#'
#' # Inspect methodology fields
#' meta <- ibge_survey_metadata("CD", year = 2022)
#' names(meta$occurrences[[1]])
#'
#' # Invalid code: clear error with suggestions
#' ibge_survey_metadata("PMS", year = 2024)
#' #> Error: Survey code "PMS" not found in the IBGE catalog.
#'
#' # Invalid year: error with available range
#' ibge_survey_metadata("CD", year = 1800)
#' #> Error: Year 1800 not available for survey "CD".
#' #> i Available years: 1940 to 2022 (9 total).
#' }
#'
#' @seealso [ibge_surveys()], [ibge_survey_periods()]
#' @export
ibge_survey_metadata <- function(survey, year, month = NULL, order = 0) {
  
  # --- Input type checks ---
  if (!is.numeric(year) || length(year) != 1) {
    cli::cli_abort("{.arg year} must be a single integer.", call = NULL)
  }
  
  if (!is.null(month)) {
    if (!is.numeric(month) || length(month) != 1 || month < 1 || month > 12) {
      cli::cli_abort(
        "{.arg month} must be an integer between 1 and 12.",
        call = NULL
      )
    }
  }
  
  # --- Pre-flight validation against IBGE catalog ---
  validate_survey_code(survey)
  validate_survey_period(survey, year, month)
  
  # --- Build request path ---
  path <- c(survey, as.integer(year))
  
  label <- glue::glue("metadata for {survey} ({year}")
  
  if (!is.null(month)) {
    path <- c(path, as.integer(month))
    label <- glue::glue("{label}/{month}")
  }
  
  path <- c(path, as.integer(order))
  label <- paste0(label, ")")
  
  data <- ibge_metadados_request(path, .label = label)
  
  # The response is an array; take first element
  if (is.list(data) && is.null(names(data)) && length(data) >= 1) {
    data <- data[[1]]
  }
  
  # --- Thematic classifications ---
  cls_raw <- data[["classificacoes_tematicas"]]
  thematic_cls <- if (!is.null(cls_raw) && length(cls_raw) > 0) {
    purrr::map_dfr(cls_raw, function(cls) {
      as_chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
      tibble::as_tibble(purrr::map(cls, as_chr))
    })
  } else {
    tibble::tibble()
  }
  
  # --- Occurrences (survey-specific metadata) ---
  occ_raw <- data[["ocorrencias_pesquisa"]]
  occurrences <- if (!is.null(occ_raw) && length(occ_raw) > 0) {
    occ_raw
  } else {
    list()
  }
  
  result <- list(
    status                   = data[["situacao"]] %||% NA_character_,
    category                 = data[["categoria"]] %||% NA_character_,
    type                     = data[["tipo"]] %||% NA_character_,
    area                     = data[["area"]] %||% NA_character_,
    acronym                  = data[["sigla"]] %||% NA_character_,
    start_date               = data[["data_inicio"]] %||% NA_character_,
    deactivation_date        = data[["data_desativacao"]] %||% NA_character_,
    sidra_url                = data[["url-sidra"]] %||% NA_character_,
    concla_url               = data[["url-concla"]] %||% NA_character_,
    thematic_classifications = thematic_cls,
    occurrences              = occurrences
  )
  
  class(result) <- c("ibge_survey_metadata", "list")
  
  n_occ <- length(occurrences)
  cli::cli_alert_success(
    "Survey {.val {survey}} ({year}): {n_occ} metadata occurrence{?s}."
  )
  
  result
}

#' @export
print.ibge_survey_metadata <- function(x, ...) {
  
  cli::cli_h1("{x$acronym}")
  
  fields <- list(
    "Status"   = x$status,
    "Category" = x$category,
    "Type"     = x$type,
    "Area"     = x$area,
    "Started"  = x$start_date
  )
  
  for (nm in names(fields)) {
    val <- fields[[nm]]
    if (!is.na(val) && nchar(val) > 0) {
      cli::cli_text("{nm}: {val}")
    }
  }
  
  if (!is.na(x$deactivation_date) && nchar(x$deactivation_date) > 0) {
    cli::cli_text("Deactivated: {x$deactivation_date}")
  }
  
  if (!is.na(x$sidra_url) && nchar(x$sidra_url) > 0) {
    cli::cli_text("SIDRA: {.url {x$sidra_url}}")
  }
  
  if (!is.na(x$concla_url) && nchar(x$concla_url) > 0) {
    cli::cli_text("CONCLA: {.url {x$concla_url}}")
  }
  
  n_cls <- nrow(x$thematic_classifications)
  if (n_cls > 0) {
    cli::cli_h2("Thematic classifications ({n_cls})")
  }
  
  n_occ <- length(x$occurrences)
  if (n_occ > 0) {
    cli::cli_h2("Metadata occurrences ({n_occ})")
    cli::cli_text("Use {.code meta$occurrences} to explore the full metadata.")
    
    first <- x$occurrences[[1]]
    if (is.list(first) && !is.null(names(first))) {
      keys <- names(first)
      n_keys <- length(keys)
      n_show <- min(8, n_keys)
      preview <- keys[seq_len(n_show)]
      cli::cli_text("Fields: {.val {preview}}{if (n_keys > n_show) paste0(' ... and ', n_keys - n_show, ' more') else ''}")
    }
  }
  
  invisible(x)
}