# Metadata API (v2) ---------------------------------------------------------
# Base URL for the IBGE Metadata API (different from the Aggregates API v3)
ibge_metadados_url <- "https://servicodados.ibge.gov.br/api/v2/metadados"

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
#'     \item{id}{Survey code (e.g. `"PMS"`, `"CD"`)}
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
#' has associated metadata records.
#'
#' @param survey Character string. The survey code as returned by
#'   [ibge_surveys()] (e.g. `"AC"` for Pesquisa Anual de Comércio,
#'   `"PMS"` for Pesquisa Mensal de Serviços).
#'
#' @return A [tibble][tibble::tibble] with columns:
#'   `year`, `month` (NA for annual surveys), and `order` (publication
#'   order within the period, where 0 is the most recent).
#'
#' @examples
#' \dontrun{
#' ibge_survey_periods("PMS")
#' ibge_survey_periods("CD")
#' }
#'
#' @seealso [ibge_surveys()], [ibge_survey_metadata()]
#' @export
ibge_survey_periods <- function(survey) {
  
  if (!is.character(survey) || length(survey) != 1 || nchar(survey) == 0) {
    cli::cli_abort(c(
      "{.arg survey} must be a single non-empty string.",
      "i" = "Use {.code ibge_surveys()} to see available survey codes."
    ), call = NULL)
  }
  
  data <- ibge_metadados_request(
    "pesquisas", survey, "periodos",
    .label = glue::glue("periods for survey {survey}")
  )
  
  result <- purrr::map_dfr(data, function(p) {
    month_val <- p[["mes"]]
    month_int <- if (is.null(month_val) || month_val == "") {
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
  
  n <- nrow(result)
  cli::cli_alert_success("{n} period{?s} found for survey {.val {survey}}.")
  result
}


#' Get survey metadata for a specific period
#'
#' Retrieves detailed institutional and methodological metadata for a
#' survey in a given reference period. The returned information includes
#' survey status, category (conjunctural/structural), thematic
#' classifications, SIDRA/CONCLA URLs, and the full set of metadata
#' occurrences for the period.
#'
#' The structure of `occurrences` varies by survey and may include fields
#' such as objective, data collection method, sample design, geographic
#' scope, reference period, and more.
#'
#' @param survey Character string. The survey code (e.g. `"PMS"`, `"CD"`).
#' @param year Integer. Reference year.
#' @param month Integer or `NULL`. Reference month (1--12). Required only
#'   for conjunctural (sub-annual) surveys. Omit for structural (annual+)
#'   surveys.
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
#' # Annual survey (no month needed)
#' ibge_survey_metadata("CD", year = 2022)
#'
#' # Conjunctural survey (month required)
#' ibge_survey_metadata("PMS", year = 2024, month = 6)
#' }
#'
#' @seealso [ibge_surveys()], [ibge_survey_periods()]
#' @export
ibge_survey_metadata <- function(survey, year, month = NULL, order = 0) {
  
  if (!is.character(survey) || length(survey) != 1 || nchar(survey) == 0) {
    cli::cli_abort(c(
      "{.arg survey} must be a single non-empty string.",
      "i" = "Use {.code ibge_surveys()} to see available survey codes."
    ), call = NULL)
  }
  
  if (!is.numeric(year) || length(year) != 1) {
    cli::cli_abort("{.arg year} must be a single integer.", call = NULL)
  }
  
  # Build path segments
  path <- c(survey, as.integer(year))
  
  label <- glue::glue("metadata for {survey} ({year}")
  
  if (!is.null(month)) {
    if (!is.numeric(month) || length(month) != 1 || month < 1 || month > 12) {
      cli::cli_abort(
        "{.arg month} must be an integer between 1 and 12.",
        call = NULL
      )
    }
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
      # Structure varies; extract all available fields
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
    
    # Show a preview of the first occurrence's top-level keys
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