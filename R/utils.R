# API base URL
ibge_base_url <- "https://servicodados.ibge.gov.br/api/v3/agregados"

#' Build and execute a request to the IBGE API
#'
#' @param ... Path segments (concatenated with `/`)
#' @param query Named list of query parameters
#' @param .label Friendly label for cli messages
#' @return Parsed JSON response as a list
#' @noRd
ibge_request <- function(..., query = list(), .label = "data") {

  path_parts <- c(...)
  url <- paste(c(ibge_base_url, path_parts), collapse = "/")

  query <- purrr::compact(query)

  cli::cli_progress_step("Fetching {(.label)} from IBGE API...")

  req <- httr2::request(url) |>
    httr2::req_user_agent("ibger (R package)") |>
    httr2::req_retry(max_tries = 3, backoff = ~ 2)

  for (nm in names(query)) {
    req <- httr2::req_url_query(req, !!nm := query[[nm]])
  }

  req <- httr2::req_error(
    req,
    is_error = function(resp) httr2::resp_status(resp) >= 400,
    body = function(resp) {
      status <- httr2::resp_status(resp)
      if (status == 500) {
        paste0(
          "The API returned error 500. This may indicate the query ",
          "exceeds the 100,000 value limit. ",
          "Try reducing localities, periods or categories."
        )
      } else {
        paste0("HTTP error ", status)
      }
    }
  )

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to access the IBGE API.",
        "x" = conditionMessage(e),
        "i" = "URL: {.url {url}}"
      ), call = NULL)
    }
  )

  result <- httr2::resp_body_json(resp, simplifyVector = FALSE)

  cli::cli_progress_done()

  result
}

#' Safely extract a value from a list
#' @noRd
pluck_chr <- function(x, ..., .default = NA_character_) {
  val <- tryCatch(purrr::pluck(x, ...), error = function(e) NULL)
  if (is.null(val)) .default else as.character(val)
}

#' Format classifications for the query string
#'
#' Converts a named R list to the API format:
#' `226[4844,96608]|218[4780]`
#'
#' @param classification Named list or NULL
#' @return Formatted string or NULL
#' @noRd
format_classification <- function(classification) {
  if (is.null(classification)) return(NULL)

  if (!is.list(classification) || is.null(names(classification))) {
    cli::cli_abort(c(
      "{.arg classification} must be a named list.",
      "i" = 'Example: {.code list("226" = c(4844, 96608), "218" = 4780)}'
    ), call = NULL)
  }

  parts <- purrr::imap_chr(classification, function(cats, cls) {
    if (identical(cats, "all") || identical(cats, "todos")) {
      paste0(cls, "[all]")
    } else {
      paste0(cls, "[", paste(cats, collapse = ","), "]")
    }
  })

  paste(parts, collapse = "|")
}

#' Format localities for the query string
#'
#' Accepts multiple natural forms:
#' - `"BR"` -> Brazil
#' - `"N3"` -> All states
#' - `list(N6 = c(3550308, 3304557))` -> Specific municipalities
#' - `list(N3 = c(33, 35), N6 = 5208707)` -> Multiple levels
#'
#' @param localities String or named list
#' @return Formatted string
#' @noRd
format_localities <- function(localities) {
  if (is.character(localities) && length(localities) == 1) {
    return(localities)
  }

  if (is.character(localities) && length(localities) > 1) {
    return(paste(localities, collapse = "|"))
  }

  if (!is.list(localities) || is.null(names(localities))) {
    cli::cli_abort(c(
      "{.arg localities} must be {.val BR}, a level code (e.g. {.val N3}) or a named list.",
      "i" = 'Example: {.code list(N6 = c(3550308, 3304557))}'
    ), call = NULL)
  }

  parts <- purrr::imap_chr(localities, function(ids, level) {
    paste0(level, "[", paste(ids, collapse = ","), "]")
  })

  paste(parts, collapse = "|")
}

#' Format periods for the URL path
#' @noRd
format_periods <- function(periods) {
  if (is.null(periods)) return("-6")

  if (is.numeric(periods) && length(periods) == 1 && periods < 0) {
    return(as.character(as.integer(periods)))
  }

  paste(periods, collapse = "|")
}

#' Format variables for the URL path
#' @noRd
format_variable <- function(variable) {
  if (is.null(variable)) return("allxp")
  if (identical(variable, "all") || identical(variable, "todas")) return("all")
  paste(variable, collapse = "|")
}

#' Parse IBGE value column
#'
#' Converts the character `value` column returned by [ibge_variables()] to
#' numeric, handling IBGE special value conventions.
#'
#' According to IBGE's tabular presentation standards, the value column may
#' contain special character codes instead of numbers:
#'
#' | Code  | Meaning                                                    |
#' |-------|------------------------------------------------------------|
#' | `-`   | Numeric value equal to zero (not from rounding)            |
#' | `..`  | Not applicable                                             |
#' | `...` | Data not available                                         |
#' | `X`   | Suppressed to avoid identifying individual respondents     |
#'
#' @param x A character or numeric vector of IBGE values.
#'
#' @return A numeric vector where:
#'   - `"-"` becomes `0`
#'   - `".."`, `"..."` and `"X"` become `NA_real_`
#'   - All other values are converted with [as.numeric()]
#'
#' @examples
#' parse_ibge_value(c("1.5", "10", "-", "..", "...", "X", NA))
#' #> [1] 1.5  10.0  0.0   NA    NA    NA    NA
#'
#' \dontrun{
#' # Typical usage after ibge_variables()
#' library(dplyr)
#'
#' ibge_variables(7060, localities = "BR") |>
#'   mutate(value = parse_ibge_value(value))
#' }
#'
#' @export
parse_ibge_value <- function(x) {
  if (is.numeric(x)) return(x)

  x <- as.character(x)

  dplyr::case_when(
    x == "-"              ~ 0,
    x %in% c("..", "...", "X") ~ NA_real_,
    TRUE                  ~ suppressWarnings(as.numeric(x))
  )
}
