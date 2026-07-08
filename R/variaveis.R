#' Get variable data from an aggregate
#'
#' Main function of the package. Retrieves variable results from an IBGE
#' aggregate for the specified localities, periods and classifications.
#'
#' Before querying the API, validates all parameters against the aggregate
#' metadata. If any parameter is invalid, stops with a clear error message
#' showing the allowed values.
#'
#' @param aggregate Numeric aggregate identifier (SIDRA table).
#' @param variable Variable(s) to retrieve. Can be:
#'   - `NULL` (default): returns all standard variables
#'   - Numeric vector: specific IDs, e.g. `c(284, 285)`
#'   - `"all"`: includes automatically generated percentage variables
#' @param periods Period(s) to query. Can be:
#'   - Negative integer: last N periods, e.g. `-6` (default)
#'   - Numeric vector: e.g. `c(201701, 201702, 201703)`
#'   - String with range: e.g. `"201701-201712"`
#' @param localities Locality(ies) to query. Can be:
#'   - `"BR"` (default): Brazil
#'   - Level code: `"N3"` (all states), `"N6"` (all municipalities)
#'   - Named list for specific localities:
#'     `list(N3 = c(33, 35))` (RJ and SP states),
#'     `list(N6 = c(3550308, 3304557))` (SP and RJ municipalities)
#' @param classification Classification(s) to filter results. Named list
#'   where names are classification IDs and values are category ID vectors.
#'   Use `"all"` for all categories.
#'   E.g. `list("226" = c(4844, 96608), "218" = 4780)`
#' @param view Display mode: `NULL` (default), `"OLAP"` or `"flat"`.
#' @param validate Logical. If `TRUE` (default), validates parameters against
#'   aggregate metadata before querying. Use `FALSE` to skip.
#' @param chunk Controls automatic splitting of large queries. The IBGE API
#'   rejects requests whose result is too large with an HTTP 500 error (the
#'   documented limit is 100,000 values, but in practice requests fail above
#'   about 50,000). Can be:
#'   - `TRUE` (default): estimates the result size (variables x periods x
#'     localities x categories) and, when it exceeds 50,000 values,
#'     transparently splits the query into multiple smaller requests (by
#'     periods, then by localities) and combines the results.
#'   - `FALSE`: always performs a single request.
#'   - Positive number: same as `TRUE`, but with a custom per-request value
#'     limit.
#'
#' @return A [tibble][tibble::tibble] in tidy (long) format with columns:
#'   `variable_id`, `variable_name`, `variable_unit`,
#'   classification columns (when present),
#'   `locality_id`, `locality_name`, `locality_level`,
#'   `period`, `value`
#'
#' @examples
#' \dontrun{
#' # IPCA in Brazil
#' ibge_variables(7060, localities = "BR")
#'
#' # Specific variables for states
#' ibge_variables(1705, variable = c(284, 285), localities = "N3")
#'
#' # Large query (all municipalities): split automatically into
#' # multiple requests to respect the API's 100,000 value limit
#' ibge_variables(1612, localities = "N6")
#'
#' # Specific municipalities with classification
#' ibge_variables(
#'   aggregate      = 1712,
#'   variable       = 214,
#'   periods        = -3,
#'   localities     = list(N6 = c(3550308, 3304557)),
#'   classification = list("226" = c(4844, 96608))
#' )
#' }
#' 
#'
#' @export
ibge_variables <- function(aggregate,
                           variable = NULL,
                           periods = -6,
                           localities = "BR",
                           classification = NULL,
                           view = NULL,
                           validate = TRUE,
                           chunk = TRUE) {

  meta <- NULL

  if (validate) {
    meta <- get_cached_metadata(aggregate)
    validate_query(
      meta           = meta,
      localities     = localities,
      periods        = periods,
      variable       = variable,
      classification = classification
    )
  }

  variable_str <- format_variable(variable)
  periods_str <- format_periods(periods)
  localities_str <- format_localities(localities)
  classification_str <- format_classification(classification)

  chunk_limit <- resolve_chunk_limit(chunk)

  plan <- NULL
  if (!is.null(chunk_limit)) {
    plan <- build_chunk_plan(
      aggregate      = aggregate,
      meta           = meta,
      variable       = variable,
      periods        = periods,
      localities     = localities,
      classification = classification,
      limit          = chunk_limit
    )
  }

  if (is.null(plan)) {
    query <- list(
      localidades   = localities_str,
      classificacao = classification_str,
      view          = view
    )

    data <- ibge_request(
      aggregate, "periodos", periods_str, "variaveis", variable_str,
      query = query,
      .label = glue::glue("variables for aggregate {aggregate}")
    )

    result <- parse_variables(data, view = view)
  } else {
    n_chunks <- length(plan)
    cli::cli_alert_info(
      "Estimated result exceeds the API limit ({chunk_limit} values); splitting into {n_chunks} request{?s}."
    )

    pieces <- purrr::imap(plan, function(ch, i) {
      data <- ibge_request(
        aggregate, "periodos", ch$periods_str, "variaveis", variable_str,
        query = list(
          localidades   = ch$localities_str,
          classificacao = classification_str,
          view          = view
        ),
        .label = glue::glue("chunk {i}/{n_chunks} for aggregate {aggregate}")
      )
      parse_variables(data, view = view)
    })

    result <- dplyr::bind_rows(pieces)
  }

  n <- nrow(result)
  cli::cli_alert_success("{n} record{?s} retrieved.")
  result
}

#' Detect response format and dispatch to the right parser
#' @noRd
parse_variables <- function(data, view = NULL) {
  if (length(data) == 0) return(tibble::tibble())

  # Flat format: first element has keys NC, NN, V, D1C, D1N, ...
  first <- data[[1]]
  is_flat <- !is.null(first[["NC"]]) && !is.null(first[["V"]])

  if (is_flat) {
    parse_variables_flat(data)
  } else {
    parse_variables_default(data)
  }
}

#' Parse flat view response
#'
#' In flat mode the first element is a header row (column labels)
#' and subsequent elements are data rows.
#'
#' Fixed keys: NC (level code), NN (level name), MC (unit code),
#' MN (unit name), V (value).
#' Dimension keys: D1C/D1N, D2C/D2N, D3C/D3N, ... (code/name pairs).
#'
#' Column names come from the header row values for a user-friendly output.
#'
#' @noRd
parse_variables_flat <- function(data) {
  if (length(data) < 2) return(tibble::tibble())

  header <- data[[1]]
  rows   <- data[-1]
  keys   <- names(header)

  # Build column name mapping from header: key -> friendly label
  col_map <- vapply(keys, function(k) {
    val <- header[[k]]
    if (is.null(val)) k else as.character(val)
  }, character(1))

  purrr::map_dfr(rows, function(row) {
    vals <- vapply(keys, function(k) {
      val <- row[[k]]
      if (is.null(val)) NA_character_ else as.character(val)
    }, character(1))

    names(vals) <- col_map
    tibble::as_tibble_row(vals)
  })
}

#' Parse default (nested) view response into a tidy tibble
#' @noRd
parse_variables_default <- function(data) {
  purrr::map_dfr(data, function(var) {
    var_id   <- pluck_chr(var, "id")
    var_name <- pluck_chr(var, "variavel")
    var_unit <- pluck_chr(var, "unidade")

    results <- var[["resultados"]]
    if (is.null(results) || length(results) == 0) return(NULL)

    purrr::map_dfr(results, function(res) {
      cls_cols <- parse_classifications(res[["classificacoes"]])

      series <- res[["series"]]
      if (is.null(series) || length(series) == 0) return(NULL)

      purrr::map_dfr(series, function(serie) {
        loc <- serie[["localidade"]]

        loc_id    <- pluck_chr(loc, "id")
        loc_name  <- pluck_chr(loc, "nome")
        loc_level <- pluck_chr(loc, "nivel", "nome")

        serie_data <- serie[["serie"]]
        if (is.null(serie_data) || length(serie_data) == 0) return(NULL)

        period_names <- names(serie_data)
        values <- as.character(unlist(serie_data, use.names = FALSE))

        n_per <- length(period_names)

        base_rep <- tibble::tibble(
          variable_id   = rep(var_id, n_per),
          variable_name = rep(var_name, n_per),
          variable_unit = rep(var_unit, n_per)
        )

        cls_rep <- cls_cols[rep(1, n_per), ]

        loc_info <- tibble::tibble(
          locality_id    = rep(loc_id, n_per),
          locality_name  = rep(loc_name, n_per),
          locality_level = rep(loc_level, n_per),
          period         = period_names,
          value          = values
        )

        dplyr::bind_cols(base_rep, cls_rep, loc_info)
      })
    })
  })
}

#' Extract classification columns
#' @noRd
parse_classifications <- function(classifications) {
  if (is.null(classifications) || length(classifications) == 0) {
    return(tibble::tibble(.rows = 1))
  }

  cols <- list()
  for (cls in classifications) {
    cls_id <- pluck_chr(cls, "id")
    col_name <- paste0("classification_", cls_id)

    cat_obj <- cls[["categoria"]]
    if (!is.null(cat_obj) && length(cat_obj) > 0) {
      cat_name <- as.character(cat_obj[[1]])
    } else {
      cat_name <- NA_character_
    }

    cols[[col_name]] <- cat_name
  }

  tibble::as_tibble(cols)
}
