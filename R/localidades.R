#' Localities for an aggregate
#'
#' Retrieves available localities for an aggregate at one or more geographic
#' levels. Validates the requested level(s) against the aggregate metadata
#' before querying.
#'
#' @inheritParams ibge_metadata
#' @param level Geographic level. Use `"N1"` (Brazil), `"N2"` (region),
#'   `"N3"` (state), `"N6"` (municipality), `"N7"` (metropolitan area), etc.
#'   For multiple levels, use a vector: `c("N6", "N7")`.
#' @param validate Logical. If `TRUE` (default), validates level against
#'   aggregate metadata. Use `FALSE` to skip.
#'
#' @return A [tibble][tibble::tibble] with columns:
#'   `id`, `name`, `level_id`, `level_name`
#'
#' @details
#' When several levels are requested, one request is made per level and the
#' results are combined. (The API's own multi-level endpoint returns an empty
#' list when any of the requested levels has no localities for the
#' aggregate, which would silently hide the other levels.)
#'
#' @examplesIf interactive()
#' ibge_localities(1437, level = "N1")
#' ibge_localities(1437, level = c("N2", "N3"))
#'
#' @export
ibge_localities <- function(aggregate, level = "N6", validate = TRUE) {

  if (validate) {
    meta <- get_cached_metadata(aggregate)
    validate_query(meta = meta, level = level)
  }

  result <- purrr::map(level, function(lvl) {
    data <- ibge_request(
      aggregate, "localidades", lvl,
      .label = glue::glue("{lvl} localities for aggregate {aggregate}")
    )
    parse_localities(data)
  })
  result <- dplyr::bind_rows(empty_localities(), result)

  n <- nrow(result)
  if (n == 0) {
    cli::cli_alert_warning(
      "No localities found for aggregate {aggregate} at level {.val {level}}."
    )
  } else {
    cli::cli_alert_success("{n} localit{?y/ies} found.")
  }
  result
}

#' Zero-row tibble with the columns promised by `ibge_localities()`
#' @noRd
empty_localities <- function() {
  tibble::tibble(
    id         = character(),
    name       = character(),
    level_id   = character(),
    level_name = character()
  )
}

#' Parse the JSON list of a `localidades` response into a tibble
#' @noRd
parse_localities <- function(data) {
  purrr::map_dfr(data, function(loc) {
    tibble::tibble(
      id         = pluck_chr(loc, "id"),
      name       = pluck_chr(loc, "nome"),
      level_id   = pluck_chr(loc, "nivel", "id"),
      level_name = pluck_chr(loc, "nivel", "nome")
    )
  })
}
