#' Localities for an aggregate
#'
#' Retrieves available localities for an aggregate at one or more geographic
#' levels. Validates the requested level(s) against the aggregate metadata
#' before querying.
#'
#' @param aggregate Numeric aggregate identifier.
#' @param level Geographic level. Use `"N1"` (Brazil), `"N2"` (region),
#'   `"N3"` (state), `"N6"` (municipality), `"N7"` (metropolitan area), etc.
#'   For multiple levels, use a vector: `c("N6", "N7")`.
#' @param validate Logical. If `TRUE` (default), validates level against
#'   aggregate metadata. Use `FALSE` to skip.
#'
#' @return A [tibble][tibble::tibble] with columns:
#'   `id`, `name`, `level_id`, `level_name`
#'
#' @examples
#' \dontrun{
#' ibge_localities(1437, level = "N1")
#' ibge_localities(1437, level = c("N6", "N7"))
#' }
#'
#' @export
ibge_localities <- function(aggregate, level = "N6", validate = TRUE) {

  if (validate) {
    meta <- get_cached_metadata(aggregate)
    validate_query(meta = meta, level = level)
  }

  level_str <- paste(level, collapse = "|")

  data <- ibge_request(
    aggregate, "localidades", level_str,
    .label = glue::glue("localities for aggregate {aggregate}")
  )

  result <- purrr::map_dfr(data, function(loc) {
    tibble::tibble(
      id         = pluck_chr(loc, "id"),
      name       = pluck_chr(loc, "nome"),
      level_id   = pluck_chr(loc, "nivel", "id"),
      level_name = pluck_chr(loc, "nivel", "nome")
    )
  })

  n <- nrow(result)
  cli::cli_alert_success("{n} localit{?y/ies} found.")
  result
}
