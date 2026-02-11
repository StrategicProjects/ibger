#' Periods for an aggregate
#'
#' Retrieves available periods for an aggregate.
#'
#' @param aggregate Numeric aggregate identifier.
#'
#' @return A [tibble][tibble::tibble] with columns:
#'   `id`, `literal`, `modification`
#'
#' @examples
#' \dontrun{
#' ibge_periods(1705)
#' }
#'
#' @export
ibge_periods <- function(aggregate) {
  data <- ibge_request(
    aggregate, "periodos",
    .label = glue::glue("periods for aggregate {aggregate}")
  )

  result <- purrr::map_dfr(data, function(p) {
    lits <- p[["literals"]]
    lit_str <- if (!is.null(lits) && length(lits) > 0) {
      paste(unlist(lits), collapse = " / ")
    } else {
      NA_character_
    }

    tibble::tibble(
      id           = pluck_chr(p, "id"),
      literal      = lit_str,
      modification = pluck_chr(p, "modificacao")
    )
  })

  n <- nrow(result)
  cli::cli_alert_success("{n} period{?s} found.")
  result
}
