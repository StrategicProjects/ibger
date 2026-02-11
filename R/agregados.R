#' List IBGE aggregates
#'
#' Retrieves the set of available aggregates (tables), grouped by survey.
#' Each aggregate corresponds to a SIDRA table.
#'
#' @param period Period of interest, e.g. `"P5[202001]"` (monthly),
#'   `"P10[20201]"` (quarterly).
#' @param subject Numeric subject code (e.g. `70` for animal slaughter).
#' @param classification Numeric classification code.
#' @param periodicity Periodicity code: `"P5"` (monthly), `"P10"`
#'   (quarterly), `"P13"` (annual), etc.
#' @param level Geographic level: `"N1"` (Brazil), `"N2"` (region), `"N3"`
#'   (state), `"N6"` (municipality), etc.
#'
#' @return A [tibble][tibble::tibble] with columns:
#'   `survey_id`, `survey_name`, `aggregate_id`, `aggregate_name`
#'
#' @examples
#' \dontrun{
#' ibge_aggregates()
#' ibge_aggregates(periodicity = "P5")
#' ibge_aggregates(level = "N6")
#' }
#'
#' @export
ibge_aggregates <- function(period = NULL,
                            subject = NULL,
                            classification = NULL,
                            periodicity = NULL,
                            level = NULL) {
  query <- list(
    periodo = period,
    assunto = subject,
    classificacao = classification,
    periodicidade = periodicity,
    nivel = level
  )

  data <- ibge_request(query = query, .label = "aggregates")

  result <- purrr::map_dfr(data, function(survey) {
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

  n <- nrow(result)
  cli::cli_alert_success("{n} aggregate{?s} found.")
  result
}
