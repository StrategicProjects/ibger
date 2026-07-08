# Shared fake aggregate metadata mirroring the structure built by
# ibge_metadata(), for tests that must not touch the API.
make_fake_meta <- function(id = 9999,
                           name = "Fake aggregate",
                           variables = NULL,
                           classifications = NULL,
                           levels = c("N1", "N3", "N6"),
                           period_start = "2000",
                           period_end = "2020",
                           frequency = "anual") {
  if (is.null(variables)) {
    variables <- tibble::tibble(
      id   = c("112", "214"),
      name = c("Valor da producao", "Quantidade produzida"),
      unit = c("Mil Reais", "Toneladas")
    )
  }

  if (is.null(classifications)) {
    classifications <- tibble::tibble(
      id   = "81",
      name = "Produto das lavouras",
      categories = list(tibble::tibble(
        category_id    = c("0", "2688", "2689"),
        category_name  = c("Total", "Arroz", "Feijao"),
        category_unit  = NA_character_,
        category_level = c("0", "1", "1")
      ))
    )
  }

  meta <- list(
    id                = id,
    name              = name,
    url               = "https://sidra.ibge.gov.br",
    survey            = "Fake survey",
    subject           = "Fake subject",
    periodicity       = list(
      frequency = frequency,
      start     = period_start,
      end       = period_end
    ),
    territorial_level = list(
      administrative = levels,
      special        = character(),
      ibge           = character()
    ),
    variables         = variables,
    classifications   = classifications
  )

  class(meta) <- c("ibge_metadata", "list")
  meta
}

# Seed the session cache with fake metadata so get_cached_metadata()
# resolves without hitting the API. Returns a function that cleans up.
seed_fake_meta <- function(aggregate, meta = make_fake_meta(id = aggregate)) {
  key <- paste0("meta_", aggregate)
  assign(key, meta, envir = .ibger_cache)
  invisible(function() {
    if (exists(key, envir = .ibger_cache)) rm(list = key, envir = .ibger_cache)
  })
}
