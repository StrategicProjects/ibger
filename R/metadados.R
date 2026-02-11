#' Metadata for an aggregate
#'
#' Retrieves complete metadata for an aggregate: name, survey, subject,
#' periodicity, territorial levels, variables, and classifications with
#' all their categories.
#'
#' @param aggregate Numeric aggregate identifier.
#'
#' @return A list of class `ibge_metadata` with:
#'   - `id`, `name`, `url`, `survey`, `subject`
#'   - `periodicity`: list with frequency, start and end
#'   - `territorial_level`: list with administrative, special and ibge
#'   - `variables`: tibble with id, name, unit
#'   - `classifications`: tibble with id, name and list-column `categories`,
#'      where each element is a tibble with category_id, category_name,
#'      category_unit and category_level
#'
#' @examples
#' \dontrun{
#' meta <- ibge_metadata(7060)
#' meta$variables
#' meta$classifications
#' tidyr::unnest(meta$classifications, categories)
#' }
#'
#' @export
ibge_metadata <- function(aggregate) {
  data <- ibge_request(
    aggregate, "metadados",
    .label = glue::glue("metadata for aggregate {aggregate}")
  )

  if (is.list(data) && is.null(names(data)) && length(data) >= 1) {
    data <- data[[1]]
  }

  # --- Variables ---
  vars_raw <- data[["variaveis"]]
  variables <- if (!is.null(vars_raw) && length(vars_raw) > 0) {
    purrr::map_dfr(vars_raw, function(v) {
      tibble::tibble(
        id   = pluck_chr(v, "id"),
        name = pluck_chr(v, "nome"),
        unit = pluck_chr(v, "unidade")
      )
    })
  } else {
    tibble::tibble(id = character(), name = character(), unit = character())
  }

  # --- Classifications (with ALL categories) ---
  cls_raw <- data[["classificacoes"]]
  classifications <- if (!is.null(cls_raw) && length(cls_raw) > 0) {
    purrr::map_dfr(cls_raw, function(cls) {
      cats_raw <- cls[["categorias"]]
      categories <- if (!is.null(cats_raw) && length(cats_raw) > 0) {
        purrr::map_dfr(cats_raw, function(cat) {
          tibble::tibble(
            category_id    = pluck_chr(cat, "id"),
            category_name  = pluck_chr(cat, "nome"),
            category_unit  = pluck_chr(cat, "unidade"),
            category_level = pluck_chr(cat, "nivel")
          )
        })
      } else {
        tibble::tibble(
          category_id = character(), category_name = character(),
          category_unit = character(), category_level = character()
        )
      }

      tibble::tibble(
        id         = pluck_chr(cls, "id"),
        name       = pluck_chr(cls, "nome"),
        categories = list(categories)
      )
    })
  } else {
    tibble::tibble(id = character(), name = character(), categories = list())
  }

  # --- Territorial level ---
  nt_raw <- data[["nivelTerritorial"]]
  territorial_level <- if (!is.null(nt_raw)) {
    list(
      administrative = unlist(nt_raw[["Administrativo"]]) %||% character(),
      special        = unlist(nt_raw[["Especial"]]) %||% character(),
      ibge           = unlist(nt_raw[["IBGE"]]) %||% character()
    )
  } else {
    list(administrative = character(), special = character(), ibge = character())
  }

  # --- Periodicity ---
  per_raw <- data[["periodicidade"]]
  periodicity <- if (!is.null(per_raw)) {
    list(
      frequency = per_raw[["frequencia"]] %||% NA_character_,
      start     = per_raw[["inicio"]] %||% NA_character_,
      end       = per_raw[["fim"]] %||% NA_character_
    )
  } else {
    list(frequency = NA_character_, start = NA_character_, end = NA_character_)
  }

  result <- list(
    id                = data[["id"]],
    name              = data[["nome"]],
    url               = data[["URL"]],
    survey            = data[["pesquisa"]],
    subject           = data[["assunto"]],
    periodicity       = periodicity,
    territorial_level = territorial_level,
    variables         = variables,
    classifications   = classifications
  )

  class(result) <- c("ibge_metadata", "list")

  n_vars <- nrow(variables)
  n_cls <- nrow(classifications)
  n_cats <- sum(purrr::map_int(classifications$categories, nrow))
  cli::cli_alert_success(
    "Aggregate {aggregate}: {n_vars} variable{?s}, {n_cls} classification{?s}, {n_cats} categor{?y/ies}."
  )

  result
}

#' @export
print.ibge_metadata <- function(x, ...) {
  cli::cli_h1(x$name)
  cli::cli_text("ID: {x$id}")
  cli::cli_text("Survey: {x$survey}")
  cli::cli_text("Subject: {x$subject}")
  cli::cli_text("Periodicity: {x$periodicity$frequency} ({x$periodicity$start} to {x$periodicity$end})")
  cli::cli_text("Territorial levels: {paste(x$territorial_level$administrative, collapse = ', ')}")

  if (nrow(x$variables) > 0) {
    cli::cli_h2("Variables ({nrow(x$variables)})")
    for (i in seq_len(nrow(x$variables))) {
      v <- x$variables[i, ]
      cli::cli_bullets(c(" " = "{v$id}: {v$name} ({v$unit})"))
    }
  }

  if (nrow(x$classifications) > 0) {
    cli::cli_h2("Classifications ({nrow(x$classifications)})")
    for (i in seq_len(nrow(x$classifications))) {
      cls <- x$classifications[i, ]
      n_cats <- nrow(cls$categories[[1]])
      cli::cli_bullets(c(" " = "{cls$id}: {cls$name} ({n_cats} categories)"))

      cats <- cls$categories[[1]]
      n_show <- min(5, n_cats)
      if (n_show > 0) {
        for (j in seq_len(n_show)) {
          cat_row <- cats[j, ]
          lvl_str <- if (!is.na(cat_row$category_level)) {
            paste0(" [level ", cat_row$category_level, "]")
          } else {
            ""
          }
          cli::cli_text("    {cat_row$category_id}: {cat_row$category_name}{lvl_str}")
        }
        if (n_cats > n_show) {
          cli::cli_text("    ... and {n_cats - n_show} more categories")
        }
      }
    }
  }

  cli::cli_text("")
  cli::cli_text("Use {.code meta$variables} and {.code meta$classifications} to access the data.")
  cli::cli_text("Use {.code tidyr::unnest(meta$classifications, categories)} to unnest.")

  invisible(x)
}
