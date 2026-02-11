# Metadata for an aggregate

Retrieves complete metadata for an aggregate: name, survey, subject,
periodicity, territorial levels, variables, and classifications with all
their categories.

## Usage

``` r
ibge_metadata(aggregate)
```

## Arguments

- aggregate:

  Numeric aggregate identifier.

## Value

A list of class `ibge_metadata` with:

- `id`, `name`, `url`, `survey`, `subject`

- `periodicity`: list with frequency, start and end

- `territorial_level`: list with administrative, special and ibge

- `variables`: tibble with id, name, unit

- `classifications`: tibble with id, name and list-column `categories`,
  where each element is a tibble with category_id, category_name,
  category_unit and category_level

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- ibge_metadata(7060)
meta$variables
meta$classifications
tidyr::unnest(meta$classifications, categories)
} # }
```
