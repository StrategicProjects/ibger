# Interactive aggregate explorer

Launches a Shiny app for browsing and filtering the full catalog of IBGE
aggregates. The app displays value boxes with summary counts, a
searchable table with column-level filters, and lets you filter by
survey or search aggregate names.

## Usage

``` r
ibge_explorer(launch.browser = TRUE)
```

## Arguments

- launch.browser:

  Logical or function. If `TRUE`, opens the app in the default browser.
  If `FALSE`, opens in the RStudio Viewer pane (default). You can also
  pass a function such as
  [`shiny::paneViewer()`](https://rdrr.io/pkg/shiny/man/viewer.html).

## Value

This function is called for its side effect (launching the app). Returns
the value of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)
invisibly.

## Details

Clicking a row shows a notification with the aggregate ID and the
corresponding
[`ibge_metadata()`](https://strategicprojects.github.io/ibger/reference/ibge_metadata.md)
call. A CSV download button is also available.

## Examples

``` r
if (FALSE) { # \dontrun{
# Open in RStudio Viewer
ibge_explorer(launch.browser = FALSE)

# Open in browser
ibge_explorer()
} # }
```
