# Shorten recorded mock file paths (CRAN limits tarball paths to 100 bytes).
# Applied by httptest2 when recording and when matching requests to mocks.
function(response) {
  response |>
    httptest2::gsub_response(
      "https://servicodados.ibge.gov.br/api/v3/agregados", "v3",
      fixed = TRUE
    ) |>
    httptest2::gsub_response(
      "https://servicodados.ibge.gov.br/api/v2/metadados", "v2",
      fixed = TRUE
    )
}
