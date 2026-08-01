#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data .env := %||%
#' @importFrom curl curl_version
## usethis namespace: end
NULL

# curl is never called directly: httr2 requires curl >= 6.0.0 at runtime
# (curl_modify_url). Declaring it in Imports (with the version constraint)
# and importing one symbol enforces the requirement at install time.
