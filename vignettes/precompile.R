# Precompile the network-dependent vignettes.
#
# All four vignettes query the live IBGE API (two of them also produce
# figures), so they cannot be built on CRAN or CI. Following the rOpenSci
# pattern (https://ropensci.org/blog/2019/12/08/precompute-vignettes/), the
# executable sources live in *.Rmd.orig; this script knits them locally into
# the *.Rmd files that ship with the package, with output and figures baked
# in. Run it from the package root whenever a .Rmd.orig changes, then commit
# the regenerated .Rmd and tutorial-*.png / ipca-example-*.png figures.
#
# Usage: Rscript vignettes/precompile.R [name ...]
# With no arguments every vignette is rebuilt; otherwise only the named ones
# (e.g. `Rscript vignettes/precompile.R getting-started api-concepts`).

vignettes <- c("getting-started", "api-concepts", "ipca-example", "tutorial")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) vignettes <- intersect(vignettes, args)

withr::with_dir("vignettes", {
  for (v in vignettes) {
    knitr::knit(paste0(v, ".Rmd.orig"), output = paste0(v, ".Rmd"))
  }
})
