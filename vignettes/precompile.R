# Precompile the network-dependent vignettes.
#
# The ipca-example and tutorial vignettes query the live IBGE API and produce
# figures, so they cannot be built on CRAN or CI. Following the rOpenSci
# pattern (https://ropensci.org/blog/2019/12/08/precompute-vignettes/), the
# executable sources live in *.Rmd.orig; this script knits them locally into
# the *.Rmd files that ship with the package, with output and figures baked
# in. Run it from the package root whenever a .Rmd.orig changes, then commit
# the regenerated .Rmd and tutorial-*.png / ipca-example-*.png figures.

local({
  old <- setwd("vignettes")
  on.exit(setwd(old), add = TRUE)
  knitr::knit("ipca-example.Rmd.orig", output = "ipca-example.Rmd")
  knitr::knit("tutorial.Rmd.orig", output = "tutorial.Rmd")
})
