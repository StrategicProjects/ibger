# Run this script in R to build the pkgdown site
# Make sure you have pkgdown installed:
#   install.packages("pkgdown")

# Set working directory to the package root
# setwd("path/to/ibger")

# Install package dependencies (if needed)
# install.packages(c("httr2", "cli", "tibble", "purrr", "dplyr", "tidyr", "rlang", "glue"))

# Build the site
pkgdown::build_site()

# The site will be generated in the docs/ folder (gitignored).
# Open docs/index.html in your browser to preview.

# Deployment is automated: the .github/workflows/pkgdown.yaml GitHub Action
# rebuilds the site on every push to main and publishes it to the gh-pages
# branch, which GitHub Pages serves at https://strategicprojects.github.io/ibger/.
# You normally don't need to run this script except for a local preview.
