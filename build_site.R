# Run this script in R to build the pkgdown site
# Make sure you have pkgdown installed:
#   install.packages("pkgdown")

# Set working directory to the package root
# setwd("path/to/ibger")

# Install package dependencies (if needed)
# install.packages(c("httr2", "cli", "tibble", "purrr", "dplyr", "tidyr", "rlang", "glue"))

# Build the site
pkgdown::build_site()

# The site will be generated in the docs/ folder.
# Open docs/index.html in your browser to preview.

# To deploy to GitHub Pages, push the docs/ folder or configure
# GitHub Actions. See: https://pkgdown.r-lib.org/articles/pkgdown.html
