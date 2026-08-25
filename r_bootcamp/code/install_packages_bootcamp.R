# Installs the R packages needed to knit r_bootcamp_d1_26.qmd and r_bootcamp_d2_26.qmd
# Run this once before the bootcamp sessions.

packages <- c(
  "tidyverse", # dplyr, ggplot2, readr, tidyr, purrr, stringr, forcats, tibble
  "knitr",
  "haven",
  "psych",
  "skimr",
  "scales",
  "broom",
  "pander"
)

to_install <- setdiff(packages, rownames(installed.packages()))

if (length(to_install) > 0) {
  install.packages(to_install)
} else {
  message("All required packages are already installed.")
}
