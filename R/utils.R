# R/utils.R — helpers used by all steps
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(ggplot2); library(tibble)
})

ensure_dirs <- function() {
  dirs <- c("data/raw","data/interim","data/clean","reports/fig")
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

# Read CSV from repo path; fall back to your Windows path if needed
read_maternal_csv <- function() {
  p1 <- "data/raw/maternal-mortality_national_zaf.csv"
  if (file.exists(p1)) return(readr::read_csv(p1, show_col_types = FALSE))
  p2 <- "C:/Users/direo/Downloads/Project Datasets/Project Datasets/maternal-mortality_national_zaf.csv"
  if (file.exists(p2)) return(readr::read_csv(p2, show_col_types = FALSE))
  stop("CSV not found at:\n- ", p1, "\n- ", p2)
}

write_csv_safe <- function(x, path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, path, ...)
}

save_plot_safe <- function(plot, path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(filename = path, plot = plot, ...)
}

# z-score that returns a plain numeric vector (not a matrix)
znum <- function(x) {
  x <- as.numeric(x)
  mu <- mean(x, na.rm = TRUE); sdv <- sd(x, na.rm = TRUE)
  if (!is.finite(sdv) || sdv == 0) return(rep(0, length(x)))
  (x - mu) / sdv
}
