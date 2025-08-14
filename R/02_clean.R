suppressPackageStartupMessages({
  library(tidyverse)
})
source("R/utils.R"); ensure_dirs()

df_sel <- readr::read_csv("data/interim/selected_subset.csv", show_col_types = FALSE)

# Before NA log
missing_before <- tibble(
  variable      = names(df_sel),
  missing_count = colSums(is.na(df_sel))
)
write_csv_safe(missing_before, "data/interim/missing_before.csv")

# Simple & transparent NA policy
df_clean <- df_sel %>%
  group_by(Indicator) %>%
  mutate(Value = if_else(is.na(Value), mean(Value, na.rm = TRUE), Value)) %>%
  ungroup()
outliers <- df_clean %>%
  group_by(Indicator) %>%
  summarise(
    iqr  = IQR(Value, na.rm = TRUE),
    low  = quantile(Value, 0.25, na.rm = TRUE) - 1.5 * iqr,
    high = quantile(Value, 0.75, na.rm = TRUE) + 1.5 * iqr,
    .groups = "drop"
  ) %>%
  left_join(df_clean, by = "Indicator") %>%
  mutate(is_outlier = Value < low | Value > high)

write_csv_safe(outliers, "data/interim/outlier_scan.csv")


missing_after <- tibble(
  variable      = names(df_clean),
  missing_count = colSums(is.na(df_clean))
)
write_csv_safe(missing_after, "data/interim/missing_after.csv")
write_csv_safe(df_clean,      "data/interim/clean_long.csv")