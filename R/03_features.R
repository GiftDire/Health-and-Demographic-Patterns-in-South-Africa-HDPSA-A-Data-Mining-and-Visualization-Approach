
suppressPackageStartupMessages({
  library(tidyverse); library(rsample); library(ggplot2)
})
source("R/utils.R"); ensure_dirs()

df_clean <- readr::read_csv("data/interim/clean_long.csv", show_col_types = FALSE)

# Wide table + change metrics (handle missing year pairs)
wide <- df_clean %>%
  select(Indicator, SurveyYear, Value) %>%
  distinct() %>%
  tidyr::pivot_wider(names_from = SurveyYear, values_from = Value, names_prefix = "Y") %>%
  mutate(
    delta      = if_else(!is.na(Y1998) & !is.na(Y2016), Y2016 - Y1998, NA_real_),
    pct_change = if_else(!is.na(Y1998) & !is.na(Y2016) & Y1998 != 0,
                         (Y2016 - Y1998)/Y1998, NA_real_),
    direction  = case_when(
      is.na(delta) ~ NA_character_,
      delta > 0    ~ "increase",
      delta < 0    ~ "decrease",
      TRUE         ~ "no change"
    )
  )
write_csv_safe(wide, "data/interim/indicator_wide.csv")

# Scale selected numerics as plain numeric columns
num_cols <- c("Y1998","Y2016","delta","pct_change")
wide2_scaled <- wide %>%
  mutate(across(all_of(num_cols), ~ znum(.x), .names = "{.col}_z"))

# Split (80/20) by indicator
set.seed(371)
split <- initial_split(wide2_scaled, prop = 0.8)
train <- training(split); test <- testing(split)

# Save final data
write_csv_safe(wide2_scaled, "data/clean/model_ready.csv")
write_csv_safe(train,        "data/clean/train_indicators.csv")
write_csv_safe(test,         "data/clean/test_indicators.csv")

p1 <- ggplot(df_clean, aes(x = SurveyYear, y = Value, group = Indicator)) +
  geom_line() + geom_point() +
  labs(title = "Maternal indicators by year (national)", x = "Year", y = "Value")
save_plot_safe(p1, "reports/fig/indicator_by_year.png", width = 8, height = 4, dpi = 120)

p2 <- ggplot(wide %>% filter(!is.na(pct_change)),
             aes(x = reorder(Indicator, pct_change), y = pct_change)) +
  geom_col() + coord_flip() +
  labs(title = "% change 1998 → 2016 by indicator", x = "", y = "% change")
save_plot_safe(p2, "reports/fig/pct_change_bar.png", width = 8, height = 5, dpi = 120)

message("03_features.R complete → model_ready.csv, train/test, and figures saved.")
