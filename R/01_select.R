# R/01_select.R — selection, profiling, verify quality
suppressPackageStartupMessages({
  library(tidyverse); library(janitor); library(skimr)
})
source("R/utils.R"); ensure_dirs()

raw <- read_maternal_csv()

df0 <- raw %>%
  filter(!(startsWith(as.character(ISO3), "#") |
           startsWith(as.character(Indicator), "#") |
           startsWith(as.character(Value), "#"))) %>%
  mutate(
    Value      = readr::parse_number(Value),
    SurveyYear = suppressWarnings(as.integer(SurveyYear))
  )

# 3) National totals only
df_nat <- df0 %>%
  filter(CharacteristicLabel %in% c("Total 15-49","Total 15–49","Total"))

keep_indicators <- c(
  "Pregnancy-related mortality ratio",
  "Maternal mortality ratio",
  "Lifetime risk of pregnancy-related death",
  "Female deaths that are pregnancy-related"
)
df_sel <- df_nat %>% filter(Indicator %in% keep_indicators) %>% distinct()

# 5) Profiling tables
skimr::skim(df_sel)  # prints to console (nice overview)

missing_tbl <- tibble(
  variable      = names(df_sel),
  missing_count = colSums(is.na(df_sel)),
  missing_pct   = round(100 * colSums(is.na(df_sel)) / nrow(df_sel), 2)
) %>% arrange(desc(missing_count))

dup_chk <- df_sel %>% count(Indicator, SurveyYear) %>% filter(n > 1)
wide_effects <- df_sel %>%
  select(Indicator, SurveyYear, Value) %>%
  distinct() %>%
  tidyr::pivot_wider(names_from = SurveyYear, values_from = Value, names_prefix = "Y") %>%
  mutate(
    delta      = Y2016 - Y1998,
    pct_change = (Y2016 - Y1998)/Y1998,
    rank_1998  = rank(Y1998, ties.method = "average"),
    rank_2016  = rank(Y2016, ties.method = "average"),
    rank_shift = rank_2016 - rank_1998
  )

rk <- suppressWarnings(cor.test(wide_effects$rank_1998, wide_effects$rank_2016, method = "spearman"))
capture.output(rk, file = "data/interim/spearman_rank.txt")
write_csv_safe(df_sel,        "data/inter_
