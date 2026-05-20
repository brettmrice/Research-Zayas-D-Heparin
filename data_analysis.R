library(tidyverse)

APTT_2024_stats <- APTT_2024_hid |>
  select(!c(Test:DT_Complete)) |>
  distinct()

APTT_2025_stats <- APTT_2025_hid |>
  select(!c(Test:DT_Complete)) |>
  distinct()