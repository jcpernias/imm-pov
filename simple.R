library(tidyverse)

load("data/hhincome.Rdata")

hhincome <- hhincome %>%
  mutate(inc_cu = hh_inc / hh_cunits)

pov_tbl <- hhincome |>
  group_by(ecv_year) |>
  summarise(mean_inc = mean(inc_cu, na.rm = TRUE),
            median_inc = median(inc_cu, na.rm = TRUE),
            pov_line = 0.6 * median_inc)

pov <- hhincome |>
  left_join(pov_tbl, by = "ecv_year") |>
  mutate(pov = inc_cu < pov_line)


pov |>
  group_by(ecv_year) |>
  summarise(pov_rate = mean(pov, na.rm = TRUE)) |>
  print(n = 50)
