# table 1

library(dplyr)
library(tidyr)
library(forcats)
library(knitr)
library(kableExtra)

d <- readRDS("results/sim_performance_real_data.rds")

cap <- paste0(
  "Benchmarks measuring wall-clock time for four data sets fit with ",
  "different models using either the strong screening rule or non rule."
)

d %>%
  mutate(rule = ifelse(screening, "strong", "none")) %>%
  mutate(dataset = as.character(dataset)) %>%
  rename(model = family) %>%
  mutate(model = fct_recode(model, OLS = "gaussian", logistic = "binomial")) %>%
  group_by(dataset, model, rule, n, p) %>%
  summarise(time = round(mean(time))) %>%
  pivot_wider(values_from = time, names_from = rule) %>%
  rename("$n$" = n,
         "$p$" = p,
         "no screening" = none,
         screening = strong) %>%
  mutate(dataset = recode(dataset, "e2006" = "e2006-tfidf")) %>%
  kable("latex",
        escape = FALSE,
        booktabs = TRUE,
        caption = cap) %>%
  add_header_above(c("", "", "", "", "time (s)" = 2))
