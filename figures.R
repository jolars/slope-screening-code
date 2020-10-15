library(dplyr)
library(tidyr)
library(forcats)
library(lattice)
library(tactile)
library(latticeExtra)
library(RColorBrewer)

thm <- tactile.theme(c(7, 4), superpose.line = list(lty = 1:3))

fw <- 5.5 # full width
sim_efficiency_gaussian_correlated <-
  readRDS("results/sim_efficiency_gaussian_correlated.rds")
sim_efficiency_violations_real_data <-
  readRDS("results/sim_efficiency_violations_real_data.rds")
sim_performance_alg <-
  readRDS("results/sim_performance_alg.rds")
sim_performance_simulated_data <-
  readRDS("results/sim_performance_simulated_data.rds")
sim_violations_gaussian_correlated <-
  readRDS("results/sim_violations_gaussian_correlated.rds")

# gaussian-correlated -----------------------------------------------------

sim_gaussian_correlated <- sim_efficiency_gaussian_correlated %>%
  mutate(rho = as.factor(rho)) %>%
  rename(active = active_true,
         screened = active_screened)

pdf("figures/gaussian-correlated.pdf", width = fw, height = 1.5)
trellis.par.set(thm)
xyplot(screened + active ~ sigma | rho,
       data = sim_gaussian_correlated,
       layout = c(3, 1),
       grid = TRUE,
       xlab = expression(sigma/max((sigma))),
       ylab = "number of predictors",
       xlim = rev(extendrange(sim_gaussian_correlated$sigma)),
       par.settings = list(superpose.line = list(col = c(1, 1), lty = c(1, 2))),
       type = "l",
       strip = strip.custom(strip.names = c(TRUE, TRUE),
                            var.name = expression(rho)),
       auto.key = list(points = FALSE, lines = TRUE, x = 0, y = 0.85))
dev.off()
knitr::plot_crop("figures/gaussian-correlated.pdf")

# gaussian-correlated-violations ------------------------------------------

d_violations_gaussian_correlated <-
  as_tibble(sim_violations_gaussian_correlated) %>%
  mutate(p = as.factor(p),
         method = as.factor(method)) %>%
  group_by(method, p, sigma_ratio) %>%
  arrange(desc(sigma_ratio)) %>%
  summarise(mean_viol = mean(n_violations > 0)) %>%
  filter(method != "safe")

pdf("figures/gaussian-correlated-violations.pdf", width = fw, height = 1.6)
trellis.par.set(thm)
xyplot(mean_viol ~ sigma_ratio | p,
       layout = c(5, 1),
       type = "h",
       col = 1,
       lex = 0.5,
       xlim = c(1.25, 0.008),
       strip = strip.custom(strip.names = c(T, T)),
       auto.key = list(lines = TRUE, points = FALSE, columns = 2),
       xscale.components = xscale.components.log,
       scales = list(x = list(log = "e")),
       xlab = expression(sigma/max((sigma))),
       ylab = "fraction of fits\n with violations",
       data = d_violations_gaussian_correlated)
dev.off()
knitr::plot_crop("figures/gaussian-correlated-violations.pdf")

# performance-simulated-data ----------------------------------------------

pdf("figures/performance-simulated-data.pdf", width = fw, height = 2.7)
trellis.par.set(thm)
d_perf_sim <- as_tibble(sim_performance_simulated_data) %>%
  mutate(screening = factor(screening,
                            c(T, F),
                            c("screening", "no screening")),

         family = fct_recode(family,
                             OLS = "gaussian",
                             logistic = "binomial",
                             multinom = "multinomial"),
         family = fct_relevel(family, "OLS", "logistic", "poisson", "multinom"),
         correlation = as.factor(correlation))

bwplot2(correlation ~ time | family,
        data = d_perf_sim,
        groups = screening,
        layout = c(1, 4),
        xlab = "time (s)",
        ylab = expression(rho),
        strip.left = TRUE,
        strip = FALSE,
        par.settings = list(plot.symbol = list(col = 1)),
        scales = list(x = list(log = 10)),
        xscale.components = xscale.components.log10ticks,
        auto.key = list(space = "top", columns = 2))
dev.off()
knitr::plot_crop("figures/performance-simulated-data.pdf")

# performance-alg ---------------------------------------------------------

pdf("figures/performance-alg.pdf", width = 1.7, height = 1.7)

trellis.par.set(thm)
tmp <- sim_performance_alg %>%
  filter(q == 0.01) %>%
  group_by(rho, alg) %>%
  summarise(xbar = mean(time),
            n = n(),
            se = sd(time)/sqrt(n()),
            lo = xbar - qt(0.975, n-1)*se,
            hi = xbar + qt(0.975, n-1)*se)

pl <- xyplot(xbar ~ rho,
             groups = alg,
             type = "l",
             xlab = expression(rho),
             ylab = "time (s)",
             lower = tmp$lo,
             upper = tmp$hi,
             data = tmp,
             strip = strip.custom(strip.levels = c(T, T)),
             prepanel = prepanel.ci,
             auto.key = list(x = 0.05,
                             y = 0.95,
                             lines = TRUE,
                             points = FALSE),
             panel = function(...) {
               panel.ci(...)
               panel.xyplot(...)
             })
pl
dev.off()
knitr::plot_crop("figures/performance-alg.pdf")

# real-data-efficiency ----------------------------------------------------

pdf("figures/efficiency-real-data.pdf", width = fw, height = 2.5)
trellis.par.set(thm)
d_efficiency_violations_real <- sim_efficiency_violations_real_data
colnames(d_efficiency_violations_real) <-
  c("dataset",
    "response",
    "n",
    "p",
    "path_length",
    "penalty",
    "violations",
    "screened",
    "active",
    "unique",
    "KKT")

d_efficiency_violations_real_frac <-
  d_efficiency_violations_real %>%
  select(-violations, -unique, -KKT) %>%
  mutate(fraction = active/p,
         screened = screened/p,
         active = active/p,
         response = fct_recode(response,
                               OLS = "gaussian",
                               logistic = "binomial")) %>%
  group_by(path_length, dataset, response) %>%
  mutate(penalty_frac = (penalty - 1)/max(penalty - 1)) %>%
  ungroup() %>%
  pivot_longer(c(screened, active)) %>%
  unite("type", c(path_length, name)) %>%
  filter(!(type %in% c("20_active", "50_active"))) %>%
  mutate(type = as_factor(type),
         type = fct_recode(type,
                           "screened (20)" = "20_screened",
                           "screened (50)" = "50_screened",
                           "screened (100)" = "100_screened",
                           "active" = "100_active"))

cols <- brewer.pal(3, "Dark2")

p <- xyplot(value ~ penalty_frac | dataset + response,
            groups = type,
            ylab = "fraction of predictors",
            xlab = "fraction of path length",
            par.settings = list(superpose.line = list(col = c(cols, "black"),
                                                      lty = c(1, 1, 1, 2))),
            auto.key = list(lines = TRUE, points = FALSE, space = "top",
                            columns = 4),
            data = d_efficiency_violations_real_frac,
            type = "l")
useOuterStrips(p)
dev.off()
knitr::plot_crop("figures/efficiency-real-data.pdf")
