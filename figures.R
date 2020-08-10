library(dplyr)
library(forcats)
library(lattice)
library(tactile)
library(latticeExtra)

thm <- tactile.theme(c(7, 4), superpose.line = list(lty = 1:3))

fw <- 5.5 # full width

load("data/sim_efficiency_gaussian_correlated.rda")
load("data/sim_efficiency_violations_real_data.rda")
load("data/sim_performance_alg.rda")
load("data/sim_performance_simulated_data.rda")
load("data/sim_violations_gaussian_correlated.rda")

# gaussian-correlated -----------------------------------------------------

sim_gaussian_correlated <- sim_efficiency_gaussian_correlated %>%
  mutate(rho = as.factor(rho)) %>%
  rename(active = active_true,
         screened = active_screened)

pdf("figures/gaussian-correlated.pdf", width = fw, height = 1.5)
trellis.par.set(thm)
xyplot(screened + active ~ sigma | rho,
       data = sim_gaussian_correlated,
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


# gaussian-correlated-violations ------------------------------------------

d_violations_gaussian_correlated <-
  as_tibble(sim_violations_gaussian_correlated) %>%
  mutate(p = as.factor(p),
         method = as.factor(method)) %>%
  group_by(method, p, sigma_ratio) %>%
  arrange(desc(sigma_ratio)) %>%
  summarise(mean_viol = mean(n_violations > 0)) %>%
  filter(method != "safe")

pdf("figures/gaussian-correlated-violations.pdf", width = 5, height = 1.3)
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

# performance-simulated-data ----------------------------------------------

pdf("figures/performance-simulated-data.pdf", width = 2.8, height = 2.7)
trellis.par.set(thm)
d_perf_sim <- as_tibble(sim_performance_simulated_data) %>%
  mutate(screening = factor(screening,
                            c(T, F),
                            c("screening", "no screening")),
         family = fct_recode(family,
                             OLS = "gaussian",
                             logistic = "binomial",
                             multinom = "multinomial"),
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

# real-data-efficiency ----------------------------------------------------

pdf("figures/efficiency-real-data.pdf", width = 5, height = 1.5)
trellis.par.set(thm)
d_efficiency_violations_real <- sim_efficiency_violations_real_data
colnames(d_efficiency_violations_real) <-
  c("dataset",
    "response",
    "n",
    "p",
    "penalty",
    "violations",
    "screened",
    "active",
    "unique",
    "KKT")
d_efficiency_violations_real_frac <-
  as_tibble(d_efficiency_violations_real) %>%
  mutate(fraction = active/p,
         screened = screened/p,
         active = active/p,
         response = fct_recode(response,
                               OLS = "gaussian",
                               logistic = "binomial"))

p <- xyplot(screened + active ~ penalty | dataset + response,
            ylab = "fraction of predictors",
            xlab = "penalty index",
            par.settings = list(superpose.line = list(col = 1, lty = c(1, 2))),
            auto.key = list(lines = TRUE, points = FALSE, x = 0.03, y = 0.84),
            data = d_efficiency_violations_real_frac,
            type = "l")
useOuterStrips(p)
dev.off()
