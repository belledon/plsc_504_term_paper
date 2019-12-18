library(tidyverse)
library(haven)
library(estimatr)
library(texreg)
library(rdrobust)


dependent_vars <- c("any_violence", "ln_eventcount", "any_killed", "ln_numberkilled",
                    "ln_duration")

data <- read_dta("final_dataset.dta") %>%
  filter(no_seats == 1) %>%
  # filter(secular_win == 1 | secular_win == 0) %>%
  mutate(z = margin_of_victory * (1 + 2*(secular_win - 1)),
         d = secular_win,
         x = secular_close_race)
to_keep <- dependent_vars %>%
  c("z", "d", "x", "province", "cluster_var")

data <- data[to_keep]


labels <- list(
  "any_violence" = "Any Event", 
  "ln_eventcount" = "Event Count", 
  "any_killed" = "Any Killed", 
  "ln_numberkilled" = "Number Killed", 
  "ln_duration" = "Number Days")

variable_names <- c("Any Event", "Event Count", "Any Killed", "Number Killed", "Number Days")

rd_wrapper <- function(data, outcome, cutoff){
  fit <- with(data, rdrobust::rdrobust(y = data[[outcome]], x = z, cluster = cluster_var,
                                       c = cutoff,
                                       vce = "hc2"))
  ret <- data.frame(dep = c(outcome),
                    cutoff = c(cutoff),
                    coef = c(fit$coef[2]),
                    conf.low = c(fit$ci[3, 1]),
                    conf.hi = c(fit$ci[3,2]),
                    se = c(fit$se[3]),
                    p.vaule = c(fit$pv[3]))
}

results <- lapply(seq(-0.2, 0.2, by = 0.01), function(c) rd_wrapper(data, "ln_eventcount", c))

cutoff_df <- bind_rows(results)
png("output/fuzzy_rd_cutoff_placebo.png", width = 800, height = 600)
cutoff_df %>%
  ggplot(aes(x=coef, y=cutoff, colour=cutoff)) +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.hi)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  theme_bw()
dev.off()
