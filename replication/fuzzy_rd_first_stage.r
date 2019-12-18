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


run_lm <- function(dep_var, right_hand, se_type) {
  form_str <- paste(dep_var, " ~ ", right_hand)
  fit <- with(data, lm_robust(as.formula(form_str), clusters = cluster_var, se_type = se_type))
  ret <- extract.lm_robust(fit,
                           include.ci = FALSE,
                           include.adjrs = FALSE,
                           include.rsquared = FALSE,
                           include.fstatistic = TRUE)
}

righthand <- "z + factor(province)"

results <- run_lm("d", righthand, "stata")

variable_names <- c("Secularist Victory (dummy)")


texreg(results,
       file = "output/fuzzy_rd_first_stage.tex",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(z = "Margin of Victory / Loss"),
       custom.model.names = variable_names,
       digits = 3,
       custom.note = ("\\parbox{.4\\linewidth}{\\vspace{2pt}%stars. \\\\
       Robust SEs clustered by cluster-district area, in brackets\\\\}"),
       table = FALSE)
