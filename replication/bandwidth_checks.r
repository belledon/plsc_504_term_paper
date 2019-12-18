library(tidyverse)
library(haven)
library(estimatr)
library(texreg)

data <- read_dta("final_dataset.dta")


## Runs a iv on a given dependent variable
run_iv <- function(dep_var, right_hand, se_type) {
  form_str <- paste(dep_var, " ~ ", right_hand)
  fit <- with(data, iv_robust(as.formula(form_str), clusters = cluster_var, se_type = se_type))
  ret <- extract.iv_robust(fit,
                           include.ci = FALSE,
                           include.adjrs = FALSE,
                           include.rsquared = FALSE,
                           include.fstatistic = TRUE)
}
variable_names <- c("Any Event", "Event Count", "Any Killed", "Number Killed", "Number Days")
dependent_vars <- c("any_violence", "ln_eventcount", "any_killed", "ln_numberkilled",
                    "ln_duration")

righthand_2pct <- "secular_win + secular_close_race_2pct + factor(province) | secular_close_win_2pct + secular_close_race_2pct + factor(province)"
results_2pct <- lapply(dependent_vars, function(d) run_iv(d, righthand_2pct, "stata"))



texreg(results_2pct,
       file = "output/bandwidth_2pct.tex",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_win = "Prop. Secular Win",
                              secular_close_race_2pct = "Prop. Secular Close Race $2\\%$"),
       custom.model.names = variable_names,
       digits = 3,
       custom.note = ("\\parbox{.6\\linewidth}{\\vspace{2pt}%stars. \\\\
       Robust SEs clustered by cluster-district area, in brackets\\\\ F-statistic reported for Prop. Secular Win}"),
       table = FALSE)

righthand_2p5pct <- "secular_win + secular_close_race_2p5pct + factor(province) | secular_close_win_2p5pct + secular_close_race_2p5pct + factor(province)"
results_2p5pct <- lapply(dependent_vars, function(d) run_iv(d, righthand_2p5pct, "stata"))



texreg(results_2p5pct,
       file = "output/bandwidth_2p5pct.tex",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_win = "Prop. Secular Win",
                              secular_close_race_2p5pct = "Prop. Secular Close Race $2.5\\%$"),
       custom.model.names = variable_names,
       digits = 3,
       custom.note = ("\\parbox{.6\\linewidth}{\\vspace{2pt}%stars. \\\\
       Robust SEs clustered by cluster-district area, in brackets\\\\ F-statistic reported for Prop. Secular Win}"),
       table = FALSE)

righthand_3p5pct <- "secular_win + secular_close_race_3p5pct + factor(province) | secular_close_win_3p5pct + secular_close_race_3p5pct + factor(province)"
results_3p5pct <- lapply(dependent_vars, function(d) run_iv(d, righthand_3p5pct, "stata"))



texreg(results_3p5pct,
       file = "output/bandwidth_3p5pct.tex",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_win = "Prop. Secular Win",
                              secular_close_race_3p5pct = "Prop. Secular Close Race $3.5\\%$"),
       custom.model.names = variable_names,
       digits = 3,
       custom.note = ("\\parbox{.6\\linewidth}{\\vspace{2pt}%stars. \\\\
       Robust SEs clustered by cluster-district area, in brackets\\\\ F-statistic reported for Prop. Secular Win}"),
       table = FALSE)

