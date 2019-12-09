library(tidyverse)
library(haven)
library(estimatr)
library(texreg)


data <- read_dta("final_dataset.dta")


## Runs a iv on a given dependent variable
run_iv <- function(dep_var, right_hand) {
  form_str <- paste(dep_var, " ~ ", right_hand)
  fit <- with(data, iv_robust(as.formula(form_str), clusters = cluster_var, se_type = "stata"))
  ret <- extract.iv_robust(fit,
                           include.ci = FALSE,
                           include.adjrs = FALSE,
                           include.rsquared = FALSE,
                           include.fstatistic = TRUE)
}

# ivregress 2sls any_violence secular_close_race (secular_win = secular_close_win) i.province, cl(cluster_var)
righthand <- "secular_win + secular_close_race + factor(province) | secular_close_win + secular_close_race + factor(province)"

## replicate table 2 - ITT estimates
dependent_vars <- c("any_violence", "ln_eventcount", "any_killed", "ln_numberkilled",
                        "ln_duration")
itt_results <- lapply(dependent_vars, function(d) run_iv(d, righthand))

texreg(itt_results,
       file = "output/iv_ate.tex",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_win = "Prop. Secular Win",
                              secular_close_race = "Prop. Secular Close Race"),
       custom.model.names = c("Any Event", "Event Count", "Any Killed", "Number Killed", "Number Days"),
       digits = 3,
       custom.note = ("\\parbox{.4\\linewidth}{\\vspace{2pt}%stars. \\\\
       Robust SEs clustered by cluster-district area, in brackets\\\\ F-statistic reported for Prop. Secular Win}"),
       table = FALSE
       )


