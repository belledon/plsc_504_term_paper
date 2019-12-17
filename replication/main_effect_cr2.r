library(tidyverse)
library(haven)
library(estimatr)
library(texreg)

data <- read_dta("final_dataset.dta")

## Runs a iv on a given dependent variable
run_iv <- function(dep_var, right_hand) {
  form_str <- paste(dep_var, " ~ ", right_hand)
  fit <- with(data, iv_robust(as.formula(form_str), clusters = cluster_var, se_type = "CR2"))
  ret <- extract.iv_robust(fit, include.ci = FALSE)
}

# ivregress 2sls any_violence secular_close_race (secular_win = secular_close_win) i.province, cl(cluster_var)
righthand <- "secular_win + secular_close_race + factor(province) | secular_close_win + secular_close_race + factor(province)"

dependent_vars <- c("any_violence", "ln_eventcount", "any_killed", "ln_numberkilled",
                        "ln_duration")
itt_results <- lapply(dependent_vars, function(d) run_iv(d, righthand))

texreg(itt_results,
       file = "output/table2_cr2.tex",
       label = "table2",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_win = "Prop. Secular Win",
                              secular_close_race = "Prop. Secular Clost Race"),
       custom.model.names = c("Any Event", "Event Count", "Any Killed", "Number Killed", "Number Days"),
       digits = 3,
       caption = "TABLE 2. Instrumental Variable Results",
       custom.note = "Robust SEs clustered by cluster-district area, in brackets",
       table = FALSE
       )
texreg(itt_results[3],
       file = "output/table2_cr2_single.tex",
       label = "table2",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_win = "Prop. Secular Win",
                              secular_close_race = "Prop. Secular Clost Race"),
       custom.model.names = c("Any Event", "Event Count", "Any Killed", "Number Killed", "Number Days")[3],
       digits = 3,
       custom.note = "Robust SEs clustered by cluster-district area, in brackets",
       table = FALSE
)

