library(tidyverse)
library(haven)
library(estimatr)
library(texreg)

## Replicates Tables 1-6

data <- read_dta("final_dataset.dta")

## replicate table 1 - Placebos on lag variables
placebo_dependent_vars <- c("any_lagged_violence", "ln_lagged_eventcount", "any_lagged_killed",
                    "ln_lagged_numberkilled", "ln_lagged_duration")

## Runs a iv on a given dependent variable
run_iv <- function(dep_var, right_hand) {
  form_str <- paste(dep_var, " ~ ", right_hand)
  fit <- with(data, iv_robust(as.formula(form_str), clusters = cluster_var, se_type = "stata"))
  ret <- extract.iv_robust(fit, include.ci = FALSE)
}

# ivregress 2sls any_violence secular_close_race (secular_win = secular_close_win) i.province, cl(cluster_var)
righthand <- "secular_win + secular_close_race + factor(province) | secular_close_win + secular_close_race + factor(province)"

placebo_test <- lapply(placebo_dependent_vars, function(d) run_iv(d, righthand))

texreg(placebo_test,
       file = "output/table1.tex",
       label = "table1",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_win = "Prop. Secular Win", 
                              secular_close_race = "Prop. Secular Clost Race"),
       custom.model.names = c("Any Event", "Event Count", "Any Killed", "Number Killed", "Number Days"),
       digits = 3,
       caption = "Placebo Check — Can Secular Victory in Close Elections at Time t Predict Prior Violence",
       custom.note = "Robust SEs clustered by cluster-district area, in brackets",
       table = FALSE,
       scalebox = 0.75
)
## replicate table 2 - ITT estimates
dependent_vars <- c("any_violence", "ln_eventcount", "any_killed", "ln_numberkilled",
                        "ln_duration")
itt_results <- lapply(dependent_vars, function(d) run_iv(d, righthand))

texreg(itt_results,
       file = "output/table2.tex",
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


