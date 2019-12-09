
library(tidyverse)
library(haven)
library(estimatr)
library(texreg)

## replicate table 3 - DIM

dim_data <- read_dta("final_dataset.dta") %>%
  filter(no_secular_close_race == 1)

dependent_vars <- c("any_violence", "ln_eventcount", "any_killed", "ln_numberkilled",
                    "ln_duration")
run_dim <- function(dep_var, right_hand, df) {
  form_str <- paste(dep_var, " ~ ", right_hand)
  fit <- with(df, lm_robust(as.formula(form_str), clusters = cluster_var, se_type = "stata"))
  ret <- extract.lm_robust(fit, include.ci = FALSE)
}
dim_right_hand <- "secular_close_win_dummy + factor(province)"
dim_results <- lapply(dependent_vars, function(d) run_dim(d, dim_right_hand, dim_data))

texreg(dim_results,
       file = "output/table3.tex",
       label = "table3",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_close_win_dummy = "Secularist Close Win"),
       custom.model.names = c("Any Event", "Event Count", "Any Killed", "Number Killed", "Number Days"),
       digits = 3,
       caption = "TABLE 3. Instrumental Variable Results",
       custom.note = "Robust SEs clustered by cluster-district area, in brackets",
       table = FALSE
       )
