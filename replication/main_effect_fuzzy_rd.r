library(tidyverse)
library(haven)
library(estimatr)
library(texreg)
library(rdrobust)

## replicate table 3 - DIM

dim_data <- read_dta("final_dataset.dta") %>%
  filter(no_seats == 1) %>%
  filter_at(vars(margin_of_victory, secular_close_win_dummy, any_violence), all_vars(!is.na(.))) %>%
  mutate(z = margin_of_victory * (1 + 2*(secular_close_win_dummy - 1)))
  # filter(no_secular_close_race == 1) %>%
  # mutate(z = margin_of_victory * (1 + 2*(secular_close_win_dummy - 1)))

dim_data <- read_dta("final_dataset.dta") %>%
  filter_at(vars(margin_of_victory, secular_close_win_dummy, any_violence), all_vars(!is.na(.))) %>%
  mutate(z = )

rdplot(dim_data$ln_eventcount, dim_data$z, p = 1)
rdplot(dim_data$secular_win, dim_data$z, p = 1)


dependent_vars <- c("any_violence", "ln_eventcount", "any_killed", "ln_numberkilled",
                    "ln_duration")
rd_wrapper <- 
  function(data, x, y, z){
    fit <- rdrobust::rdrobust(y = data[[y]], x = data[[x]], fuzzy = data[[z]], vce = "hc2")
    ret <- data.frame(rownames(fit$coef), fit$coef, fit$se, fit$z, fit$pv, fit$ci)
    row.names(ret) <- NULL
    names(ret) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
    ret
  }
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