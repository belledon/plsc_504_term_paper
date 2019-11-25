library(tidyverse)
library(haven)
library(estimatr)
library(texreg)
library(rdrobust)

## Extension that estimates the ATT using fuzzy rd via `rdrobust` package

data <- read_dta("final_dataset.dta") %>%
  ## Filter and clean data
  filter_at(vars(any_violence, secular_win, secular_close_race),
            all_vars(!is.na(.))) %>%
  # for exploration, only consider relevant rows
  select(joined_district, year, totalsecularvotes_current, valid_current, no_seats, secular_win, 
         secular_close_race, no_secular_close_race, margin_of_victory)

fit <- rdrobust(data$any_violence, data$secular_close_race,
                c = 0.5)


## extract.rdrobust = function(fit) {
## }

## ## Runs fuzzy rd-robust
## run_iv <- function(dep_var, right_hand) {
##   form_str <- paste(dep_var, " ~ ", right_hand)
##   fit <- with(data, rdrobust(as.formula(form_str),
##                              clusters = cluster_var,
##                              fuzzy = secular_close_race))
##   ret <- extract.rdrobust(fit)
## }
## righthand <- "secular_win + secular_close_race + factor(province) | secular_close_win + secular_close_race + factor(province)"

## dependent_vars <- c("any_violence", "ln_eventcount", "any_killed", "ln_numberkilled",
##                     "ln_duration")

## att_results <- lapply(dependent_vars, function(d) run_iv(d, righthand))

## texreg(itt_results,
##        file = "fuzzy_rd_att.tex",
##        label = "fuzzy_rd_att",
##        stars = c(0.01, 0.05, 0.1),
##        custom.coef.map = list(secular_win = "Prop. Secular Win",
##                               secular_close_race = "Prop. Secular Clost Race"),
##        custom.model.names = c("Any Event", "Event Count", "Any Killed", "Number Killed", "Number Days"),
##        digits = 3,
##        custom.note = "Robust SEs clustered by cluster-district area, in brackets",
##        table = FALSE
##        )

## placebo_test <- lapply(placebo_dependent_vars, function(d) run_iv(d, righthand))

## texreg(placebo_test,
##        file = "table1.tex",
##        label = "table1",
##        stars = c(0.01, 0.05, 0.1),
##        custom.coef.map = list(secular_win = "Prop. Secular Win", 
##                               secular_close_race = "Prop. Secular Clost Race"),
##        custom.model.names = c("Any Event", "Event Count", "Any Killed", "Number Killed", "Number Days"),
##        digits = 3,
##        caption = "Placebo Check — Can Secular Victory in Close Elections at Time t Predict Prior Violence",
##        custom.note = "Robust SEs clustered by cluster-district area, in brackets",
##        table = FALSE,
##        scalebox = 0.75
## )

