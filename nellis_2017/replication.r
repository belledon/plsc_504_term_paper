library(tidyverse)
library(haven)
library(estimatr)
library(texreg)

data <- read_dta("final_dataset.dta")


## replicate table 1 - Placebos on lag variables
placebo_dependent_vars <- c("any_lagged_violence", "ln_lagged_eventcount", "any_lagged_killed",
                    "ln_lagged_numberkilled", "ln_lagged_duration")

## Runs a iv on a given dependent variable
run_iv <- function(dep_var, right_hand) {
  form_str <- paste(dep_var, " ~ ", right_hand)
  fit <- with(data, iv_robust(as.formula(form_str), clusters = cluster_var))
  ret <- extract.iv_robust(fit)
}
righthand <- "secular_win + secular_close_race + factor(province) | secular_close_win + secular_close_race + factor(province)"

placebo_test <- lapply(placebo_dependent_vars, function(d) run_iv(d, righthand))

texreg(placebo_test,
       file = "table1.tex",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_win = "Prop. Secular Win", 
                              secular_close_race = "Prop. Secular Clost Race"),
       custom.model.names = c("Any Event", "Event Count", "Any Killed", "Number Killed", "Number Days"),
       digits = 3,
       caption = "TABLE 1. Placebo Check—Can Secular Victory in Close Elections at Time t Predict Prior Violence (t − 1)?",
       custom.note = "Robust SEs clustered by cluster-district area, in brackets"
)
## replicate table 2 - ITT estimates
dependent_vars <- c("any_violence", "ln_eventcount", "any_killed", "ln_numberkilled",
                        "ln_duration")
itt_results <- lapply(dependent_vars, function(d) run_iv(d, righthand))

texreg(itt_results,
       file = "table2.tex",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_win = "Prop. Secular Win",
                              secular_close_race = "Prop. Secular Clost Race"),
       custom.model.names = c("Any Event", "Event Count", "Any Killed", "Number Killed", "Number Days"),
       digits = 3,
       caption = "TABLE 2. Instrumental Variable Results",
       custom.note = "Robust SEs clustered by cluster-district area, in brackets"
       )


## replicate table 3 - DIM
run_dim <- function(dep_var, right_hand, df) {
  form_str <- paste(dep_var, " ~ ", right_hand)
  fit <- with(df, lm_robust(as.formula(form_str), clusters = cluster_var))
  ret <- extract.lm_robust(fit)
}
dim_right_hand <- "secular_close_win_dummy + factor(province)"
dim_data <- data %>%
  filter(no_secular_close_race == 1)
dim_results <- lapply(dependent_vars, function(d) run_dim(d, dim_right_hand, dim_data))

texreg(dim_results,
       file = "table3.tex",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_close_win_dummy = "Secularist Close Win"),
       custom.model.names = c("Any Event", "Event Count", "Any Killed", "Number Killed", "Number Days"),
       digits = 3,
       caption = "TABLE 3. Instrumental Variable Results",
       custom.note = "Robust SEs clustered by cluster-district area, in brackets"
       )

## replicate table 4

# reg any_lagged_killed secular_close_race, cl(cluster_var)
lag_killed1 <- with(data, lm_robust(any_lagged_killed ~ secular_close_race, clusters = cluster_var)) %>%
  extract.lm_robust()
# reg any_lagged_killed secular_close_race i.cluster_var, cl(cluster_var)
lag_killed2 <- with(data, lm_robust(any_lagged_killed ~ secular_close_race + factor(cluster_var), 
                                    clusters = cluster_var)) %>%
  extract.lm_robust()
# reg any_lagged_killed secular_close_race i.cluster_var i.province_year, cl(cluster_var)
lag_killed3 <- with(data, lm_robust(any_lagged_killed ~ secular_close_race + factor(cluster_var) + factor(province_year), 
                                    clusters = cluster_var)) %>%
  extract.lm_robust()

texreg(list(lag_killed1, lag_killed2, lag_killed3),
       file = "table4.tex",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_close_race = "Secularist Close Race"),
       custom.model.names = c("No Fixed Effects", "Disctrict Cluster FE", "Disctrict Cluster + Province-Year FEs"),
       digits = 3,
       caption = "TABLE 3. Instrumental Variable Results",
       custom.note = "Robust SEs clustered by cluster-district area, in brackets"
)

# Probably need to use GLM.nb
lag_any1 <- with(data, lm_robust(lagged_eventcount ~ secular_close_race, clusters = cluster_var)) %>%
  extract.lm_robust()
lag_any2 <- with(data, lm_robust(lagged_eventcount ~ secular_close_race + factor(cluster_var), 
                                    clusters = cluster_var)) %>%
  extract.lm_robust()
lag_any3 <- with(data, lm_robust(lagged_eventcount ~ secular_close_race + factor(cluster_var) + factor(province_year), 
                                    clusters = cluster_var)) %>%
  extract.lm_robust()


## Replicate table 5

# reg secular_vote_prop_current interaction any_event_6m_neg1 prop_secular_incumbent_tmin1 i.cluster_var if (year !=1988), cl(cluster_var)
mech_data <- data %>%
  filter(year != 1988)

mech1 <- with(mech_data, 
              lm_robust(secular_vote_prop_current ~ interaction + any_event_6m_neg1 + prop_secular_incumbent_tmin1 + factor(cluster_var), 
                    clusters = cluster_var)) %>% 
  tidy()

#  reg secular_vote_prop_current interaction2 ln_eventcount_6m_neg1_placebo prop_secular_incumbent_tmin1 i.cluster_var if (year !=1988), cl(cluster_var)
mech2 <- with(mech_data, 
              lm_robust(secular_vote_prop_current ~ interaction2 + ln_eventcount_6m_neg1_placebo + prop_secular_incumbent_tmin1 + factor(cluster_var), 
                        clusters = cluster_var)) %>% 
  tidy()
# reg secular_vote_prop_current interaction any_event_6m_neg1 prop_secular_incumbent_tmin1 i.cluster_var i.province_year if (year !=1988), cl(cluster_var)
mech3 <- with(mech_data, 
              lm_robust(secular_vote_prop_current ~ interaction + any_event_6m_neg1 + prop_secular_incumbent_tmin1 + factor(cluster_var) + factor(province_year), 
                        clusters = cluster_var)) %>% 
  tidy()

# reg secular_vote_prop_current interaction2 ln_eventcount_6m_neg1_placebo prop_secular_incumbent_tmin1 i.cluster_var i.province_year if (year !=1988), cl(cluster_var)
mech4 <- with(mech_data, 
              lm_robust(secular_vote_prop_current ~ interaction2 + ln_eventcount_6m_neg1_placebo + prop_secular_incumbent_tmin1 + factor(cluster_var) + factor(province_year), 
                        clusters = cluster_var)) %>% 
  tidy()


## Table 6, secular/ non-secular candidate differences

data <- read_dta("mna_close_elections_final.dta")
#feudal first_election elections_won_previously elections_cont_previously cab_after_elec cabinet_position swiched_btw_sec_nonsec
to_summarize <- c("sect", "feudal", "first_election", "elections_won_previously", "elections_cont_previously",
                  "cab_after_elec", "cabinet_position" ,"swiched_btw_sec_nonsec")

table6_df <- data %>%
  group_by(secular_party) %>%
  summarise_at(to_summarize, list(mean = mean, std = sd))
