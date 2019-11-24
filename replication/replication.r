library(tidyverse)
library(haven)
library(estimatr)
library(texreg)
library(rddensity)

## Replicates Tables 1-6

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
       file = "table2.tex",
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
       label = "table3",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_close_win_dummy = "Secularist Close Win"),
       custom.model.names = c("Any Event", "Event Count", "Any Killed", "Number Killed", "Number Days"),
       digits = 3,
       caption = "TABLE 3. Instrumental Variable Results",
       custom.note = "Robust SEs clustered by cluster-district area, in brackets",
       table = FALSE
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
       label = "table4",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_close_race = "Secularist Close Race"),
       custom.model.names = c("No Fixed Effects", "Disctrict Cluster FE", "Disctrict Cluster + Province-Year FEs"),
       digits = 3,
       caption = "TABLE 4. Correlation Between Close Secular/Nonsecular Elections and Violence at Time t-1",
       custom.note = "Robust SEs clustered by cluster-district area, in brackets",
       table = FALSE
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
  extract.lm_robust()

#  reg secular_vote_prop_current interaction2 ln_eventcount_6m_neg1_placebo prop_secular_incumbent_tmin1 i.cluster_var if (year !=1988), cl(cluster_var)
mech2 <- with(mech_data,
              lm_robust(secular_vote_prop_current ~ interaction2 + ln_eventcount_6m_neg1_placebo + prop_secular_incumbent_tmin1 + factor(cluster_var),
                        clusters = cluster_var)) %>%
  extract.lm_robust()
# reg secular_vote_prop_current interaction any_event_6m_neg1 prop_secular_incumbent_tmin1 i.cluster_var i.province_year if (year !=1988), cl(cluster_var)
mech3 <- with(mech_data,
              lm_robust(secular_vote_prop_current ~ interaction + any_event_6m_neg1 + prop_secular_incumbent_tmin1 + factor(cluster_var) + factor(province_year),
                        clusters = cluster_var)) %>%
  extract.lm_robust()

# reg secular_vote_prop_current interaction2 ln_eventcount_6m_neg1_placebo prop_secular_incumbent_tmin1 i.cluster_var i.province_year if (year !=1988), cl(cluster_var)
mech4 <- with(mech_data,
              lm_robust(secular_vote_prop_current ~ interaction2 + ln_eventcount_6m_neg1_placebo + prop_secular_incumbent_tmin1 + factor(cluster_var) + factor(province_year),
                        clusters = cluster_var)) %>%
  extract.lm_robust()

texreg(list(mech1, mech2, mech3, mech4),
       file = "table5.tex",
       label = "table5",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_vote_prop_current = "Secular Party Vote Share",
                              interaction = "Prop. Secular (t-1) x Any violence",
                              interaction2 = "Prop. Secular (t-1) x Event count (ln)",
                              any_event_6m_neg1 = "Any violence",
                              prop_secular_incumbent_tmin1 = "Prop. secularist wins (t - 1)",
                              ln_eventcount_6m_neg1_placebo = "Event count"),
       custom.model.names = c("", "", "", ""),
       digits = 3,
       caption = "TABLE 5. Mechanisms - Electoral Incentives",
       custom.note = "Robust SEs clustered by cluster-district area, in brackets",
       table = FALSE
       )

## Table 6, secular/ non-secular candidate differences

## data <- read_dta("mna_close_elections_final.dta")
## #feudal first_election elections_won_previously elections_cont_previously cab_after_elec cabinet_position swiched_btw_sec_nonsec
## to_summarize <- c("sect", "feudal", "first_election", "elections_won_previously", "elections_cont_previously",
##                   "cab_after_elec", "cabinet_position" ,"swiched_btw_sec_nonsec")

## table6_ttest <- t.test(sect ~ secular_party, data = data)

## library(knitr)
## table6_df <- data %>%
##   group_by(secular_party) %>%
##   summarise_at(to_summarize, list(mean = mean, std = sd))

## Fig 1: Parties
data <- read_dta("dalp_expert_evaluations.dta") %>%
  mutate(d4bin = d4bin == 1)

png("fig_1.png", width = 800, height = 600)
data %>%
  ggplot() +
  geom_histogram(aes(d4bin)) +
  facet_grid(partyorder ~ .) +
  theme_bw()
dev.off()

