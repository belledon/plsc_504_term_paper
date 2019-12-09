library(tidyverse)
library(haven)
library(estimatr)
library(texreg)


mech_data <- read_dta("final_dataset.dta") %>%
  filter(year != 1988)

## Replicate table 5

# reg secular_vote_prop_current interaction any_event_6m_neg1 prop_secular_incumbent_tmin1 i.cluster_var if (year !=1988), cl(cluster_var)

mech1 <- with(mech_data,
              lm_robust(secular_vote_prop_current ~ interaction + any_event_6m_neg1 + prop_secular_incumbent_tmin1 + factor(cluster_var),
                        clusters = cluster_var,
                        se_type = "stata")) %>%
  extract.lm_robust(
    include.ci = FALSE,
    include.adjrs = FALSE,
    include.rsquared = FALSE,
    )

#  reg secular_vote_prop_current interaction2 ln_eventcount_6m_neg1_placebo prop_secular_incumbent_tmin1 i.cluster_var if (year !=1988), cl(cluster_var)
mech2 <- with(mech_data,
              lm_robust(secular_vote_prop_current ~ interaction2 + ln_eventcount_6m_neg1_placebo + prop_secular_incumbent_tmin1 + factor(cluster_var),
                        clusters = cluster_var,
                        se_type = "stata")) %>%
  extract.lm_robust(
    include.ci = FALSE,
    include.adjrs = FALSE,
    include.rsquared = FALSE,
    )

# reg secular_vote_prop_current interaction any_event_6m_neg1 prop_secular_incumbent_tmin1 i.cluster_var i.province_year if (year !=1988), cl(cluster_var)
mech3 <- with(mech_data,
              lm_robust(secular_vote_prop_current ~ interaction + any_event_6m_neg1 + prop_secular_incumbent_tmin1 + factor(cluster_var) + factor(province_year),
                        clusters = cluster_var,
                        se_type = "stata")) %>%
  extract.lm_robust(
    include.ci = FALSE,
    include.adjrs = FALSE,
    include.rsquared = FALSE,
    )

# reg secular_vote_prop_current interaction2 ln_eventcount_6m_neg1_placebo prop_secular_incumbent_tmin1 i.cluster_var i.province_year if (year !=1988), cl(cluster_var)
mech4 <- with(mech_data,
              lm_robust(secular_vote_prop_current ~ interaction2 + ln_eventcount_6m_neg1_placebo + prop_secular_incumbent_tmin1 + factor(cluster_var) + factor(province_year),
                        clusters = cluster_var,
                        se_type = "stata")) %>%
  extract.lm_robust(
    include.ci = FALSE,
    include.adjrs = FALSE,
    include.rsquared = FALSE,
    )

texreg(list(mech1, mech2, mech3, mech4),
       groups = list("OLS" = 1:4),
       file = "output/mechanisms.tex",
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
       custom.note = ("\\parbox{.4\\linewidth}{\\vspace{2pt}%stars. \\\\
       Robust SEs clustered by cluster-district area, in brackets \\\\ Province Fixed effects omitted}"),
       table = FALSE
       )
