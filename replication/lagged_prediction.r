library(tidyverse)
library(haven)
library(estimatr)
library(texreg)


data <- read_dta("final_dataset.dta")
## replicate table 4

# reg any_lagged_killed secular_close_race, cl(cluster_var)
lag_killed1 <- with(data, lm_robust(any_lagged_killed ~ secular_close_race,
                                    clusters = cluster_var,
                                    se_type = "stata")) %>%
  extract.lm_robust(
    include.ci = FALSE,
    include.adjrs = FALSE,
    include.rsquared = FALSE,
  )

# reg any_lagged_killed secular_close_race i.cluster_var, cl(cluster_var)
lag_killed2 <- with(data, lm_robust(any_lagged_killed ~ secular_close_race + factor(cluster_var),
                                    clusters = cluster_var,
                                    se_type = "stata")) %>%
  extract.lm_robust(
    include.ci = FALSE,
    include.adjrs = FALSE,
    include.rsquared = FALSE,
    )

# reg any_lagged_killed secular_close_race i.cluster_var i.province_year, cl(cluster_var)
lag_killed3 <- with(data, lm_robust(any_lagged_killed ~ secular_close_race + factor(cluster_var) + factor(province_year), 
                                    clusters = cluster_var,
                                    se_type = "stata")) %>%
  extract.lm_robust(
    include.ci = FALSE,
    include.adjrs = FALSE,
    include.rsquared = FALSE,
    )

texreg(list(lag_killed1, lag_killed2, lag_killed3),
       file = "output/lagged_prediction.tex",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_close_race = "Secularist Close Race"),
       custom.model.names = c("No Fixed Effects", "Disctrict Cluster FE", "Disctrict Cluster + Province-Year FEs"),
       digits = 3,
       caption = "TABLE 4. Correlation Between Close Secular/Nonsecular Elections and Violence at Time t-1",
       custom.note = ("\\parbox{.4\\linewidth}{\\vspace{2pt}%stars. \\\\
       Robust SEs clustered by cluster-district area, in brackets}"),
       table = FALSE
)
