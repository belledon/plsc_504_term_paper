library(tidyverse)
library(haven)
library(estimatr)
library(texreg)

data <- read_dta("final_dataset.dta")


## xi: reg secular_win secular_close_win secular_close_race i.province, cl(cluster_var)
fit <- with(data,
            lm_robust(secular_win ~ secular_close_win + secular_close_race + factor(province),
                      clusters = cluster_var,
                      se_type = "stata")) %>%
  extract.lm_robust(.,
                    include.ci = FALSE,
                    include.adjrs = FALSE,
                    include.rsquared = FALSE,
                    include.fstatistic = TRUE)



texreg(list(fit),
       file = "output/first_stage.tex",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_win = "Prop. Secular Win",
                              secular_close_win = "Prop. Secular Close Win",
                              secular_close_race = "Prop. Secular Close Race"),
       custom.model.names = c("Prop. Secular Win"),
       digits = 3,
       caption = "First Stage",
       custom.note = ("\\parbox{.4\\linewidth}{\\vspace{2pt}%stars. \\\\
       Robust SEs clustered by cluster-district area, in brackets\\\\ F-statistic reported for Prop.Secular Close Win}"),
       table = FALSE,
       scalebox = 0.75
)
