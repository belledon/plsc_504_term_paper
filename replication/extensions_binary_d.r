library(tidyverse)
library(haven)
library(estimatr)
library(texreg)


dependent_vars <- c("any_violence", "ln_eventcount", "any_killed", "ln_numberkilled",
                    "ln_duration")

data <- read_dta("final_dataset.dta") %>%
  mutate(z = secular_close_win,
         d = secular_win > 0,
         x = secular_close_race)
to_keep <- dependent_vars %>%
  c("z", "d", "x", "province", "cluster_var")

data <- data[to_keep]

data %>%
  ggplot(aes(x = x, y = d)) +
  geom_point() +
  labs(x = "Instrument", y = "Treatment") +
  theme_bw() + 
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=35,face="bold"))


## Runs a iv on a given dependent variable
run_iv <- function(dep_var, right_hand, se_type) {
  form_str <- paste(dep_var, " ~ ", right_hand)
  fit <- with(data, iv_robust(as.formula(form_str), clusters = cluster_var, se_type = se_type))
  ret <- extract.iv_robust(fit,
                           include.ci = FALSE,
                           include.adjrs = FALSE,
                           include.rsquared = FALSE,
                           include.fstatistic = TRUE)
}

righthand <- "d +  factor(province) | z + factor(province)"

results_stata <- lapply(dependent_vars, function(d) run_iv(d, righthand, "stata"))
# results_cr2 <- lapply(dependent_vars, function(d) run_iv(d, righthand, "CR2"))

results <- results_stata
variable_names <- c("Any Event", "Event Count", "Any Killed", "Number Killed", "Number Days")


texreg(results,
       file = "output/extension_binary_d.tex",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(dTRUE = "Prop. Secular Win > 0"),
       custom.model.names = variable_names,
       digits = 3,
       custom.note = ("\\parbox{.4\\linewidth}{\\vspace{2pt}%stars. \\\\
       Robust SEs clustered by cluster-district area, in brackets\\\\ F-statistic reported for Prop. Secular Win}"),
       table = FALSE)

