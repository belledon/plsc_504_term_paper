library(tidyverse)
library(haven)
library(estimatr)
library(texreg)

data <- read_dta("final_dataset.dta")


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

righthand <- "secular_win + secular_close_race + factor(province) | secular_close_win + secular_close_race + factor(province)"

dependent_vars <- c("area_gis", 
                    "pacca_pct", 
                    "electricity_pct", 
                    "gas_pct",
                    "lit_t",
                    "lit_f",
                    "pri_sch_pc")

results_stata <- lapply(dependent_vars, function(d) run_iv(d, righthand, "stata"))
# results_cr2 <- lapply(dependent_vars, function(d) run_iv(d, righthand, "CR2"))

results <- results_stata
variable_names <- c("Area",
                    "Pacca Prop. HHs",
                    "Elecriticy",
                    "Gas",
                    "Total Literacy", 
                    "Female Literacy",
                    "Primary Schools")


texreg(results,
       file = "output/pretreatment_census.tex",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_win = "Prop. Secular Win",
                              secular_close_race = "Prop. Secular Close Race"),
       custom.model.names = variable_names,
       digits = 3,
       custom.note = ("\\parbox{.4\\linewidth}{\\vspace{2pt}%stars. \\\\
       Electoral outcomes for 1988, 1990, 1993, and 1997 are used to predict (as a falsiﬁcation test) census outcomes measured in
1981; electoral outcomes for 2002 and 2008 are used to predict census outcomes measured in 1998. Sample sizes vary somewhat
across models due to missingness in some census data. Missingness is minimal and appears to be unsystematic.\\\\
       Robust SEs clustered by cluster-district area, in brackets\\\\ F-statistic reported for Prop. Secular Win}"),
       table = FALSE)
