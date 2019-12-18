library(tidyverse)
library(haven)
library(estimatr)
library(texreg)
library(rdrobust)


dependent_vars <- c("any_violence", "ln_eventcount", "any_killed", "ln_numberkilled",
                    "ln_duration")

data <- read_dta("final_dataset.dta") %>%
  filter(no_seats == 1) %>%
  # filter(secular_win == 1 | secular_win == 0) %>%
  mutate(z = margin_of_victory * (1 + 2*(secular_win - 1)),
         d = secular_win,
         x = secular_close_race)
to_keep <- dependent_vars %>%
  c("z", "d", "x", "province", "cluster_var")

data <- data[to_keep]

png("output/fuzzy_rd_treatment.png", width = 800, height = 600)
data %>%
  ggplot(aes(x = z, y = d)) +
  geom_point() +
  labs(x = "Avg. Margin of Victory", y = "Secularist Victory") +
  theme_bw() + 
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=35,face="bold"))
dev.off()

labels <- c(
  "any_violence" = "Any Event", 
  "ln_eventcount" = "Event Count", 
  "any_killed" = "Any Killed", 
  "ln_numberkilled" = "Number Killed", 
  "ln_duration" = "Number Days")

my_labeler <- function(string){
  return(labels[string])
}
png("output/fuzzy_rd_data.png", width = 800, height = 600)
data %>%
  select(-province, -x, -cluster_var) %>%
  gather(-z, -d, key = "Dep", value = "Val") %>%
  ggplot(aes(y = Val, x = z, color = factor(d))) +
  labs(x = "Instrument", y = "Violence") +
  geom_point() +
  facet_wrap(~ Dep, scales = "free",
             labeller = labeller(Dep = labels)) +
  theme_bw() + 
  theme(strip.text.x = element_text(size = 15)) + 
  # theme_bw() + 
  theme(axis.title=element_text(size=25))
# axis.title=element_text(size=35,face="bold"))
dev.off()

run_lm <- function(dep_var, right_hand, se_type) {
  form_str <- paste(dep_var, " ~ ", right_hand)
  fit <- with(data, lm_robust(as.formula(form_str), clusters = cluster_var, se_type = se_type))
  ret <- extract.lm_robust(fit,
                           include.ci = FALSE,
                           include.adjrs = FALSE,
                           include.rsquared = FALSE,
                           include.fstatistic = TRUE)
}

# ivregress 2sls any_violence secular_close_race (secular_win = secular_close_win) i.province, cl(cluster_var)
righthand <- "secular_close_win + secular_close_race + factor(province)"


results <- lapply(dependent_vars, function(d) run_lm(d, righthand, "stata"))

variable_names <- c("Any Event", "Event Count", "Any Killed", "Number Killed", "Number Days")


texreg(results,
       file = "output/fuzzy_rd_itt.tex",
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = list(secular_close_win = "Prop. Secular Win",
                              secular_close_race = "Prop. Secular Close Race"),
       custom.model.names = variable_names,
       digits = 3,
       custom.note = ("\\parbox{.4\\linewidth}{\\vspace{2pt}%stars. \\\\
       Robust SEs clustered by cluster-district area, in brackets\\\\ F-statistic reported for Prop. Secular Win}"),
       table = FALSE)


fit <- rdrobust(data$any_violence, data$z, fuzzy = data$d, p = 3)
rdplot(data$ln_eventcount, data$z, p = 2)

rd_wrapper <- function(data, outcome){
  fit <- with(data, rdrobust::rdrobust(y = data[[outcome]], x = z, 
                                       cluster = cluster_var,
                                       p = 1))
  ret <- data.frame(dep = c(labels[[outcome]]),
                    estimate = c(fit$coef[2]),
                    conf.low = c(fit$ci[3, 1]),
                    conf.hi = c(fit$ci[3,2]),
                    std.error = c(fit$se[3]),
                    p.vaule = c(fit$pv[3]))
}


results_stata <- lapply(dependent_vars, function(d) rd_wrapper(data, d))
# results_cr2 <- lapply(dependent_vars, function(d) run_iv(d, righthand, "CR2"))

results <- bind_rows(results_stata)

tabled <- results %>%
  knitr::kable(
    caption = "Estimate Comparison",
    format = "latex")


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

