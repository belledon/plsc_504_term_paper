library(tidyverse)
library(haven)
library(estimatr)
library(reshape2)

dependent_vars <- c("any_violence", "ln_eventcount", "any_killed", "ln_numberkilled",
                    "ln_duration")

data <- read_dta("final_dataset.dta") %>%
  mutate(z = secular_close_win,
         d = secular_win,
         x = secular_close_race)
to_keep <- dependent_vars %>%
  c("z", "d", "x")
data <- data[to_keep]

# D on Z
png("output/D_on_Z.png", width = 800, height = 600)
data %>%
  ggplot(aes(x = z, y = d)) +
  geom_point() +
  labs(x = "Instrument", y = "Treatment") +
  theme_bw() + 
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=35,face="bold"))
dev.off()

# X on Z
png("output/X_on_Z.png", width = 800, height = 600)
data %>%
  ggplot(aes(x = z, y = x)) +
  geom_point() +
  labs(x = "Instrument", y = "Covariate X") +
  theme_bw() + 
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=35,face="bold"))
dev.off()

# Y on X

# New facet label names for supp variable
# dependent_vars <- c("any_violence", "ln_eventcount", "any_killed", "ln_numberkilled",
# "ln_duration")
labels <- list(
  "any_violence" = "Any Event", 
  "ln_eventcount" = "Event Count", 
  "any_killed" = "Any Killed", 
  "ln_numberkilled" = "Number Killed", 
  "ln_duration" = "Number Days")
my_labeler <- function(variable, value){
  return(labels[value])
}
png("output/Y_on_Z.png", width = 800, height = 600)
data %>%
  select(-x) %>%
  gather(-d, -z, key = "Dep", value = "Val") %>%
  ggplot(aes(y = Val, x = z, color = d)) +
  labs(x = "Instrument", y = "Violence") +
  geom_point() +
  facet_wrap(~ Dep, scales = "free",
             labeller = my_labeler) +
  theme_bw() + 
  theme(strip.text.x = element_text(size = 15)) + 
  # theme_bw() + 
  theme(axis.title=element_text(size=25))
        # axis.title=element_text(size=35,face="bold"))
dev.off()
