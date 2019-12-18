library(tidyverse)
library(haven)
library(estimatr)
library(texreg)
library(rddensity)

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

dens_test <- rddensity(data$z, p = 1)

# png("output/fuzzy_rd_desity.png", width = 800, height = 600)
rdplot <- rdplotdensity(dens_test, data$z,
                        # type = "line",
                        # lty = 2,
                        title = "Running Variable McCrary Test",
                        xlabel = "Secular party margin of victory/loss",
                        ylabel = "Density")
rdplot$Estplot
rdplot$Estplot +
  # theme(axis.title=element_text(size=25)) +
  geom_jitter(aes(x = x, y = y), alpha = 0.2, data = dens_df)
# dev.off()
