library(tidyverse)
library(haven)
library(estimatr)
library(texreg)
library(rddensity)

data <- read_dta("mccrary.dta") %>%
  # clean up nas
  filter_at(vars(secular_mov), all_vars(!is.na(.))) %>%
  filter((secular_mov > -1) & (secular_mov < 1))

## Fig A4 Density Test

dens_test <- rddensity(data$secular_mov, p = 3)

png("output/fig_a4.png", width = 800, height = 600)
rdplot <- rdplotdensity(dens_test, data$secular_mov,
              # type = "line",
              lty = 2,
              title = "Running Variable McCrary Test",
              xlabel = "Secular party margin of victory/loss",
              ylabel = "Density")
rdplot$Estplot +
  # theme(axis.title=element_text(size=25)) +
  geom_jitter(aes(x = x, y = y), alpha = 0.2, data = dens_df)
dev.off()

