library(tidyverse)
library(haven)
library(estimatr)
library(texreg)
library(rddensity)

## Fig A4 Density Test

data <- read_dta("mccrary.dta")
dens_test <- rddensity(data$secular_mov)
png("fig_a4.png", width = 800, height = 600)
data %>%
  ggplot(aes(x = secular_mov)) +
  stat_density() +
  geom_vline(xintercept = 0) +
  ggtitle("Running Variable Density") +
  xlab("Secular party margin of victory/loss") +
  ylab("Density") +
  xlim(-1, 1) +
  theme_bw()
dev.off()
