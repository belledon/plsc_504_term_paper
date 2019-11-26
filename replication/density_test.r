library(tidyverse)
library(haven)
library(estimatr)
library(texreg)
library(rddensity)

data <- read_dta("mccrary.dta") %>%
  # clean up nas
  filter_at(vars(secular_mov), all_vars(!is.na(.))) %>%
  filter((secular_mov > -1) & (secular_mov < 1))

## Fig A3 Histogram

png("fig_a3.png", width = 800, height = 600)
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

## Fig A4 Density Test

dens_test <- rddensity(data$secular_mov, p = 3)
dens <- density(data$secular_mov, n = 100, from = -1, to = 1)
dens_df <- data.frame(x = dens$x,
                      y = dens$y,
                      z = dens$x > 0)

fit <- lm_robust(y ~ poly(x, 3)*z, data = dens_df)

plot <- rdplotdensity(dens_test, data$secular_mov)

png("fig_a4.png", width = 800, height = 600)
rdplot <- rdplotdensity(dens_test, data$secular_mov,
              type = "line",
              lty = 2,
              title = "Running Variable McCrary Test",
              xlabel = "Secular party margin of victory/loss",
              ylabel = "Density")
rdplot$Estplot +
  geom_jitter(aes(x = x, y = y), alpha = 0.2, data = dens_df)
dev.off()

