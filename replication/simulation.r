library(tidyverse)
library(DeclareDesign)
library(ggthemes)


# Write a function to make untreated potential outcomes
make_Y0 <- function(X, JD) {
  N = length(X)
  2*X + 1.5*X^2 - 0.6*X^3 - JD/500
}

# Write a function to make treated potential outcomes
make_Y1 <- function(X, JD) {
  N = length(X)
  1.5 + 0.2*X - 1.4*X^2 - JD/500
}

rd_design <-
  declare_population(
    # Individual MNA constituencies
    N = 2000,
    # joined-district membership
    JD = sample(1:500, N, replace = TRUE),
    
    # Specify a forcing variable - margin of victory
    Z = runif(N, -0.5, 0.5),
    # Pr(Treatment=1) discontinuously changes at X=0
    D = 1*(Z > 0),
    noise = rnorm(N, 0.1,0.4)
  ) +
  declare_potential_outcomes(Y ~ D * make_Y1(X) + (1 - D) * make_Y0(X) + noise) +
  # Estimand is the ATE exactly at the cutpoint
  declare_estimand(LATE = make_Y1(0) - make_Y0(0)) +
  declare_reveal(Y, Z)

dat <- draw_data(rd_design)
