library(tidyverse)
library(brms)
library(janitor)
set.seed(502)


sim_dat <- function(nsims, nu) {
  cols <- nu |>
    map(function(nu) {
      rt(nsims, nu)
    })

  names(cols) <- paste0("nu_", nu)

  data <- as.data.frame(cols)

  return(data)
}

# select 6 different values of nu from 2-25 to simulate data from
nu <- sample(c(2:25), 6, replace = F) |> sort()
dat_100000 <- sim_dat(100000, nu) # simulate the data
dat_10000 <- sim_dat(10000, nu) # simulate the data
dat_1000 <- sim_dat(1000, nu) # simulate the data
dat_100 <- sim_dat(100, nu) # simulate the data

sprintf(
  "We are trying the following values of nu: %s, %s, %s, %s, %s, %s",
  nu[1],
  nu[2],
  nu[3],
  nu[4],
  nu[5],
  nu[6]
)

# Defining Prior Distributions

prior_uniform_1_50 <- prior(uniform(1, 50), class = "nu", lb = 1, ub = 50)
prior_gamma_1_4 <- prior(gamma(1, 4), class = "nu")
prior_gamma_15_5 <- prior(gamma(1.5, 5), class = "nu")
prior_gamma_2_5 <- prior(gamma(2, 5), class = "nu")
prior_half_normal_5 <- prior(normal(0, 5), class = "nu", lb = 0)
prior_half_normal_10 <- prior(normal(0, 10), class = "nu", lb = 0)
prior_half_cauchy <- prior(cauchy(0, 5), class = "nu", lb = 0)

# Sample Size 100

# uniform(1,50)

dat_100 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_uniform_1_50,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Unif_1_50_100.rds")

# gamma(1,4)

dat_100 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_gamma_1_4,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Gamma_1_4_100.rds")

# gamma(1.5,5)

dat_100 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_gamma_15_5,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Gamma_1_5_5_100.rds")

#gamma(2,5)

dat_100 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_gamma_2_5,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Gamma_2_5_100.rds")

# half normal(0,5)

dat_100 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_half_normal_5,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Half_Normal_0_5_100.rds")

# half normal(0,10)

dat_100 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_half_normal_10,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS( "Posteriors/Half_Normal_0_10_100.rds")

# half cauchy(0,5)
dat_100 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_half_cauchy,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Half_Cauchy_0_5_100.rds")

## Sample Size 1000

# uniform(1,50)
dat_1000 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_uniform_1_50,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Unif_1_50_1000.rds")

# gamma(1,4)

dat_1000 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_gamma_1_4,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Gamma_1_4_1000.rds")

# gamma(1.5,5)

dat_1000 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_gamma_15_5,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Gamma_1_5_5_1000.rds")

# gamma(2,5)

dat_1000 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_gamma_2_5,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Gamma_2_5_1000.rds")

# half normal(0,5)
dat_1000 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_half_normal_5,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Half_Normal_0_5_1000.rds")

# half normal(0,10)
dat_1000 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_half_normal_10,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Half_Normal_0_10_1000.rds")

# half cauchy(0,5)

dat_1000 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_half_cauchy,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Half_Cauchy_0_5_1000.rds")

# Sample size 10000

# unif(1,50)

dat_10000 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_uniform_1_50,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Unif_1_50_10000.rds")

# gamma(1,4)

dat_10000 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_gamma_1_4,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Gamma_1_4_10000.rds")

# gamma(1.5,5)

dat_10000 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_gamma_15_5,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Gamma_1_5_5_10000.rds")

# gamma(2,5)

dat_10000 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_gamma_2_5,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Gamma_2_5_10000.rds")

# half normal(0,5)
dat_10000 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_half_normal_5,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Half_Normal_0_5_10000.rds")

# half normal(0,10)
dat_10000 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_half_normal_10,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Half_Normal_0_10_10000.rds")

# half cauchy(0,5)
dat_10000 |>
  map(function(x) {
    temp_data <- data.frame(y = x)

    brm(
      y ~ 1,
      data = temp_data,
      family = student(),
      prior = c(
        prior_half_cauchy,
        prior(exponential(1), class = "sigma")
      ),
      warmup = 1000,
      iter = 51000,
      chains = 4
    )
  }) |> saveRDS("Posteriors/Half_Cauchy_0_5_10000.rds")