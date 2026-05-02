library(tidyverse)
library(brms)
library(janitor)
set.seed(502)


# nu: 5, 6, 7, 15, 17, 20

#------------------ Uniform(1,50) -----------------------

## n = 100

### reading in posteriors
uniform_post_100 <- readRDS("Posteriors/Unif_1_50_100.rds")

### Getting Posterior Distributions/Traceplots/Summaries
post_summaries_unif_100 <- uniform_post_100 |> map(\(x) summary(x))
post_nu_unif_100 <- uniform_post_100 |> map(\(x) as_draws_df(x)$nu)
post_mean_unif_100 <- post_nu_unif_100 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_unif_100)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Uniform(0,50) prior"
        ),
        subtitle = "n = 100"
    )

#### traceplots:

mcmc_plot(uniform_post_100$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 100"
    )
mcmc_plot(uniform_post_100$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 100"
    )
mcmc_plot(uniform_post_100$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 100"
    )
mcmc_plot(uniform_post_100$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 100"
    )
mcmc_plot(uniform_post_100$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 100"
    )
mcmc_plot(uniform_post_100$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 100"
    )

#### Fit Summaries

post_summaries_unif_100

## n = 1000

### reading in posteriors
uniform_post_1000 <- readRDS("Posteriors/Unif_1_50_1000.rds")

### Getting Posterior Distributions/Traceplots/Summaries
post_summaries_unif_1000 <- uniform_post_1000 |> map(\(x) summary(x))
post_nu_unif_1000 <- uniform_post_1000 |> map(\(x) as_draws_df(x)$nu)
post_mean_unif_1000 <- post_nu_unif_1000 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_unif_1000)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Uniform(1,50) prior"
        ),
        subtitle = "n = 1000"
    )

#### traceplots:

mcmc_plot(uniform_post_1000$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 1000"
    )
mcmc_plot(uniform_post_1000$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 1000"
    )
mcmc_plot(uniform_post_1000$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 1000"
    )
mcmc_plot(uniform_post_1000$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 1000"
    )
mcmc_plot(uniform_post_1000$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 1000"
    )
mcmc_plot(uniform_post_1000$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 1000"
    )

#### Fit summaries

post_summaries_unif_1000

## n = 10000

### reading in posteriors
uniform_post_10000 <- readRDS("Posteriors/Unif_1_50_10000.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_unif_10000 <- uniform_post_10000 |> map(\(x) summary(x))
post_nu_unif_10000 <- uniform_post_10000 |> map(\(x) as_draws_df(x)$nu)
post_mean_unif_10000 <- post_nu_unif_10000 |> map(mean)


#### Posterior histograms
df <- stack(post_nu_unif_10000)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Uniform(1,50) prior"
        ),
        subtitle = "n = 10000"
    )

#### traceplots:

mcmc_plot(uniform_post_10000$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 10000"
    )
mcmc_plot(uniform_post_10000$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 10000"
    )
mcmc_plot(uniform_post_10000$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 10000"
    )
mcmc_plot(uniform_post_10000$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 10000"
    )
mcmc_plot(uniform_post_10000$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 10000"
    )
mcmc_plot(uniform_post_10000$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Uniform(1, 50) Prior | Sample Size 10000"
    )

#### Fit Summaries

post_summaries_unif_10000

#---------------- Gamma(1,4) ------------------------

## n = 100

### reading in posteriors
gamma_post_14_100 <- readRDS("Posteriors/Gamma_1_4_100.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_gamma_14_100 <- gamma_post_14_100 |> map(\(x) summary(x))
post_nu_gamma_14_100 <- gamma_post_14_100 |> map(\(x) as_draws_df(x)$nu)
post_mean_gamma_14_100 <- post_nu_gamma_14_100 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_gamma_14_100)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Gamma(1,4) prior"
        ),
        subtitle = "n = 100"
    )

#### traceplots:

mcmc_plot(gamma_post_14_100$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Gamma(1,4) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_14_100$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Gamma(1,4) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_14_100$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Gamma(1,4) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_14_100$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Gamma(1,4) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_14_100$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Gamma(1,4) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_14_100$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Gamma(1,4) Prior | Sample Size 100"
    )

#### Fit Summaries

post_summaries_gamma_14_100

## n = 1000

### reading in posteriors
gamma_post_14_1000 <- readRDS("Posteriors/Gamma_1_4_1000.rds")
### Getting Posterior Distributions/Traceplots/Summaries


post_summaries_gamma_14_1000 <- gamma_post_14_1000 |> map(\(x) summary(x))
post_nu_gamma_14_1000 <- gamma_post_14_1000 |> map(\(x) as_draws_df(x)$nu)
post_mean_gamma_14_1000 <- post_nu_gamma_14_1000 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_gamma_14_1000)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Gamma(1,4) prior"
        ),
        subtitle = "n = 1000"
    )

#### traceplots:

mcmc_plot(gamma_post_14_1000$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Gamma(1,4) Prior | Sample Size 1000"
    )
mcmc_plot(gamma_post_14_1000$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Gamma(1,4) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_14_1000$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Gamma(1,4) Prior | Sample Size 1000"
    )
mcmc_plot(gamma_post_14_1000$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Gamma(1,4) Prior | Sample Size 1000"
    )
mcmc_plot(gamma_post_14_1000$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Gamma(1,4) Prior | Sample Size 1000"
    )
mcmc_plot(gamma_post_14_1000$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Gamma(1,4) Prior | Sample Size 1000"
    )

#### Fit Summaries

post_summaries_gamma_14_1000


## n = 10000

### reading in posteriors
gamma_post_14_10000 <- readRDS("Posteriors/Gamma_1_4_10000.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_gamma_14_10000 <- gamma_post_14_10000 |> map(\(x) summary(x))
post_nu_gamma_14_10000 <- gamma_post_14_10000 |> map(\(x) as_draws_df(x)$nu)
post_mean_gamma_14_10000 <- post_nu_gamma_14_10000 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_gamma_14_10000)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Gamma(1,4) prior"
        ),
        subtitle = "n = 10000"
    )

#### traceplots:

mcmc_plot(gamma_post_14_10000$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Gamma(1,4) Prior | Sample Size 10000"
    )
mcmc_plot(gamma_post_14_10000$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Gamma(1,4) Prior | Sample Size 10000"
    )
mcmc_plot(gamma_post_14_10000$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Gamma(1,4) Prior | Sample Size 10000"
    )
mcmc_plot(gamma_post_14_10000$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Gamma(1,4) Prior | Sample Size 10000"
    )
mcmc_plot(gamma_post_14_10000$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Gamma(1,4) Prior | Sample Size 10000"
    )
mcmc_plot(gamma_post_14_10000$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Gamma(1,4) Prior | Sample Size 10000"
    )

#### Fit Summaries

post_summaries_gamma_14_10000

#---------------- Gamma(1.5,5) ----------------------

## n = 100

### reading in posteriors
gamma_post_155_100 <- readRDS("Posteriors/Gamma_1_5_5_100.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_gamma_155_100 <- gamma_post_155_100 |> map(\(x) summary(x))
post_nu_gamma_155_100 <- gamma_post_155_100 |> map(\(x) as_draws_df(x)$nu)
post_mean_gamma_155_100 <- post_nu_gamma_155_100 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_gamma_155_100)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Gamma(1.5,5) prior"
        ),
        subtitle = "n = 100"
    )

#### traceplots:

mcmc_plot(gamma_post_155_100$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_155_100$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_155_100$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_155_100$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_155_100$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_155_100$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 100"
    )

#### Fit Summaries

post_summaries_gamma_155_100

## n = 1000

### reading in posteriors
gamma_post_155_1000 <- readRDS("Posteriors/Gamma_1_5_5_1000.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_gamma_155_1000 <- gamma_post_155_1000 |> map(\(x) summary(x))
post_nu_gamma_155_1000 <- gamma_post_155_1000 |> map(\(x) as_draws_df(x)$nu)
post_mean_gamma_155_1000 <- post_nu_gamma_155_1000 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_gamma_155_1000)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Gamma(1.5,5) prior"
        ),
        subtitle = "n = 1000"
    )

#### traceplots:

mcmc_plot(gamma_post_155_1000$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 1000"
    )
mcmc_plot(gamma_post_155_1000$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 1000"
    )
mcmc_plot(gamma_post_155_1000$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 1000"
    )
mcmc_plot(gamma_post_155_1000$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 1000"
    )
mcmc_plot(gamma_post_155_1000$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 1000"
    )
mcmc_plot(gamma_post_155_1000$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 1000"
    )

#### Fit Summaries

post_summaries_gamma_155_1000

## n = 10000

### reading in posteriors
gamma_post_155_10000 <- readRDS("Posteriors/Gamma_1_5_5_10000.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_gamma_155_10000 <- gamma_post_155_10000 |> map(\(x) summary(x))
post_nu_gamma_155_10000 <- gamma_post_155_10000 |> map(\(x) as_draws_df(x)$nu)
post_mean_gamma_155_10000 <- post_nu_gamma_155_10000 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_gamma_155_10000)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Gamma(1.5,5) prior"
        ),
        subtitle = "n = 10000"
    )

#### traceplots:

mcmc_plot(gamma_post_155_10000$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 10000"
    )
mcmc_plot(gamma_post_155_10000$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 10000"
    )
mcmc_plot(gamma_post_155_10000$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 10000"
    )
mcmc_plot(gamma_post_155_10000$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 10000"
    )
mcmc_plot(gamma_post_155_10000$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 10000"
    )
mcmc_plot(gamma_post_155_10000$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Gamma(1.5,5) Prior | Sample Size 10000"
    )

#### Fit Summaries

post_summaries_gamma_155_10000

#------------------------ Gamma(2,5) ----------------------

## n = 100

### reading in posteriors
gamma_post_25_100 <- readRDS("Posteriors/Gamma_2_5_100.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_gamma_25_100 <- gamma_post_25_100 |> map(\(x) summary(x))
post_nu_gamma_25_100 <- gamma_post_25_100 |> map(\(x) as_draws_df(x)$nu)
post_mean_gamma_25_100 <- post_nu_gamma_25_100 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_gamma_25_100)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Gamma(2,5) prior"
        ),
        subtitle = "n = 100"
    )

#### traceplots:

mcmc_plot(gamma_post_25_100$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Gamma(2,5) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_25_100$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Gamma(2,5) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_25_100$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Gamma(2,5) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_25_100$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Gamma(2,5) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_25_100$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Gamma(2,5) Prior | Sample Size 100"
    )
mcmc_plot(gamma_post_25_100$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Gamma(2,5) Prior | Sample Size 100"
    )

#### Fit Summaries

post_summaries_gamma_25_100

## n = 1000

### reading in posteriors
gamma_post_25_1000 <- readRDS("Posteriors/Gamma_2_5_1000.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_gamma_25_1000 <- gamma_post_25_1000 |> map(\(x) summary(x))
post_nu_gamma_25_1000 <- gamma_post_25_1000 |> map(\(x) as_draws_df(x)$nu)
post_mean_gamma_25_1000 <- post_nu_gamma_25_1000 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_gamma_25_1000)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Gamma(2,5) prior"
        ),
        subtitle = "n = 1000"
    )

#### traceplots:

mcmc_plot(gamma_post_25_1000$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Gamma(2,5) Prior | Sample Size 1000"
    )
mcmc_plot(gamma_post_25_1000$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Gamma(2,5) Prior | Sample Size 1000"
    )
mcmc_plot(gamma_post_25_1000$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Gamma(2,5) Prior | Sample Size 1000"
    )
mcmc_plot(gamma_post_25_1000$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Gamma(2,5) Prior | Sample Size 1000"
    )
mcmc_plot(gamma_post_25_1000$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Gamma(2,5) Prior | Sample Size 1000"
    )
mcmc_plot(gamma_post_25_1000$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Gamma(2,5) Prior | Sample Size 1000"
    )

#### Fit Summaries

post_summaries_gamma_25_1000

## n = 10000

### reading in posteriors
gamma_post_25_10000 <- readRDS("Posteriors/Gamma_2_5_10000.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_gamma_25_10000 <- gamma_post_25_10000 |> map(\(x) summary(x))
post_nu_gamma_25_10000 <- gamma_post_25_10000 |> map(\(x) as_draws_df(x)$nu)
post_mean_gamma_25_10000 <- post_nu_gamma_25_10000 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_gamma_25_10000)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Gamma(2,5) prior"
        ),
        subtitle = "n = 10000"
    )

#### traceplots:

mcmc_plot(gamma_post_25_10000$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Gamma(2,5) Prior | Sample Size 10000"
    )
mcmc_plot(gamma_post_25_10000$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Gamma(2,5) Prior | Sample Size 10000"
    )
mcmc_plot(gamma_post_25_10000$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Gamma(2,5) Prior | Sample Size 10000"
    )
mcmc_plot(gamma_post_25_10000$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Gamma(2,5) Prior | Sample Size 10000"
    )
mcmc_plot(gamma_post_25_10000$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Gamma(2,5) Prior | Sample Size 10000"
    )
mcmc_plot(gamma_post_25_10000$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Gamma(2,5) Prior | Sample Size 10000"
    )

#### Fit Summaries

post_summaries_gamma_25_10000

#------------------------ Half Normal(0,5) ------------------------

## n = 100

### reading in posteriors
hnorm_post_05_100 <- readRDS("Posteriors/Half_Normal_0_5_100.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_hnorm_05_100 <- hnorm_post_05_100 |> map(\(x) summary(x))
post_nu_hnorm_05_100 <- hnorm_post_05_100 |> map(\(x) as_draws_df(x)$nu)
post_mean_hnorm_05_100 <- post_nu_hnorm_05_100 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_hnorm_05_100)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Half Normal(0,5) prior"
        ),
        subtitle = "n = 100"
    )

#### traceplots:

mcmc_plot(hnorm_post_05_100$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 100"
    )
mcmc_plot(hnorm_post_05_100$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 100"
    )
mcmc_plot(hnorm_post_05_100$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 100"
    )
mcmc_plot(hnorm_post_05_100$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 100"
    )
mcmc_plot(hnorm_post_05_100$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 100"
    )
mcmc_plot(hnorm_post_05_100$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 100"
    )

#### Fit Summaries

post_summaries_hnorm_05_100

## n = 1000

### reading in posteriors
hnorm_post_05_1000 <- readRDS("Posteriors/Half_Normal_0_5_1000.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_hnorm_05_1000 <- hnorm_post_05_1000 |> map(\(x) summary(x))
post_nu_hnorm_05_1000 <- hnorm_post_05_1000 |> map(\(x) as_draws_df(x)$nu)
post_mean_hnorm_05_1000 <- post_nu_hnorm_05_1000 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_hnorm_05_1000)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Half Normal(0,5) prior"
        ),
        subtitle = "n = 1000"
    )

#### traceplots:

mcmc_plot(hnorm_post_05_1000$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 1000"
    )
mcmc_plot(hnorm_post_05_1000$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 1000"
    )
mcmc_plot(hnorm_post_05_1000$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 1000"
    )
mcmc_plot(hnorm_post_05_1000$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 1000"
    )
mcmc_plot(hnorm_post_05_1000$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 1000"
    )
mcmc_plot(hnorm_post_05_1000$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 1000"
    )

#### Fit Summaries

post_summaries_hnorm_05_1000

## n = 10000

### reading in posteriors
hnorm_post_05_10000 <- readRDS("Posteriors/Half_Normal_0_5_10000.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_hnorm_05_10000 <- hnorm_post_05_10000 |> map(\(x) summary(x))
post_nu_hnorm_05_10000 <- hnorm_post_05_10000 |> map(\(x) as_draws_df(x)$nu)
post_mean_hnorm_05_10000 <- post_nu_hnorm_05_10000 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_hnorm_05_10000)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Half Normal(0,5) prior"
        ),
        subtitle = "n = 10000"
    )

#### traceplots:

mcmc_plot(hnorm_post_05_10000$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 10000"
    )
mcmc_plot(hnorm_post_05_10000$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 10000"
    )
mcmc_plot(hnorm_post_05_10000$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 10000"
    )
mcmc_plot(hnorm_post_05_10000$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 10000"
    )
mcmc_plot(hnorm_post_05_10000$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 10000"
    )
mcmc_plot(hnorm_post_05_10000$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Half Normal(0,5) Prior | Sample Size 10000"
    )

#### Fit Summaries

post_summaries_hnorm_05_10000

#------------------------ Half Normal(0,10) --------------------------

## n = 100

### reading in posteriors
hnorm_post_010_100 <- readRDS("Posteriors/Half_Normal_0_10_100.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_hnorm_010_100 <- hnorm_post_010_100 |> map(\(x) summary(x))
post_nu_hnorm_010_100 <- hnorm_post_010_100 |> map(\(x) as_draws_df(x)$nu)
post_mean_hnorm_010_100 <- post_nu_hnorm_010_100 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_hnorm_010_100)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Half Normal(0,10) prior"
        ),
        subtitle = "n = 100"
    )

#### traceplots:

mcmc_plot(hnorm_post_010_100$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 100"
    )
mcmc_plot(hnorm_post_010_100$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 100"
    )
mcmc_plot(hnorm_post_010_100$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 100"
    )
mcmc_plot(hnorm_post_010_100$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 100"
    )
mcmc_plot(hnorm_post_010_100$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 100"
    )
mcmc_plot(hnorm_post_010_100$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 100"
    )

#### Fit Summaries

post_summaries_hnorm_010_100

## n = 1000

### reading in posteriors
hnorm_post_010_1000 <- readRDS("Posteriors/Half_Normal_0_10_1000.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_hnorm_010_1000 <- hnorm_post_010_1000 |> map(\(x) summary(x))
post_nu_hnorm_010_1000 <- hnorm_post_010_1000 |> map(\(x) as_draws_df(x)$nu)
post_mean_hnorm_010_1000 <- post_nu_hnorm_010_1000 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_hnorm_010_1000)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Half Normal(0,10) prior"
        ),
        subtitle = "n = 1000"
    )

#### traceplots:

mcmc_plot(hnorm_post_010_1000$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 1000"
    )
mcmc_plot(hnorm_post_010_1000$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 1000"
    )
mcmc_plot(hnorm_post_010_1000$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 1000"
    )
mcmc_plot(hnorm_post_010_1000$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 1000"
    )
mcmc_plot(hnorm_post_010_1000$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 1000"
    )
mcmc_plot(hnorm_post_010_1000$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 1000"
    )

#### Fit Summaries

post_summaries_hnorm_010_1000

## n = 10000

### reading in posteriors
hnorm_post_010_10000 <- readRDS("Posteriors/Half_Normal_0_10_10000.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_hnorm_010_10000 <- hnorm_post_010_10000 |> map(\(x) summary(x))
post_nu_hnorm_010_10000 <- hnorm_post_010_10000 |> map(\(x) as_draws_df(x)$nu)
post_mean_hnorm_010_10000 <- post_nu_hnorm_010_10000 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_hnorm_010_10000)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Half Normal(0,10) prior"
        ),
        subtitle = "n = 10000"
    )

#### traceplots:

mcmc_plot(hnorm_post_010_10000$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 10000"
    )
mcmc_plot(hnorm_post_010_10000$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 10000"
    )
mcmc_plot(hnorm_post_010_10000$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 10000"
    )
mcmc_plot(hnorm_post_010_10000$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 10000"
    )
mcmc_plot(hnorm_post_010_10000$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 10000"
    )
mcmc_plot(hnorm_post_010_10000$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Half Normal(0,10) Prior | Sample Size 10000"
    )

#### Fit Summaries

post_summaries_hnorm_010_10000

#--------------------------- Half Cauchy(0,5) ---------------------------

## n = 100

### reading in posteriors
hcauchy_post_100 <- readRDS("Posteriors/Half_Cauchy_0_5_100.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_hcauchy_100 <- hcauchy_post_100 |> map(\(x) summary(x))
post_nu_hcauchy_100 <- hcauchy_post_100 |> map(\(x) as_draws_df(x)$nu)
post_mean_hcauchy_100 <- post_nu_hcauchy_100 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_hcauchy_100)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Half Cauchy(0,5) prior"
        ),
        subtitle = "n = 100"
    )

#### traceplots:

mcmc_plot(hcauchy_post_100$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 100"
    )
mcmc_plot(hcauchy_post_100$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 100"
    )
mcmc_plot(hcauchy_post_100$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 100"
    )
mcmc_plot(hcauchy_post_100$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 100"
    )
mcmc_plot(hcauchy_post_100$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 100"
    )
mcmc_plot(hcauchy_post_100$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 100"
    )

#### Fit Summaries

post_summaries_hcauchy_100

## n = 1000

### reading in posteriors
hcauchy_post_1000 <- readRDS("Posteriors/Half_Cauchy_0_5_1000.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_hcauchy_1000 <- hcauchy_post_1000 |> map(\(x) summary(x))
post_nu_hcauchy_1000 <- hcauchy_post_1000 |> map(\(x) as_draws_df(x)$nu)
post_mean_hcauchy_1000 <- post_nu_hcauchy_1000 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_hcauchy_1000)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Half Cauchy(0,5) prior"
        ),
        subtitle = "n = 1000"
    )

#### traceplots:

mcmc_plot(hcauchy_post_1000$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 1000"
    )
mcmc_plot(hcauchy_post_1000$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 1000"
    )
mcmc_plot(hcauchy_post_1000$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 1000"
    )
mcmc_plot(hcauchy_post_1000$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 1000"
    )
mcmc_plot(hcauchy_post_1000$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 1000"
    )
mcmc_plot(hcauchy_post_1000$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 1000"
    )

#### Fit Summaries

post_summaries_hcauchy_1000

## n = 10000

### reading in posteriors
hcauchy_post_10000 <- readRDS("Posteriors/Half_Cauchy_0_5_10000.rds")

### Getting Posterior Distributions/Traceplots/Summaries

post_summaries_hcauchy_10000 <- hcauchy_post_10000 |> map(\(x) summary(x))
post_nu_hcauchy_10000 <- hcauchy_post_10000 |> map(\(x) as_draws_df(x)$nu)
post_mean_hcauchy_10000 <- post_nu_hcauchy_10000 |> map(mean)

#### Posterior histograms
df <- stack(post_nu_hcauchy_10000)
colnames(df) <- c("post", "nu")

summaries <- df |>
    dplyr::group_by(nu) |>
    dplyr::summarise(
        post_mean = mean(post),
        ci_lower = quantile(post, 0.025),
        ci_upper = quantile(post, 0.975),
        true_mean = as.numeric(sub("nu_", "", unique(nu))),
        .groups = "drop"
    )

df |>
    ggplot(aes(x = post, fill = nu)) +
    geom_histogram(bins = 100, color = "#81818160") +
    facet_wrap(
        ~nu,
        scales = "free_x",
        labeller = as_labeller(
            function(x) paste0("nu == ", sub("nu_", "", x)),
            label_parsed
        )
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = post_mean),
        linetype = "dashed",
        color = "red"
    ) +

    geom_vline(
        data = lines,
        aes(xintercept = true_mean),
        linetype = "solid",
        color = "blue"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_lower),
        linetype = "dotdash"
    ) +
    geom_vline(
        data = summaries,
        aes(xintercept = ci_upper),
        linetype = "dotdash"
    ) +
    scale_x_continuous() +

    theme(legend.position = "none", strip.text = element_text()) +
    labs(
        x = "",
        y = "",
        title = expression(
            "Posterior Distributions of " ~ nu ~
                " with a Half Cauchy(0,5) prior"
        ),
        subtitle = "n = 10000"
    )

#### traceplots:

mcmc_plot(hcauchy_post_10000$nu_5, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 5")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 10000"
    )
mcmc_plot(hcauchy_post_10000$nu_6, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 6")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 10000"
    )
mcmc_plot(hcauchy_post_10000$nu_7, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 7")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 10000"
    )
mcmc_plot(hcauchy_post_10000$nu_15, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 15")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 10000"
    )
mcmc_plot(hcauchy_post_10000$nu_17, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 17")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 10000"
    )
mcmc_plot(hcauchy_post_10000$nu_20, variable = "nu", type = "trace") +
    labs(
        title = expression(paste("Traceplot of MCMC Chains for ", nu, " = 20")),
        subtitle = "Half Cauchy(0,5) Prior | Sample Size 10000"
    )

#### Fit Summaries

post_summaries_hcauchy_10000
