source("renv/activate.R")
library(tidyverse)
library(igraph)
source("figures/theme.R")

source("src/network_metrics.R")
source("src/train.R")

infiles <- list.files("steps/fits_combined/", ".Rds", full.names = TRUE)
names(infiles) <- basename(infiles) |> str_remove(".Rds")

data <- map(infiles, read_rds) |>
  bind_rows(.id = "file") |>
  separate(file, c("graph", "seed"), remove = FALSE) |>
  rowwise() |>
  mutate(posterior = list(sample_from_posterior(bdgraph)))

true_distances <- data$true_graph |> map(compute_distance)
spearmans <- map2(true_distances, data$posterior, \(true, x){
  map(x, ~cor(true, compute_distance(.x), method = "spearman")) |> as.numeric()
})


data |>
  ungroup() |>
  mutate(
    `50%` = map(spearmans, quantile, 0.5) |> as.numeric(),
    `2.5%` = map(spearmans, quantile, 0.025) |> as.numeric(),
    `97.5%` = map(spearmans, quantile, 0.975) |> as.numeric()
    ) |>
  mutate(n = as.factor(n)) |>
  arrange(`50%`) |>
  ggplot() +
  geom_pointrange(
    aes(
      x = n, y = `50%`, ymin = `2.5%`,
      ymax = `97.5%`
    ),
    position = position_dodge2(width = 0.7),
    alpha = 0.9
  ) +
  xlab("Sample size") +
  ylab("Spearman correlation distances")

ggsave(
  "figures/08-credibility-confidence_combined/distances_spearman.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

true_modularity <- data$true_graph |> map(compute_modularity)
modularities <- map2(true_modularity, data$posterior, \(true, x){
  map(x, ~true - compute_modularity(.x)) |> as.numeric()
}) 

data |>
  ungroup() |>
  mutate(
    `50%` = map(modularities, quantile, 0.5) |> as.numeric(),
    `2.5%` = map(modularities, quantile, 0.025) |> as.numeric(),
    `97.5%` = map(modularities, quantile, 0.975) |> as.numeric(),
    n = as.factor(n),
  ) |>
  arrange(`50%`) |>
  ggplot() +
  geom_pointrange(
    aes(
      x = n, y = `50%`, ymin = `2.5%`,
      ymax = `97.5%`
    ),
    position = position_dodge2(width = 0.7),
    alpha = 0.9
  ) +
  xlab("Sample size") +
  ylab("Modularity error")

ggsave(
  "figures/08-credibility-confidence_combined/modularity.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

hub_error <- map2(data$true_graph, data$posterior, \(true, x){
  map(x, ~compute_mean_error_hub_score(true, .x)) |> as.numeric()
})

data |>
  ungroup() |>
  mutate(
    `50%` = map(hub_error, quantile, 0.5) |> as.numeric(),
    `2.5%` = map(hub_error, quantile, 0.025) |> as.numeric(),
    `97.5%` = map(hub_error, quantile, 0.975) |> as.numeric(),
    n = as.factor(n)
    ) |>
  arrange(`50%`) |>
  ggplot() +
  geom_pointrange(
    aes(
      x = n, y = `50%`, ymin = `2.5%`,
      ymax = `97.5%`
    ),
    position = position_dodge2(width = 0.7),
    alpha = 0.9
  ) +
  xlab("Sample size") +
  ylab("Hub error")

ggsave(
  "figures/08-credibility-confidence_combined/hub_score.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

data |>
  filter(metric == "ami", graph == "cluster") |>
  mutate(n = as.factor(n)) |>
  arrange(`50%`) |>
  ggplot() +
  geom_pointrange(
    aes(
      x = n, y = `50%`, ymin = `2.5%`,
      ymax = `97.5%`, colour = method
    ),
    position = position_dodge2(width = 0.7),
    alpha = 0.9
  ) +
  xlab("Sample size") +
  ylab("Adjusted mutual information criterion")

ggsave(
  "figures/03-credibility-confidence/cluster_ami.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)
