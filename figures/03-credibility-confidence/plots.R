source("renv/activate.R")
library(tidyverse)
library(igraph)
source("figures/theme.R")

infiles <- list.files("steps/credibility_confidence_intervals/", ".Rds", full.names = TRUE)
names(infiles) <- basename(infiles) |> str_remove(".Rds")
data <- map(infiles, read_rds) |>
  bind_rows(.id = "file") |>
  separate(file, c("graph", "seed"), remove = FALSE)

data |>
  filter(metric == "spearman", graph == "random") |>
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
  ylab("Spearman correlation distances")

ggsave(
  "figures/03-credibility-confidence/distances_spearman.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)


data |>
  filter(metric == "modularity_error", graph == "random") |>
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
  ylab("Modularity error")

ggsave(
  "figures/03-credibility-confidence/modularity.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

data |>
  filter(metric == "hub_error", graph == "random") |>
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
  ylab("Hub error")

ggsave(
  "figures/03-credibility-confidence/hub_score.pdf",
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
