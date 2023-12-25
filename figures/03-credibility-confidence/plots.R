source("renv/activate.R")
library(tidyverse)
library(igraph)
library(ggsci)
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
  ylab("Spearman correlation distances") +
  theme_bw() +
  scale_color_nejm(labels = c("95% credibility interval (Bayesian)", "95% confidence interval (SpiecEASI)")) +
  labs(color = "") +
  theme(legend.position = "bottom")

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
  theme_bw() +
  geom_pointrange(
    aes(
      x = n, y = `50%`, ymin = `2.5%`,
      ymax = `97.5%`, colour = method
    ),
    position = position_dodge2(width = 0.7),
    alpha = 0.9
  ) +
  xlab("Sample size") +
  ylab("Modularity error") +
  scale_color_nejm(labels = c("95% credibility interval (Bayesian)", "95% confidence interval (SpiecEASI)")) +
  labs(color = "") +
  theme(legend.position = "bottom")

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
  ylab("Hub error") +
  theme_bw() +
  scale_color_nejm(labels = c("Bayesian", "SpiecEASI", "Combined")) +
  labs(color = "") +
  theme(legend.position = "none")

ggsave(
  "figures/03-credibility-confidence/hub_score.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)
