source("renv/activate.R")
library(tidyverse)
library(BDgraph)
library(igraph)
library(ggsci)
source("figures/theme.R")

infiles <- list.files("steps/credibility_confidence_intervals/", ".Rds", full.names = TRUE)
names(infiles) <- basename(infiles) |> str_remove(".Rds")
data <- map(infiles, read_rds) |>
  bind_rows(.id = "file") |>
  separate(file, c("graph", "seed"), remove = FALSE) |>
  filter(metric == "modularity_error", graph == "random")

data$true_mod <- data$seed |>
  map(\(seed){
    set.seed(seed)
    max_edges <- 100 * 99 / 2
  size <- round(max_edges * runif(1, 0.01, 0.1))
  gen <- bdgraph.sim(
    p = 100, graph = "random", n = 0, type = "nbinom", size = size
  )
  true_graph <- gen$G |>
    graph_from_adjacency_matrix(mode = "undirected")

  compute_modularity(true_graph)
  }) |>
  as.numeric()


data |>
  mutate(n = as.factor(n)) |>
  arrange(`50%`) |>
  ggplot() +
  theme_bw() +
  geom_pointrange(
    aes(
      x = n, y = true_mod - `50%`, ymin = true_mod - `2.5%`,
      ymax = true_mod - `97.5%`, colour = method
    ),
    position = position_dodge2(width = 0.7),
    alpha = 0.9
  ) +
  geom_point(aes(x = n, y = true_mod, colour = method), position = position_dodge2(width = 0.7), shape = 1) +
  xlab("Sample size") +
  ylab("Modularity") +
  scale_color_nejm(labels = c("95% credibility interval (Bayesian)", "95% confidence interval (SpiecEASI)")) +
  labs(color = "") +
  ylim(0, 1) +
  theme(legend.position = "bottom")

ggsave(
  "figures/03-credibility-confidence/modularity2.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)