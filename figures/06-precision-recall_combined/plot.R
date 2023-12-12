source("renv/activate.R")
library(tidyverse)
library(igraph)
source("figures/theme.R")

infiles <- list.files("steps/fits_combined/", ".Rds", full.names = TRUE)
names(infiles) <- basename(infiles) %>% str_remove(".Rds")
data <- map(infiles, read_rds) %>%
  bind_rows() %>%
  rowwise() %>%
  mutate(
    inferred = BDgraph::select(bdgraph, cut = 0.5) %>% 
      graph_from_adjacency_matrix(mode = "undirected") %>%
      list(),
    tp = intersection(true_graph, inferred) %>% E() %>% length(),
    fp = E(true_graph %m% inferred) %>% length(),
    fn = E(inferred %m% true_graph) %>% length(),
    f1 = tp / (tp + 0.5 * (fp + fn))
  )

data %>%
  ggplot(aes(x = n, y = f1)) +
  geom_boxplot(aes(group = n)) +
  facet_wrap(~graph_type) +
  theme_classic() +
  xlab("Number of samples") +
  ylab("F1 score") +
  labs(color = "") +
  theme(legend.position = "bottom")

ggsave(
  "figures/06-precision-recall_combined/f1-counts.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height
)

data %>%
  mutate(
    precision = tp / (tp + fp),
    recall = tp / (tp + fn),
  ) %>%
  ggplot(aes(x = precision, y = recall, colour = n)) +
  geom_point() +
  geom_abline() +
  scale_colour_viridis_c(option = "magma", alpha = 0.8) +
  theme_classic() +
  xlab("Precision") +
  ylab("Recall") +
  labs(color = "Number of samples") +
  theme(legend.position = "bottom")

ggsave(
  "figures/06-precision-recall_combined/prec_recall-counts.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height
)