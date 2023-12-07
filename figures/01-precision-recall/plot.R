source("renv/activate.R")
library(tidyverse)
library(igraph)
source("figures/theme.R")

infiles <- list.files("steps/fits/", ".Rds", full.names = TRUE)
names(infiles) <- basename(infiles) %>% str_remove(".Rds")
data <- map(infiles, read_rds) %>%
  bind_rows() %>%
  pivot_longer(
    cols = c("bdgraph_graph", "pulsar_graph"),
    names_to = "method", values_to = "inferred"
  ) %>%
  rowwise() %>%
  filter(gen != "nbinom_shr") %>%
  mutate(
    tp = intersection(true_graph, inferred) %>% E() %>% length(),
    fp = E(true_graph %m% inferred) %>% length(),
    fn = E(inferred %m% true_graph) %>% length(),
    f1 = tp / (tp + 0.5 * (fp + fn))
  )

data %>%
  filter(gen == "normal") %>%
  ggplot(aes(x = n, y = f1, colour = method)) +
  geom_boxplot(aes(group = paste0(n, method))) +
  facet_wrap(~graph_type) +
  scale_colour_discrete(labels = c("Bayesian", "SpiecEASI")) +
  theme_classic() +
  xlab("Number of samples") +
  ylab("F1 score") +
  labs(color = "") +
  theme(legend.position = "bottom")

ggsave(
  "figures/01-precision-recall/f1-normal.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height
)

data %>%
  filter(gen == "nbinom") %>%
  ggplot(aes(x = n, y = f1, colour = method)) +
  geom_boxplot(aes(group = paste0(n, method))) +
  facet_wrap(~graph_type) +
  scale_colour_discrete(labels = c("Bayesian", "SpiecEASI")) +
  theme_classic() +
  xlab("Number of samples") +
  ylab("F1 score") +
  labs(color = "") +
  theme(legend.position = "bottom")

ggsave(
  "figures/01-precision-recall/f1-counts.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height
)

data %>%
  mutate(
    precision = tp / (tp + fp),
    recall = tp / (tp + fn),
    method = if_else(
      method == "pulsar_graph",
      "SpiecEASI", "Bayesian"
    )
  ) %>%
  filter(gen == "normal", graph_type == "random") %>%
  ggplot(aes(x = precision, y = recall, colour = n)) +
  geom_point() +
  geom_abline() +
  facet_wrap(~method) +
  scale_colour_viridis_c(option = "magma", alpha = 0.8) +
  theme_classic() +
  xlab("Precision") +
  ylab("Recall") +
  labs(color = "Number of samples") +
  theme(legend.position = "bottom")

ggsave(
  "figures/01-precision-recall/prec_recall-normal.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height
)

data %>%
  mutate(
    precision = tp / (tp + fp),
    recall = tp / (tp + fn),
    method = if_else(
      method == "pulsar_graph",
      "SpiecEASI", "Bayesian"
    )
  ) %>%
  filter(gen == "nbinom", graph_type == "random") %>%
  ggplot(aes(x = precision, y = recall, colour = n)) +
  geom_point() +
  geom_abline() +
  facet_wrap(~method) +
  scale_colour_viridis_c(option = "magma", alpha = 0.8) +
  theme_classic() +
  xlab("Precision") +
  ylab("Recall") +
  labs(color = "Number of samples") +
  theme(legend.position = "bottom")

ggsave(
  "figures/01-precision-recall/prec_recall-counts.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height
)
