source("renv/activate.R")
library(tidyverse)
library(igraph)
library(lmtest)
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
    f1 = tp / (tp + 0.5 * (fp + fn)),
    graph_type = str_to_title(graph_type),
    max_degree = true_graph |> degree() |> max()
  )
data |>
  ggplot(aes(x = max_degree, y = f1, colour = method)) +
  geom_point() +
  facet_wrap(~n)

test <- function(x) {
  fullmodel <- lm(f1 ~ n + max_degree + graph_type, data = x)
  reducedmodel <- lm(f1 ~ n + graph_type, data = x)
  lrtest(reducedmodel, fullmodel) |> broom::tidy()
}

list(
  "bdgraph" = data %>% filter(method == "bdgraph_graph", gen == "normal") %>% test(),
  "pulsar" = data %>% filter(method == "pulsar_graph", gen == "normal") %>% test()
) |>
  bind_rows(.id = "method") |>
  mutate(method = str_to_title(method)) |>
  write_csv("steps/correlation_node_f1.csv")
