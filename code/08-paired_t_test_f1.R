source("renv/activate.R")
library(tidyverse)
library(igraph)
library(lmtest)
source("figures/theme.R")

infiles <- list.files("steps/fits/", ".Rds", full.names = TRUE)
names(infiles) <- basename(infiles) %>% str_remove(".Rds")
data <- map(infiles, read_rds) %>%
  bind_rows() %>%
  filter(gen != "nbinom_shr") %>%
  rowwise() %>%
  mutate(
    tp_pulsar = intersection(true_graph, pulsar_graph) %>% E() %>% length(),
    fp_pulsar = E(true_graph %m% pulsar_graph) %>% length(),
    fn_pulsar = E(pulsar_graph %m% true_graph) %>% length(),
    f1_pulsar = tp_pulsar / (tp_pulsar + 0.5 * (fp_pulsar + fn_pulsar)),
    tp_bdgraph = intersection(true_graph, bdgraph_graph) %>% E() %>% length(),
    fp_bdgraph = E(true_graph %m% bdgraph_graph) %>% length(),
    fn_bdgraph = E(bdgraph_graph %m% true_graph) %>% length(),
    f1_bdgraph = tp_bdgraph / (tp_bdgraph + 0.5 * (fp_bdgraph + fn_bdgraph)),
    graph_type = str_to_title(graph_type),
    max_degree = true_graph |> degree() |> max(),
    difference_f1 = f1_pulsar - f1_bdgraph
  )

# t-test for difference in F1 score
test <- function(x) {
  t.test(x$f1_bdgraph, x$f1_pulsar, paired = TRUE, alternative = "two.sided", var.equal = FALSE) |>
    broom::tidy()
}
list(
  "normal" = data %>% filter(gen == "normal") %>% test(),
  "nbinom" = data %>% filter(gen == "nbinom") %>% test()
) |>
  bind_rows(.id = "gen") |>
  mutate(method = str_to_title(gen)) |>
  write_csv("steps/paired_test_f1.csv")
