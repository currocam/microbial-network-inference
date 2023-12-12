#! Rscript --vanilla
source("renv/activate.R")
library(huge)
library(igraph)
library(pulsar)
library(BDgraph)
library(tidyverse)
library(VGAM)
library(compositions)

source("src/train.R")

run <- function(graph, seed) {
  set.seed(seed)
  # Define the sample sizes we are going to use
  sample_sizes <- c(25, 100, 200, 400, 1000)
  # We are going to simulate graphs from 5% to 15% dense if random graph
  max_edges <- 100 * 99 / 2
  size <- case_match(
    graph,
    "random" ~ round(max_edges * runif(1, 0.01, 0.1)),
    "hub" ~ sample(1:10, size = 1),
    "cluster" ~ 0
  )
  if (size == 0) {
    size <- NULL
  }
  gen <- bdgraph.sim(
    p = 100, graph = graph, n = 1000, type = "nbinom", size = size
  )
  t_graph_counts <- gen$G %>%
    graph_from_adjacency_matrix(mode = "undirected")
  depth_factor <- runif(1000, 0.75, 1.25)
  counts <- as.data.frame(round(gen$data * depth_factor) + 1)

  res_negb <- expand_grid(
    seed = seed, graph_type = graph,
    true_graph = list(t_graph_counts),
    n = sample_sizes, gen = "nbinom"
  )
  res_negb$bdgraph <- res_negb$n %>%
    map(\(n) {
      data <- slice_sample(counts, n = n) |>
        clr() |>
        as.matrix()
      scores <- train_pulsar_confidence_scores(data)
      bdgraph(
        data = data,
        method = "ggm",
        iter = 10000,
        cores = 8,
        g.prior = as.matrix(scores),
        save = TRUE
      )
    })
  res_negb
}

expand_grid(
  type = c("random"),
  seed = 600:610
) %>%
  pwalk(.progress = TRUE, \(type, seed) {
    outfile <- paste0("steps/fits_combined/", type, "_", seed, ".Rds")
    print(outfile)
    if (!file.exists(outfile)) {
      run(type, seed) %>% write_rds(outfile)
    }
  })
