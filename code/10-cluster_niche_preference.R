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

run <- function(seed) {
  set.seed(seed)
  # Define the sample sizes we are going to use
  sample_sizes <- c(25, 50, 75, 100, 150)
  gen <- bdgraph.sim(
    p = 100, graph = "cluster", n = 150, type = "nbinom"
  )

  t_graph_counts <- graph_from_adjacency_matrix((gen$K < 0) * gen$G, mode = "undirected")
  depth_factor <- runif(1000, 0.75, 1.25)
  counts <- as.data.frame(round(gen$data * depth_factor) + 1)

  res_negb <- expand_grid(
    seed = seed, graph_type = "cluster",
    true_graph = list(t_graph_counts),
    n = sample_sizes, gen = "nbinom"
  )



  res_negb$pulsar_graph <- res_negb$n %>%
    map(\(n) slice_sample(counts, n = n) %>%
      clr() %>%
      train_pulsar_niche())

  res_negb$bdgraph_graph <- res_negb$n %>%
    map(\(n) slice_sample(counts, n = n) %>%
      clr() %>%
      train_bdgraph_niche())

  res_negb
}

expand_grid(
  seed = 910:915
) %>%
  pwalk(.progress = TRUE, \(seed) {
    outfile <- paste0("steps/niche_preference/cluster", "_", seed, ".Rds")
    print(outfile)
    if (!file.exists(outfile)) {
      run(seed) %>% write_rds(outfile)
    }
  })
