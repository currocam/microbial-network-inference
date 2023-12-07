bdgraph2igraph <- function(g) {
  p <- 100
  vec_g <- c(rep(0, p * (p - 1) / 2))
  vec_g[which(unlist(strsplit(as.character(g), "")) == 1)] <- 1
  m <- matrix(0, p, p)
  m[upper.tri(m)] <- vec_g
  graph_from_adjacency_matrix(m, mode = "undirected")
}

train_bdgraph <- function(data) {
  data <- as.matrix(data)
  fit <- bdgraph(
    data = data,
    method = "ggm",
    iter = 10000,
    cores = 8
  )
  print(dim(data))
  fit %>%
    BDgraph::select(cut = 0.5) %>%
    graph_from_adjacency_matrix(mode = "undirected")
}

train_pulsar <- function(data) {
  data <- as.matrix(data)
  max_lamda <- getMaxCov(data)
  path <- getLamPath(max_lamda, max_lamda * 0.001, len = 50)
  hugeargs <- list(lambda = path, verbose = FALSE)
  fit <- pulsar(
    data,
    fun = huge::huge, fargs = hugeargs,
    rep.num = 20, criterion = "stars", thresh = 0.05,
    lb.stars = TRUE, ub.stars = TRUE, ncores = 8
  )
  print(dim(data))
  fit$est$path[[fit$stars$opt.index]] |>
    graph_from_adjacency_matrix(mode = "undirected")
}
