compute_modularity <- function(graph) {
  walktrap <- cluster_walktrap(graph)
  modularity(walktrap)
}

compute_distance <- function(graph) {
  dist <- distances(graph)
  dist[upper.tri(dist)]
}

compute_adjusted_mutual_information <- function(x, y) {
  xclust <- x |> cluster_walktrap()
  yclust <- y |> cluster_walktrap()
  aricode::AMI(xclust$membership, yclust$membership)
}

summary_hub_score_square_error <- function(x, y, fun = mean) {
  fun((hub_score(x, scale = TRUE)$vector - hub_score(y, scale = TRUE)$vector)^2)
}
compute_mean_error_hub_score <- function(x, y) {
  summary_hub_score_square_error(x, y, mean)
}
