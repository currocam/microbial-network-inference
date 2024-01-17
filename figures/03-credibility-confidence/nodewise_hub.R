source("renv/activate.R")
library(tidyverse)
library(BDgraph)
library(igraph)
library(ggsci)
source("figures/theme.R")

data <- read_rds("steps/node_wise_intervals/random.Rds")
true_row <- subset(data, method == "true") |> pull("hub") |> pluck(1)
data <- subset(data, method != "true")
res <- tibble(.rows = 0)
data |>
  group_split(method, n) |>
  walk(\(rows){
    hubs <- reduce(rows$hub, rbind)
    res <<- tibble(
      method = rows$method[[1]],
      n = rows$n[[1]],
      true_values = true_row,
      nodes = 1:100,
      median = apply(hubs, 2, median),
      `2.5%` = apply(hubs, 2, quantile, 0.025),
      `97.5%` = apply(hubs, 2, quantile, 0.975)
    ) |>
    bind_rows(res)
  }) 

set.seed(400)
data_plot <- filter(res, nodes %in% sample(1:100, size = 30))

data_plot |>
  filter(n == 50) |>
  mutate(nodes = as.character(nodes)) |>
  ggplot() +
  theme_bw() +
  geom_errorbar(
    aes(
      x = nodes, y = median, ymin = `2.5%`,
      ymax = `97.5%`, colour = method
    ),
    alpha = 0.9, position = position_dodge2(width = 0.1), width = 0.8
  ) +
  geom_point(aes(x = nodes, y = true_values), size = 1.5) +
  xlab("Node") +
  ylab("Hub-score") +
  scale_color_nejm(labels = c("95% credibility interval (Bayesian)", "95% confidence interval (SpiecEASI)")) +
  labs(color = "") +
  ylim(0, 1) +
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "")+
  theme(legend.position = "bottom")

ggsave(
  "figures/03-credibility-confidence/hub_nodewise.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

infiles <- list.files("steps/node_wise_intervals/", full.names = TRUE)
res <- tibble(.rows = 0)
walk(infiles, \(file){
  data <- read_rds(file)
  true_row <- subset(data, method == "true") |> pull("hub") |> pluck(1)
  data <- subset(data, method != "true")
  data |>
    group_split(method, n) |>
    walk(\(rows){
      hubs <- reduce(rows$hub, rbind)
      res <<- tibble(
        file = file,
        method = rows$method[[1]],
        n = rows$n[[1]],
        true_values = true_row,
        nodes = 1:100,
        median = apply(hubs, 2, median),
        `2.5%` = apply(hubs, 2, quantile, 0.025),
        `97.5%` = apply(hubs, 2, quantile, 0.975)
      ) |>
      bind_rows(res)
    }) 
})

res |>
  rowwise() |>
  mutate(
    is_contained = true_values > `2.5%` & true_values < `97.5%`
  ) |>
  group_by(method, n, file) |>
  summarise(
    number = n(),
    fraction = sum(is_contained)/number,
  ) |>
  ggplot(
    aes(x = n, y = fraction, colour = method)
  )+
  theme_bw() +
  geom_boxplot(aes(group = interaction(n, method)))+
  xlab("Sample size") +
  ylab("Fraction of nodes with hub-score within interval range") +
  scale_color_nejm(labels = c("95% credibility interval (Bayesian)", "95% confidence interval (SpiecEASI)")) +
  labs(color = "") +
  ylim(0, 1) +
  theme(legend.position = "bottom")

ggsave(
  "figures/03-credibility-confidence/hub_nodewise_contain.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

res |>
  group_by(method, n, file) |>
  summarise(
    number = n(),
    size = mean(`97.5%` - `2.5%`)
  ) |>
  ggplot(
    aes(x = n, y = size, colour = method)
  )+
  theme_bw() +
  geom_boxplot(aes(group = interaction(n, method)))+
  xlab("Sample size") +
  ylab("95% interval range") +
  scale_color_nejm(labels = c("95% credibility interval (Bayesian)", "95% confidence interval (SpiecEASI)")) +
  labs(color = "") +
  ylim(0, 1) +
  theme(legend.position = "bottom")

ggsave(
  "figures/03-credibility-confidence/hub_nodewise_size.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)


res |>
  rowwise() |>
  mutate(
    is_contained = true_values > `2.5%` & true_values < `97.5%`
  ) |>
  group_by(method, n, file) |>
  summarise(
    number = n(),
    size = mean(`97.5%` - `2.5%`),
    fraction = sum(is_contained)/number,
    efficiency = fraction/size
  ) |>
  ggplot(
    aes(x = n, y = efficiency, colour = method)
  )+
  theme_bw() +
  geom_boxplot(aes(group = interaction(n, method)))
