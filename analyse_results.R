source("common.R")

library(ggplot2)

plot_cost <- function(res)
{
  ggplot(res, aes(x=tuner, y=cost)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + geom_jitter(width = 0.2, alpha=0.75)
}

results <- readRDS("test_results.rds")

tabcost <- rbindlist(lapply(results, `[`, j = c("scenario", "tuner", "rep", "cost", "instance"), with = FALSE))
tabcost[, rank_cost := rank(cost), by= list(scenario, instance)]
tabsum <- tabcost[, list(cost_mean = mean(cost), cost_sd = sd(cost),
                         cost_mean_rank = mean(rank_cost), cost_sd_rank = sd(rank_cost)),
                  by=list(scenario, tuner)]

plot_cost(results[["acotsp-tsp-rue-2000"]])

ggplot(dcast(res, scenario + instance + rep ~ tuner, value.var = "cost"),
       aes(x = irace_3.5, y = irace_git)) + geom_point() + geom_abline(slope = 1, intercept = 0)


