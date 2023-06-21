source("common.R")

res <- readRDS("test_results.rds")
res[, list(mean_cost=mean(cost)), by= list(scenario, tuner)]

res[, list(rank_cost=rank(cost)), by= list(scenario, tuner, instance)][, list(mean_rank = mean(rank_cost)), by=list(scenario,tuner)]

library(ggplot2)

ggplot(res, aes(x=tuner, y=cost)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + geom_jitter(width = 0.2, alpha=0.75)

ggplot(dcast(res, scenario + instance + rep ~ tuner, value.var = "cost"),
       aes(x = irace_3.5, y = irace_git)) + geom_point() + geom_abline(slope = 1, intercept = 0)


