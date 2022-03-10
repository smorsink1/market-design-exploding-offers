
# 
# setwd("~/Documents/School/College/Senior Year/ECON 136/Paper Code")

experiments <- c("baseline", "high_cost", "no_cost",
                 "high_interdependence", "low_interdependence",
                 "similar_at_top", "varied_at_top")
all_files <- list.files()

loadExperiment <- function(experiment_name) {
  # loads all files for a certain experiment ("baseline", "high_cost", etc)
  experiment_files <- grep(experiment_name, all_files, value = TRUE)
  experiment_df <- purrr::map_dfr(experiment_files, readr::read_csv)
  experiment_df$sim_id <- 1:nrow(experiment_df)
  return (experiment_df)
}

experiment_dfs <- purrr::map(experiments, loadExperiment)
names(experiment_dfs) <- experiments

getExperimentStats <- function(experiment_name) {
  # returns table with summary stats for given experiment
  experiment_df <- experiment_dfs[[experiment_name]]
  data.frame("experiment_name" = experiment_name,
             "mean_n_daa_blocks" = mean(experiment_df$n_daa_blockings),
             "mean_n_eoffer_blocks" = mean(experiment_df$n_e_offer_blockings),
             "mean_n_daa_blocks_removed" = mean(experiment_df$n_daa_blockings - experiment_df$n_shared_blockings),
             "pct_helpful_eoffers" = mean(experiment_df$n_e_offer_blockings < experiment_df$n_daa_blockings),
             "mean_net_blocks_added" = mean(experiment_df$n_e_offer_blockings - experiment_df$n_daa_blockings))
}

experiment_stats <- purrr::map_dfr(experiments, getExperimentStats)
# readr::write_csv(experiment_stats, "experiment_stats.csv")

par(mfrow = c(3, 1))
hist((experiment_dfs$baseline$n_e_offer_blockings) - (experiment_dfs$baseline$n_daa_blockings),
     xlim = c(-10, 50), xlab = "Baseline", main = "Change in Blocking Pairs when Exploding Offers Allowed", breaks = 20)
hist((experiment_dfs$no_cost$n_e_offer_blockings) - (experiment_dfs$no_cost$n_daa_blockings),
     xlim = c(-10, 50), xlab = "No Rejection Cost", main = "", breaks = 10)
hist((experiment_dfs$high_cost$n_e_offer_blockings) - (experiment_dfs$high_cost$n_daa_blockings),
     xlim = c(-10, 50), xlab = "High Rejection Cost", main = "", breaks = 30)

par(mfrow = c(3, 1))
hist((experiment_dfs$baseline$n_e_offer_blockings) - (experiment_dfs$baseline$n_daa_blockings),
     xlim = c(-10, 50), xlab = "Baseline", main = "Change in Blocking Pairs when Exploding Offers Allowed", breaks = 20)
hist((experiment_dfs$low_interdependence$n_e_offer_blockings) - (experiment_dfs$low_interdependence$n_daa_blockings),
     xlim = c(-10, 50), xlab = "Low Preference Dependence", main = "", breaks = 10)
hist((experiment_dfs$high_interdependence$n_e_offer_blockings) - (experiment_dfs$high_interdependence$n_daa_blockings),
     xlim = c(-10, 50), xlab = "High Preference Dependence", main = "", breaks = 30)

