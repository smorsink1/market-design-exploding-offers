
## For the paper: 
## The Persistence of Exploding Offers in Job Matching Programs:
## Theory and Evidence from the IMF's Economist Program

## Outline
## 1. Setup
## 2. Investigating Distributions (from which to draw EP utilities)
## 3. Simulating preference data
## 4. Running DAA, with and without exploding offers

#### 1. Setup #### 

set.seed(136)
library(dplyr)
library(purrr)

#### 2. Investigating Distributions ####

# We will sample payoffs from a beta distribution (which is always between 0 and 1)
# We want situations in which payoffs from top choices are close & others where they are far
# beta(a = 1, b = 1) is uniform distribution - payoffs evenly distributed
# beta(a = 2, b = 2) has payoffs clustered near the middle
# beta(a = 3, b = 1) has payoffs clustered near the top
# beta(a = 1, b = 3) has payoffs clustered near the bottom

# investigating order statistics

getBetaSD <- function(a, b) {
  num <- a * b
  denom <- ((a + b)^2) * (a + b + 1)
  return (sqrt(num / denom))
}

getNth <- function(x, n, diff = FALSE) {
  # returns nth-highest element of x
  # if diff = TRUE, return difference between nth and (n-1)th highest elements of x
  x_ordered <- x[order(x, decreasing = TRUE)]
  if (diff) {
    return (x_ordered[n] - x_ordered[n - 1])
  } else {
    return (x_ordered[n])
  }
}

sample10 <- function(a, b) rbeta(n = 10, shape1 = a, shape2 = b)

getSimOrderStatReport <- function(a, b, n_samples = 10000) {
  # simulates many draws of 10 from beta(a, b) (for given a,b)
  # reports the average nth highest element (n from 1:10)
  # and the average difference from nth to (n-1)th highest element
  many_samples <- replicate(10000, sample10(a, b))
  n1 <- apply(many_samples, 2, getNth, n = 1)
  n2 <- apply(many_samples, 2, getNth, n = 2)
  n3 <- apply(many_samples, 2, getNth, n = 3)
  n4 <- apply(many_samples, 2, getNth, n = 4)
  n5 <- apply(many_samples, 2, getNth, n = 5)
  n6 <- apply(many_samples, 2, getNth, n = 6)
  n7 <- apply(many_samples, 2, getNth, n = 7)
  n8 <- apply(many_samples, 2, getNth, n = 8)
  n9 <- apply(many_samples, 2, getNth, n = 9)
  n10 <- apply(many_samples, 2, getNth, n = 10)
  report <- data.frame("a" = a,
                       "b" = b,
                       "n_sims" = n_samples,
                       "mean_1" = mean(n1),
                       "mean_2" = mean(n2),
                       "mean_3" = mean(n3),
                       "mean_4" = mean(n4),
                       "mean_5" = mean(n5),
                       "mean_6" = mean(n6),
                       "mean_7" = mean(n7),
                       "mean_8" = mean(n8),
                       "mean_9" = mean(n9),
                       "mean_10" = mean(n10),
                       "diff_1" = mean(n1 - n2),
                       "diff_2" = mean(n2 - n3),
                       "diff_3" = mean(n3 - n4),
                       "diff_4" = mean(n4 - n5),
                       "diff_5" = mean(n5 - n6),
                       "diff_6" = mean(n6 - n7),
                       "diff_7" = mean(n7 - n8),
                       "diff_8" = mean(n8 - n9),
                       "diff_9" = mean(n9 - n10))
  return (report)
}

unif <- getSimOrderStatReport(a = 1, b = 1, n_samples = 100000)
middle <- getSimOrderStatReport(a = 2, b = 2, n_samples = 100000)
top <- getSimOrderStatReport(a = 3, b = 1, n_samples = 100000)
bottom <- getSimOrderStatReport(a = 1, b = 3, n_samples = 100000)

all_report <- dplyr::bind_rows(unif, middle, top, bottom)
# name refers to where payoffs are close together 

#### 3. Simulating Preference Data ####

## Simplified setting: 40 EPs, 10 Depts (each with 4 openings)
## EPs have...
##   payoffs from being matched with each department
##   beliefs about how likely it is that they're matched with each department
##   some cost of rejecting an exploding offer (fixed across all EPs)
##   --> maybe leave this out?
## Departments have...
##   preferences over EPs
##   an "enthuasiasm type"
##     type 1: always prefer someone who wants to work for you
##     type 2: you don't care about how enthusiastic the employees are

getEP <- function(id, beta_a, beta_b, n_depts) {
  # given an id and parameters of a beta distribution from which to draw preferences,
  # returns preferences for an EP
  payoff_from_dept <- rbeta(n = n_depts, shape1 = beta_a, shape2 = beta_b)
  payoff_from_dept <- runif(n = n_depts, min = 0, max = 1)
  dept_rank <- rank(-payoff_from_dept)
  ep_df <- data.frame("ep" = id, 
                      "dept" = 1:n_depts,
                      "payoff_to_ep" = payoff_from_dept,
                      "rank_by_ep" = dept_rank)
  return (ep_df)
}

getEPs <- function(beta_a, beta_b, n_eps, n_depts) {
  # creates n_eps EPs with preferences drawn from specified beta distribution
  eps <- purrr::map_dfr(1:n_eps, getEP, beta_a = beta_a, beta_b = beta_b, n_depts = n_depts)
  return (eps)
}

getDept <- function(id, n_eps) {
  # given an id and number of eps,
  # returns preferences for a department over eps
  dept_df <- data.frame("dept" = id,
                        "ep" = 1:n_eps,
                        "rank_by_dept" = sample(1:n_eps, size = n_eps, replace = FALSE),
                        "dept_type" = 0)
  return (dept_df)
}

getDepts <- function(type_prob, n_eps, n_depts) {
  # creates n_depts Departments with random preferences & enthusiasm type (in accordance with type prob)
  depts <- purrr::map_dfr(1:n_depts, getDept, n_eps = n_eps)
  n_type_1_depts <- round(type_prob * n_depts)
  type_1_depts <- sample(1:n_depts, size = n_type_1_depts)
  depts$dept_type[depts$dept %in% type_1_depts] <- 1
  
  n_openings <- rep(n_eps / n_depts, times = n_depts)
  depts$n_openings <- rep(n_openings, each = n_eps)
  
  return (depts)
}

#### 4. Running DAA ####

runDAA <- function(eps, depts) {
  # given reported preferences of EPs and departments,
  # runs EP-proposing DAA and returns the matches
  
  rejected_eps <- unique(eps$ep)
  eps_algo <- eps
  depts_algo <- depts
  depts_algo[["held"]] <- 0
  
  while (length(rejected_eps) > 0) {
    # rejected EPs make proposals
    proposal_df <- data.frame("ep" = rejected_eps,
                              "propose_to" = sapply(rejected_eps, getProposal, eps_algo_df = eps_algo),
                              "rejected" = 0)
    
    old_depts_algo_holds <- depts_algo$held
    # depts decide which proposals to hold
    depts_algo <- purrr::map_dfr(unique(depts$dept), getHolds,
                                 depts_algo_df = depts_algo, proposal_df = proposal_df)
    
    # identify rejected_eps
    change_in_hold <- depts_algo$held - old_depts_algo_holds
    held_rejected_eps <- depts_algo[which(change_in_hold == -1), c("ep", "dept")]
    
    unheld_rejected_eps <- depts_algo %>%
      dplyr::group_by(ep) %>%
      dplyr::summarize(n_holds = sum(held)) %>%
      dplyr::filter(n_holds == 0) %>%
      dplyr::pull(ep)
    proposal_df$rejected[which(proposal_df$ep %in% unheld_rejected_eps)] <- 1
    unheld_rejecteds <- dplyr::left_join(eps_algo, proposal_df, by = c("ep", "dept" = "propose_to"))
    unheld_rejected_eps <- unheld_rejecteds[which(unheld_rejecteds$rejected == 1), c("ep", "dept")]
    
    rejected_eps_df <- dplyr::bind_rows(held_rejected_eps, unheld_rejected_eps) %>%
      dplyr::mutate(rejected = 1)
    rejected_eps <- rejected_eps_df$ep
    
    eps_algo <- dplyr::left_join(eps_algo, rejected_eps_df, by = c("ep", "dept"))
    eps_algo <- eps_algo[-which(eps_algo$rejected == 1), -ncol(eps_algo)]
  }
  
  return (depts_algo[which(depts_algo$held == 1), ])
}

getProposal <- function(ep_id, eps_algo_df) {
  # given a specific ep, and their updated preferences,
  # returns the department that they will propose to 
  ep_specific <- eps_algo_df[eps_algo_df$ep == ep_id, ]
  return (ep_specific$dept[which.min(ep_specific$rank_by_ep)])
}

getHolds <- function(dept_id, depts_algo_df, proposal_df) {
  # given a specific department, information about who they're holding so far,
  #   and a dataframe of proposals in the current round,
  # returns the EPs that they hold after the current round
  
  dept_specific <- depts_algo_df[depts_algo_df$dept == dept_id, ]
  n_openings <- dept_specific$n_openings[1]
  
  dept_held_props <- dept_specific$ep[which(dept_specific$held == 1)]
  dept_new_props <- proposal_df$ep[which(proposal_df$propose_to == dept_id)]
  
  dept_props <- dept_specific[dept_specific$ep %in% c(dept_held_props, dept_new_props), ]
  dept_ranked <- dept_props[order(dept_props$rank_by_dept), ]
  
  dept_specific$held <- as.integer(dept_specific$ep %in% dept_ranked$ep[1:n_openings])
  
  return (dept_specific)
}

#### 5. Checking for Blocking Pairs ####

getPreferredMatches <- function(side, id, matching, ep_true_prefs, dept_true_prefs) {
  # given a side ("ep" or "dept") and an id (which ep/dept we are talking about),
  # a matching (which actual matches occur) and both sides' true preferences,
  # returns any matches that the actor in question would prefer to their actual match
  
  if (side == "ep") {
    true_prefs <- ep_true_prefs
    other_side <- "dept"
    rank_col <- "rank_by_ep"
  } else {
    true_prefs <- dept_true_prefs
    other_side <- "ep"
    rank_col <- "rank_by_dept"
  }
  
  true_prefs_specific <- true_prefs[which(true_prefs[[side]] == id), ]
  actual_match <- matching[[other_side]][matching[[side]] == id]
  actual_match_rank <- true_prefs_specific[[rank_col]][true_prefs_specific[[other_side]] %in% actual_match]
  preferred <- true_prefs_specific[which(true_prefs_specific[[rank_col]] < max(actual_match_rank)), ]
  return (preferred[which(!(preferred$ep %in% actual_match)), c("ep", "dept")])
}

getBlockingPairs <- function(matching, ep_true_prefs, dept_true_prefs) {
  # given a matching of EPs and departments, and each side's true preferences,
  # finds any blocking pairs (EP-department pairs that would leave their match for each other)
  
  all_ep_preferreds <- purrr::map_dfr(unique(ep_true_prefs$ep), 
                                      getPreferredMatches,
                                      side = "ep", matching = matching, 
                                      ep_true_prefs = ep_true_prefs, dept_true_prefs = dept_true_prefs)
  all_dept_preferreds <- purrr::map_dfr(unique(ep_true_prefs$dept), 
                                        getPreferredMatches,
                                        side = "dept", matching = matching, 
                                        ep_true_prefs = ep_true_prefs, dept_true_prefs = dept_true_prefs)
  
  blocking_pairs <- dplyr::intersect(all_ep_preferreds, all_dept_preferreds)
  
  return (blocking_pairs)
}

#### 6. Updating Preferences due to Dependencies ####

# dependent preferences: 

updateSpecificDeptPrefs <- function(dept_id, dept_prefs_w_enthus) {
  # helper function for updateDeptPrefs
  dept_specific <- dept_prefs_w_enthus[dept_prefs_w_enthus$dept == dept_id, ]
  if (dept_specific$dept_type[1] != 1) {
    # if they don't care about enthusiasm, then just keep same preferences
    return (dept_specific)
  } else {
    # if they do care about enthusiasm, then rank first by enthusiasm, then by initial ranking
    dept_specific_upd <- dept_specific %>%
      dplyr::arrange(desc(enthusiastic), rank_by_dept)
    dept_specific_upd$rank_by_dept <- 1:nrow(dept_specific_upd)
    return (dept_specific_upd)
  }
}

updateDeptPrefs <- function(ep_prefs, dept_prefs) {
  # given ep preferences and department preferences, 
  # updates preferences of type=1 departments to account for EP enthusiasm
  # (any enthusiastic EP jumps a non-enthusiastic EP)
  ep_prefs[["enthusiastic"]] <- as.integer(ep_prefs$rank_by_ep == 1)
  dept_prefs_w_enthus <- dept_prefs %>%
    dplyr::left_join(ep_prefs[, c("ep", "dept", "enthusiastic")], by = c("ep", "dept"))
  dept_prefs_upd <- purrr::map_dfr(unique(dept_prefs$dept), updateSpecificDeptPrefs, dept_prefs_w_enthus = dept_prefs_w_enthus)
  return (dept_prefs_upd)
}

#### 7. A Round of Exploding Offers ####

# we assume that EPs have the following beliefs over DAA matches:
# 19% match with 1st choice, 17% match with 2nd choice, ..., 1% match with 10th choice

updateReports <- function(ep_prefs, dept_prefs, accepted_e_offers) {
  # updates ep & dept reports after the e-offer round to reflect accepted e-offers
  e_offers_short <- accepted_e_offers[, c("ep", "dept")]
  e_offers_short$e_offer <- 1
  
  ep_prefs_upd <- ep_prefs %>%
    dplyr::left_join(e_offers_short, by = c("ep", "dept"))
  ep_prefs_upd$rank_by_ep[which(ep_prefs_upd$e_offer == 1)] <- 0
  
  dept_prefs_upd <- dept_prefs %>%
    dplyr::left_join(e_offers_short, by = c("ep", "dept"))
  dept_prefs_upd$rank_by_dept[which(dept_prefs_upd$e_offer == 1)] <- 0
  
  return (list("ep_prefs_upd" = ep_prefs_upd, "dept_prefs_upd" = dept_prefs_upd))
}

simExplodingOfferRound <- function(ep_prefs, dept_prefs, e_reject_cost, beta_a, beta_b) {
  # depts e-offer their top n_candidates, candidates accept if U(offeror) > E[U(DAA)]
  # returns updated reported preferences by eps and depts
  ep_beliefs <- data.frame("rank_by_ep" = 1:10,
                           "prob" = seq(0.19, 0.01, by = -0.02))
  
  cost_of_reject <- getBetaSD(beta_a, beta_b) * e_reject_cost
  
  ep_eu_daa <- eps %>%
    dplyr::left_join(ep_beliefs, by = "rank_by_ep") %>%
    dplyr::group_by(ep) %>%
    dplyr::summarize(daa_exp_u = sum(payoff_to_ep * prob) - cost_of_reject)
  
  e_offers <- dept_prefs[dept_prefs$rank_by_dept <= dept_prefs$n_openings, ] %>%
    dplyr::distinct(ep, .keep_all = TRUE) %>%
    dplyr::left_join(ep_prefs[, c("ep", "dept", "payoff_to_ep")], by = c("ep", "dept")) %>%
    dplyr::left_join(ep_eu_daa, by = "ep")
  
  accepted_e_offers <- e_offers[e_offers$payoff_to_ep > e_offers$daa_exp_u, ]
  
  return (updateReports(ep_prefs, dept_prefs, accepted_e_offers))
}

#### 8. Effects of Exploding Offers ####

simMarket <- function(sim_id, beta_a, beta_b, dept_type_prob, cost_of_reject) {
  # simulates a set of EPs and Departments, 
  # compares number of blocking pairs under DAA to 
  # number of blocking pairs under e-offer-round + DAA
  eps <- getEPs(beta_a = 1, beta_b = 1, n_eps = 40, n_depts = 10)
  depts <- getDepts(type_prob = 0.5, n_eps = 40, n_depts = 10)
  daa_output <- runDAA(eps, depts)
  dept_prefs_upd <- updateDeptPrefs(eps, depts)
  blocking_pairs <- getBlockingPairs(daa_output, eps, dept_prefs_upd)
  
  e_offer_prefs <- simExplodingOfferRound(eps, depts)
  e_offer_daa_output <- runDAA(e_offer_prefs$ep_prefs_upd, e_offer_prefs$dept_prefs_upd)
  e_offer_blocking_pairs <- getBlockingPairs(e_offer_daa_output, eps, dept_prefs_upd)
  
  return (data.frame("sim_id" = sim_id, 
                     "n_daa_blockings" = nrow(blocking_pairs),
                     "n_e_offer_blockings" = nrow(e_offer_blocking_pairs),
                     "n_shared_blockings" = nrow(dplyr::intersect(blocking_pairs, e_offer_blocking_pairs))))
}

baseline <- purrr::map_dfr(1:1000, simMarket, beta_a = 1, beta_b = 1, dept_type_prob = 0.5, cost_of_reject = 0.2)

# varying cost of rejecting an exploding offer
no_cost <- purrr::map_dfr(1:1000, simMarket, beta_a = 1, beta_b = 1, dept_type_prob = 0.5, cost_of_reject = 0)
high_cost <- purrr::map_dfr(1:1000, simMarket, beta_a = 1, beta_b = 1, dept_type_prob = 0.5, cost_of_reject = 0.5)

# varying utility differentials between employers
varied_at_top <- purrr::map_dfr(1:1000, simMarket, beta_a = 1, beta_b = 3, dept_type_prob = 0.5, cost_of_reject = 0.2)
similar_at_top <- purrr::map_dfr(1:1000, simMarket, beta_a = 3, beta_b = 1, dept_type_prob = 0.5, cost_of_reject = 0.2)

# varying employer preference dependences on employee preferences
high_interdependence <- purrr::map_dfr(1:1000, simMarket, beta_a = 1, beta_b = 1, dept_type_prob = 1, cost_of_reject = 0.2)
low_interdependence <- purrr::map_dfr(1:1000, simMarket, beta_a = 1, beta_b = 1, dept_type_prob = 0.1, cost_of_reject = 0.2)


