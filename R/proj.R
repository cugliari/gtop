#' Prediction conciliation by projection.
#' 
#' Uses a simple L2 projection to reconciliate hierarchical time
#' series forecasts.
#' 
#' Individual and global forecasts are reconciliated by projecting
#' the quantities over a ...

#' @param preds_indiv : K-length vector with predictions ybar_1,...,ybar_K for individual regions
#' @param pred_total  : number with prediction ybar_* for the total consumption
#' @param weights_indiv : K-length vector with weights a_1,...,a_K for individual regions
#' @param weight_total  : number with weight a_* for the total consumption
#' @examples
#' K <- 5
#' proj(preds_indiv = rep(0, K), 1, 
#'      weights_indiv = rep(1, K), 
#'      weights_total = 2,
#'      bounds_indiv  = rep(1 / K, K))

proj <- function(preds_indiv,   pred_total, 
                 weights_indiv, weight_total ) {

  z    <- sum(preds_indiv) - pred_total
  suma <- sum(1 / weights_indiv) + 1 / weight_total
  s    <- z / suma / weights_indiv
  
  return(list(preds = preds_indiv - s))
  }
