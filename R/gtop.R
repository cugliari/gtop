#' Reconciliate individual predictions using GTOP 
#'
#' Uses a Game Theory approach to reconciliate hierarchical 
#' time series predicitons 
#'   
#' In hierarchical time series forecasts, one predicts individuals
#' quantities and a global quantity. There exists a contraint that
#' matches the sum of the individual quantities to the global quantity.
#' However, forecasting models don't take into account this constraint.
#' 
#' With GTOP you can reconciliate bla bla bla
#'   
#' @param preds_indiv: vector contains the individual predictions
#' @param pred_total: prediction for the sum of individuals
#' @param weights_indiv: vector, contains the weights of the individuals
#' @param weights_total: weight of the total
#' @param bounds_indiv : vector, contains the bounds of the individuals
#' @param optim : logical, should use the optimized code for large 
#'                number of individuals ? (default = TRUE)
#' @return A list with the reconciliated predictions for the 
#'         individuals and the total, and the solution of the 
#'         minimisation problem.
#' @examples 
#' K <- 5
#' indiv <- rep(0, K)
#' total <- 1
#' gtop(preds_indiv = indiv, total, 
#'      weights_indiv = rep(1, K), weights_total = 2,
#'      bounds_indiv  = rep(1 / K, K))


gtop <- function(preds_indiv,   pred_total,
                 weights_indiv, weight_total, 
                 bounds_indiv, optim = TRUE) {
  preds <- 0
  solution <- 0
  return(list(preds_indiv = preds, solution = v))
} 

