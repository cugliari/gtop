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
#' gtopLasso(preds_indiv = rep(0, K), 1, 
#'      weights_indiv = rep(1, K), 
#'      weights_total = 2,
#'      bounds_indiv  = rep(1 / K, K))


gtopLasso <- function(preds_indiv,   pred_total,
                      weights_indiv, weight_total, 
                      bounds_indiv, optim = TRUE) {
 
  if(any(weights_indiv <= 0)) stop("weights_indiv must all be positive")
  if(any(bounds_indiv  <= 0)) stop("bounds_indiv must all be positive")
  
  K <- length(preds_indiv)
  z <- pred_total - sum(preds_indiv)
  
  A <- rbind(diag(sqrt(weights_indiv)), rep(sqrt(weight_total), K))
  
  U <- matrix(rep(bounds_indiv * weight_total, K), nrow = K, byrow=FALSE) + 
    diag(bounds_indiv * weights_indiv)
  
  v <- weight_total * z * bounds_indiv
  
  Uinv <- diag(1 / (weights_indiv * bounds_indiv)) - 
    1 / (1 / weight_total + sum(1 / weights_indiv)) *
    (1 / weights_indiv) %*% t(1 / (weights_indiv * bounds_indiv))
  
  D    <-   A %*% Uinv
  b    <- - D %*% v
  
  # Lasso shooting divides the least square part by two which
  #  is equivalent to multipliying lambda by two so we had to
  #  divide lambda by two in order to compensate
  t <- lassoshooting(x = D, y = b, lambda = 1, thr = 1e-8)$coefficients
  s <- Uinv %*% (t + v)
  
  W <- sum( (D %*% t - b)^2 ) + 2 * sum(abs(t))
  V <- W - weight_total * z^2
  
  return(list(preds_indiv = preds_indiv + s, solution = V))
}
