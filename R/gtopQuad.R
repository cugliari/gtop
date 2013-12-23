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
#' @return A list with the reconciliated predictions for the 
#'         individuals and the total, and the solution of the 
#'         minimisation problem.
#' @examples 
#' K <- 5
#' gtopQuad(preds_indiv = rep(0, K), 1, 
#'          weights_indiv = rep(1, K), 
#'          weights_total = 2,
#'          bounds_indiv  = rep(1 / K, K))

gtopQuad <- function(preds_indiv,   pred_total,
                     weights_indiv, weight_total, 
                     bounds_indiv) {
  
  if(any(weights_indiv <= 0)) stop("weights_indiv must all be positive")
  
  K <- length(preds_indiv)
  z <- pred_total - sum(preds_indiv)
  y <- c(preds_indiv, pred_total)
  
  A2 <- diag(c(weights_indiv, weight_total)) # gtop's A^2 matrix & 
  # solve.QP's Dmat matrix
  Rinv <- diag(1 / sqrt(c(weights_indiv, weight_total))) # A2 = R^T %*% R 

  
  # solve.QP's Dmat matrix
  
  Amat <- matrix(0, K + 1, 2 * K + 1) # solve.QP's A matrix (constraints matrix)
  Amat[, 1] <- c(rep(1, K), -1)                    # sum of indivs == total
  Amat[cbind(1:K, 1 + seq(1, 2 * K, by = 2))] <-  1    # lower boundaries 
  Amat[cbind(1:K, 1 + seq(2, 2 * K, by = 2))] <- -1    # upper boundaries
  
  # bvec : solve.QP's independent coefficients on the contraint
  # dvec : linear part of the objective function  
  bvec <- c(0, t(Amat[, -1]) %*% y - rep(bounds_indiv, each = 2))
  dvec <- A2 %*% y
  
  res <- solve.QP(Rinv, dvec, Amat, bvec, meq = 1, factorized = TRUE) 
  
  v <- - (t(res$solution - y) %*% A2 %*% (res$solution - y))
  
  return(list(preds_indiv = res$solution, solution = v))
} 

