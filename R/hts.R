#' Prediction conciliation by ...n.
#' 
#' Uses a simple L2 projection to reconciliate hierarchical time
#' series forecasts.
#' 
#' @param preds_indiv : K-length vector with predictions ybar_1,...,ybar_K for individual regions
#' @param pred_total  : number with prediction ybar_* for the total consumption
#' @return A vector with the reconciliated predictions for the 
#'         individuals and the total.
#' @references Hydman et al. (xxx) 
#' @examples
#' K <- 5
#' hts(preds_indiv = rep(0, K), 1)

hts <- function(preds_indiv, pred_total) {
  K <- length(preds_indiv)
  fcasts <- matrix(c(pred_total, preds_indiv), ncol = K + 1)
  S      <- rbind(rep(1, K), diag(K))
  ycombined <- combinef(fcasts, S, return = 'bottomlevelonly')
  return(c(ycombined[, 1:K], sum(ycombined[, 1:K])))
}


