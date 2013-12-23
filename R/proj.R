# Prediction conciliation by projection.

# preds_indiv : K-length vector with predictions ybar_1,...,ybar_K for individual regions
# pred_total  : number with prediction ybar_* for the total consumption

# weights_indiv : K-length vector with weights a_1,...,a_K for individual regions
# weight_total  : number with weight a_* for the total consumption

projectPredictions <- function(preds_indiv,   pred_total, 
                               weights_indiv, weight_total ) {

  z    <- sum(preds_indiv) - pred_total
  suma <- sum(1 / weights_indiv) + 1 / weight_total
  s    <- z / suma / weights_indiv
  
  return(list(preds_proj = preds_indiv - s))
  }


#preds      <- rep(0, 9)
#pred_total <- 1
#weights    <- rep(1, 9)
#weight_total <- 1 / 9

#res <- projectPredictions(preds_indiv = preds, pred_total = pred_total,
#                          weights_indiv = weights, 
#                          weight_total  = weight_total)
