\name{projectPredictions}
\alias{projectPredictions}
\title{Prediction conciliation by projection.}
\usage{
projectPredictions(preds_indiv, pred_total, weights_indiv, weight_total)
}
\arguments{
  \item{preds_indiv}{: K-length vector with predictions
  ybar_1,...,ybar_K for individual regions}

  \item{pred_total}{: number with prediction ybar_* for the
  total consumption}

  \item{weights_indiv}{: K-length vector with weights
  a_1,...,a_K for individual regions}

  \item{weight_total}{: number with weight a_* for the
  total consumption}
}
\description{
Uses a simple L2 projection to reconciliate hierarchical
time series forecasts.
}
\details{
Individual and global forecasts are reconciliated by
projecting the quantities over a ...
}
\examples{
preds <- rep(0, 9)
pred_total <- 1
weights    <- rep(1, 9)
weight_total <- 1 / 9
proj(preds_indiv = preds, pred_total = pred_total,
     weights_indiv = weights,
     weight_total  = weight_total)
}

