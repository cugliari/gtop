\name{proj}
\alias{proj}
\title{Prediction conciliation by projection.}
\usage{
proj(preds_indiv, pred_total, weights_indiv, weight_total)
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
K <- 5
proj(preds_indiv = rep(0, K), 1,
     weights_indiv = rep(1, K),
     weights_total = 2,
     bounds_indiv  = rep(1 / K, K))
}

