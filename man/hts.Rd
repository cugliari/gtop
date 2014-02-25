\name{hts}
\alias{hts}
\title{Prediction conciliation by ...n.}
\usage{
hts(preds_indiv, pred_total)
}
\arguments{
  \item{preds_indiv}{: K-length vector with predictions
  ybar_1,...,ybar_K for individual regions}

  \item{pred_total}{: number with prediction ybar_* for the
  total consumption}
}
\value{
A vector with the reconciliated predictions for the
individuals and the total.
}
\description{
Uses a simple L2 projection to reconciliate hierarchical
time series forecasts.
}
\examples{
K <- 5
hts(preds_indiv = rep(0, K), 1)
}
\references{
Hydman et al. (xxx)
}

