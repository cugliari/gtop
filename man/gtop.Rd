\name{gtop}
\alias{gtop}
\title{Reconciliate individual predictions using GTOP}
\usage{
gtop(preds_indiv, pred_total, weights_indiv, weight_total, bounds_indiv,
  solver = "quad")
}
\arguments{
  \item{preds_indiv:}{vector contains the individual
  predictions}

  \item{pred_total:}{prediction for the sum of individuals}

  \item{weights_indiv:}{vector, contains the weights of the
  individuals}

  \item{weight_total:}{weight of the total}

  \item{bounds_indiv}{: vector, contains the bounds of the
  individuals}

  \item{solver}{: string, use quadratic programming
  (\code{quad}) or Lasso-like solvers (\code{lasso})}
}
\value{
A list with \item{pred_indivs} the reconciliated
predictions for the individuals and the total,
\item{solution} the solution to the associate minimisation
problem.
}
\description{
Uses a Game Theory approach to reconciliate hierarchical
time series predicitons
}
\details{
In hierarchical time series forecasts, one predicts
individuals quantities and a global quantity. There exists
a contraint that matches the sum of the individual
quantities to the global quantity. However, forecasting
models don't take into account this constraint. With GTOP
you can reconciliate the individual and global quantities
in order to match the aggregate consistency contraint.
}
\examples{
K <- 5
indiv <- rep(0, K)
total <- 1
gtop(preds_indiv   = indiv,
     pred_total = total,
     weights_indiv = rep(1, K),
     weight_total = 2,
     bounds_indiv  = rep(1 / K, K))
}

