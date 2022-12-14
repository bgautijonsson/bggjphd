% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smooth_distributions_spatial.R
\name{pi_etahat_cond_eta_theta_spatial}
\alias{pi_etahat_cond_eta_theta_spatial}
\title{Log density of y given x and theta}
\usage{
pi_etahat_cond_eta_theta_spatial(eta)
}
\arguments{
\item{y}{The current samples of y}

\item{Z}{The model matrix as in y = Z * x + e}

\item{x}{The current samples of the latent parameters x}

\item{chol_Q_e}{Block diagonal matrix of Hessian matrices from the Max step}
}
\value{
The log density of y conditioned on x and theta
}
\description{
Log density of y given x and theta
}
