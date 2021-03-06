% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bps.R
\name{bps}
\alias{bps}
\title{Calculates index scores for Behavioral Pattern Separation (BPS) game}
\usage{
bps(data, ...)
}
\arguments{
\item{data}{Raw data of class \code{data.frame}.}

\item{...}{Other input argument for future expansion.}
}
\value{
A \link[tibble:tibble-package]{tibble} contains following values:
\item{pc}{Percent of correct responses.}
\item{p_sim_lure}{Percent of similar responses for "lure" stimuli.}
\item{p_sim_foil}{Percent of similar responses for "foil" stimuli.}
\item{p_sim_old}{Percent of similar responses for "target" (i.e., "old") stimuli.}
\item{bps_score}{BPS score.}
\item{is_normal}{Checking result whether the data is normal.}
}
\description{
The index was developed by Stark et. al. (2013), named as "BPS score".
}
