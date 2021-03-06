% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spattemp.R
\name{spattemp}
\alias{spattemp}
\title{Calculates index scores for Spatiotemporal Memory (so-called Camera) games.}
\usage{
spattemp(data, ...)
}
\arguments{
\item{data}{Raw data of class \code{data.frame}.}

\item{...}{Other input argument for future expansion.}
}
\value{
A \link[tibble:tibble-package]{tibble} contains following values:
\item{pc_order}{Percent of correct in temporal order judgment.}
\item{pc_loc}{Percent of correct (distance equals to 0) in location
judgment.}
\item{mean_dist}{Mean distance in location judgment.}
\item{is_normal}{Checking result whether the data is normal.}
}
\description{
Mean distance and percentage of correct responses are returned. The only
difference with \code{\link{locmem}} is that this function takes order
accuracy (thus temporal) into consideration.
}
