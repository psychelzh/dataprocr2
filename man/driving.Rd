% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/driving.R
\name{driving}
\alias{driving}
\title{Calculates index scores for Driving game.}
\usage{
driving(data, ...)
}
\arguments{
\item{data}{Raw data of class \code{data.frame}.}

\item{...}{Other input argument for future expansion.}
}
\value{
A \link[tibble:tibble-package]{tibble} contains following values:
\item{still_ratio}{The ratio of still duration in yellow light state.}
\item{is_normal}{Checking result whether the data is normal.}
}
\description{
Caculcate the ratio of still duration in yellow light state.
}
