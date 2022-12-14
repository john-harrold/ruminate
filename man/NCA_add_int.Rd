% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NCA_Server.R
\name{NCA_add_int}
\alias{NCA_add_int}
\title{Adds Analysis Interval to Current Analysis}
\usage{
NCA_add_int(state, interval_start, interval_stop, nca_parameters)
}
\arguments{
\item{state}{NCA state from \code{NCA_fetch_state()}}

\item{interval_start}{Interval start time (numeric).}

\item{interval_stop}{Interval stop time (numeric).}

\item{nca_parameters}{list of NCA parameters in the interval}
}
\value{
State with interval added to the current analysis.
}
\description{
Takes the start time, stop time, and NCA parameters and adds
them to the intervals table
}
