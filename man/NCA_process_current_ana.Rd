% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NCA_Server.R
\name{NCA_process_current_ana}
\alias{NCA_process_current_ana}
\title{Processes Current Analysis to be Run}
\usage{
NCA_process_current_ana(state)
}
\arguments{
\item{state}{NCA state from \code{NCA_fetch_state()}}
}
\value{
Current analysis list with isgood and msgs set
}
\description{
Takes the current analysis and checks different aspects to for
any issues to make sure it's good to go.
}
