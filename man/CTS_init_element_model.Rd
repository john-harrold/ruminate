% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CTS_Server.R
\name{CTS_init_element_model}
\alias{CTS_init_element_model}
\title{Initializes Cohort When Model Changes}
\usage{
CTS_init_element_model(state, element)
}
\arguments{
\item{state}{CTS state from \code{CTS_fetch_state()}}

\item{element}{Element list from \code{CTS_fetch_current_element()}}
}
\value{
CTS state object with the current cohort ui elements initialized
based on the current model selected
}
\description{
When a source model changes this will update information about
that model like the default dvcols and selection information about the
dvcols
}
