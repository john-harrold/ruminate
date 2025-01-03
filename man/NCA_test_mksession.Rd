% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NCA_Server.R
\name{NCA_test_mksession}
\alias{NCA_test_mksession}
\title{Populate Session Data for Module Testing}
\usage{
NCA_test_mksession(session = list())
}
\arguments{
\item{session}{Shiny session variable (in app) or a list (outside of app)}
}
\value{
The NCA portion of the \code{all_sess_res} returned from \code{\link{FM_app_preload}}
}
\description{
Populates the supplied session variable for testing.
}
\examples{
session = shiny::MockShinySession$new()
sess_res = NCA_test_mksession(session=session)
}
