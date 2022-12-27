% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NCA_Server.R
\name{NCA_fetch_current_obj}
\alias{NCA_fetch_current_obj}
\title{Fetches the Current Analysis Object}
\usage{
NCA_fetch_current_obj(state, obj_type)
}
\arguments{
\item{state}{NCA state from \code{NCA_fetch_state()}}

\item{obj_type}{Type of object to return (either "table" or "figure").}
}
\value{
list containing the following elements
\itemize{
}
}
\description{
Takes the current state and object type and returns the
currently selected object. For example if you have specified figure, it will
look at the output figure selected and the figure number of that figure and
return the ggplot object for that.
by subject id highlighting of certain NCA aspects (e.g. points used for half-life)
}