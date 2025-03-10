% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CTS_Server.R
\name{CTS_Server}
\alias{CTS_Server}
\title{Clinical Trial Simulator State Server}
\usage{
CTS_Server(
  id,
  FM_yaml_file = system.file(package = "formods", "templates", "formods.yaml"),
  MOD_yaml_file = system.file(package = "ruminate", "templates", "CTS.yaml"),
  deployed = FALSE,
  react_state = NULL
)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the modules UI elements}

\item{FM_yaml_file}{App configuration file with FM as main section.}

\item{MOD_yaml_file}{Module configuration file with MC as main section.}

\item{deployed}{Boolean variable indicating whether the app is deployed or not.}

\item{react_state}{Variable passed to server to allow reaction outside of module (\code{NULL})}
}
\value{
UD Server object
}
\description{
Server function for the Clinical Trial Simulator  Shiny Module
}
